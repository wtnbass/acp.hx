;; acp.hx - Agent Client Protocol client for helix (steel)
;;
;; PoC: spawns an ACP agent (claude-agent-acp by default), talks JSON-RPC over
;; stdio from a native reader thread, and renders the chat in a right sidebar.

(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "helix/static.scm")
(require "helix/ext.scm")
(require (prefix-in helix. "helix/commands.scm"))

(provide acp-open
         acp-close
         acp-toggle
         acp-focus
         acp-cancel
         acp-follow-toggle
         acp-configure!)

;;; ---------------------------------------------------------------------------
;;; config

(define *acp-command* "npx -y @agentclientprotocol/claude-agent-acp")
(define *acp-width* 60)
(define *acp-log* "/tmp/acp-hx.log")

;;@doc
;; Configure the agent command, the sidebar width, the agent stderr log path,
;; and whether the editor follows the agent (#:follow 'on / 'off).
(define (acp-configure! #:command [command #f] #:width [width #f] #:log [log #f] #:follow [follow #f])
  (when follow (set! *acp-follow?* (equal? follow 'on)))
  (when command (set! *acp-command* command))
  (when width (set! *acp-width* width))
  (when log (set! *acp-log* log)))

;;; ---------------------------------------------------------------------------
;;; state (only touched on the main thread)

(define *acp-proc* #f)
(define *acp-stdin* #f)
(define *acp-session-id* #f)
(define *acp-cwd* #f)
(define *acp-follow?* #t)
(define *acp-status* "stopped")

(define *acp-next-id* 0)
(define *acp-handlers* (hash)) ; request id -> (lambda (msg) ...)

;; transcript entries, newest first; each is (list kind text-box)
;; kind: 'user 'agent 'thought 'tool 'plan 'info 'error
(define *acp-entries* '())
(define *acp-tools* (hash)) ; toolCallId -> (list text-box title status)
(define *acp-plan-box* #f)

(define *acp-input* "")
(define *acp-scroll* 0) ; lines scrolled up from the bottom
(define *acp-permission* #f) ; (list request-id title options) while pending

(define *acp-open?* #f)
(define *acp-focused?* #f)

(define *acp-lines-cache* #f) ; (cons width lines)

(define (acp-dirty!)
  (set! *acp-lines-cache* #f))

(define (acp-push-entry! kind text)
  (define b (box text))
  (set! *acp-entries* (cons (list kind b) *acp-entries*))
  (acp-dirty!)
  b)

;; streaming chunks extend the newest entry when it has the same kind
(define (acp-append-chunk! kind text)
  (if (and (pair? *acp-entries*) (equal? (car (car *acp-entries*)) kind))
      (let ([b (cadr (car *acp-entries*))])
        (set-box! b (string-append (unbox b) text))
        (acp-dirty!))
      (acp-push-entry! kind text)))

;;; ---------------------------------------------------------------------------
;;; JSON-RPC transport

(define (acp-write! msg)
  (when *acp-stdin*
    (with-handler (lambda (err) (acp-push-entry! 'error (to-string "write failed: " err)))
                  ;; write-line! would print the string with quotes
                  (write-string (string-append (value->jsexpr-string msg) "\n") *acp-stdin*)
                  (flush-output-port *acp-stdin*))))

(define (acp-request! method params on-result)
  (set! *acp-next-id* (+ *acp-next-id* 1))
  ;; string ids: string->jsexpr turns every JSON number into a float, so an int id
  ;; would never match its response
  (define id (string-append "acp-" (number->string *acp-next-id*)))
  (set! *acp-handlers* (hash-insert *acp-handlers* id on-result))
  (acp-write! (hash "jsonrpc" "2.0" "id" id "method" method "params" params)))

(define (acp-notify! method params)
  (acp-write! (hash "jsonrpc" "2.0" "method" method "params" params)))

(define (acp-respond! id result)
  (acp-write! (hash "jsonrpc" "2.0" "id" id "result" result)))

(define (acp-respond-error! id code message)
  (acp-write! (hash "jsonrpc" "2.0" "id" id "error" (hash "code" code "message" message))))

;; runs on a native thread; every message hops to the main thread
(define (acp-reader-loop port)
  (let loop ()
    (define line (with-handler (lambda (_) (eof-object)) (read-line-from-port port)))
    (if (eof-object? line)
        (hx.with-context acp-on-exit)
        (begin
          (define msg (with-handler (lambda (_) #f) (string->jsexpr line)))
          (when (hash? msg)
            (hx.with-context (lambda () (acp-dispatch msg))))
          (loop)))))

(define (get h . keys)
  (let loop ([v h] [ks keys])
    (cond
      [(null? ks) v]
      [(hash? v) (loop (hash-try-get v (car ks)) (cdr ks))]
      [else #f])))

(define (acp-dispatch msg)
  (with-handler
   (lambda (err) (acp-push-entry! 'error (to-string "dispatch: " err)))
   (define method (get msg 'method))
   (define id (get msg 'id))
   (cond
     [(and method id) (acp-handle-request id method (get msg 'params))]
     [method (acp-handle-notification method (get msg 'params))]
     [id
      (define handler (hash-try-get *acp-handlers* id))
      (set! *acp-handlers* (hash-remove *acp-handlers* id))
      (define err (get msg 'error))
      (cond
        [(hash? err) (acp-push-entry! 'error (to-string "error: " (get err 'message)))
                     (set! *acp-status* "ready")]
        [handler (handler (get msg 'result))])]))
  (acp-redraw!))

(define (acp-on-exit)
  (set! *acp-proc* #f)
  (set! *acp-stdin* #f)
  (set! *acp-session-id* #f)
  (set! *acp-status* "stopped")
  (acp-push-entry! 'info (string-append "agent exited (log: " *acp-log* ")"))
  (acp-redraw!))

;;; ---------------------------------------------------------------------------
;;; ACP

(define (acp-start!)
  (unless *acp-proc*
    (set! *acp-status* "starting")
    (define cwd (helix-find-workspace))
    (set! *acp-cwd* cwd)
    (define result
      (~> (command "sh" (list "-c" (string-append "exec " *acp-command* " 2>>" *acp-log*)))
          (with-current-dir cwd)
          (with-stdin-piped)
          (with-stdout-piped)
          (spawn-process)))
    (if (Ok? result)
        (let* ([proc (Ok->value result)]
               [stdout (child-stdout proc)])
          (set! *acp-proc* proc)
          (set! *acp-stdin* (child-stdin proc))
          (spawn-native-thread (lambda () (acp-reader-loop stdout)))
          (acp-initialize! cwd))
        (begin
          (set! *acp-status* "stopped")
          (acp-push-entry! 'error (to-string "spawn failed: " (Err->value result)))))))

(define (acp-initialize! cwd)
  (acp-request!
   "initialize"
   (hash "protocolVersion" 1
         "clientCapabilities" (hash "fs" (hash "readTextFile" #f "writeTextFile" #f)
                                    "terminal" #f)
         "clientInfo" (hash "name" "acp.hx" "version" "0.1.0"))
   (lambda (_)
     (acp-request! "session/new"
                   (hash "cwd" cwd "mcpServers" '())
                   (lambda (result)
                     (set! *acp-session-id* (get result 'sessionId))
                     (set! *acp-status* "ready")
                     (acp-push-entry! 'info (string-append "session started in " cwd)))))))

(define (acp-send-prompt! text)
  (cond
    [(not *acp-session-id*) (set-status! "acp: session is not ready yet")]
    [else
     (acp-push-entry! 'user text)
     (set! *acp-status* "thinking")
     (set! *acp-scroll* 0)
     (acp-request! "session/prompt"
                   (hash "sessionId" *acp-session-id*
                         "prompt" (list (hash "type" "text" "text" text)))
                   (lambda (result)
                     (define reason (get result 'stopReason))
                     (unless (equal? reason "end_turn")
                       (acp-push-entry! 'info (to-string "stopped: " reason)))
                     (set! *acp-status* "ready")))]))

;;@doc
;; Cancel the running prompt turn.
(define (acp-cancel)
  (when *acp-session-id*
    (acp-notify! "session/cancel" (hash "sessionId" *acp-session-id*))))

(define (acp-handle-notification method params)
  (when (equal? method "session/update")
    (acp-handle-update (get params 'update))))

(define (acp-handle-update u)
  (define kind (get u 'sessionUpdate))
  (cond
    [(equal? kind "agent_message_chunk")
     (define text (get u 'content 'text))
     (when (string? text) (acp-append-chunk! 'agent text))]
    [(equal? kind "agent_thought_chunk")
     (define text (get u 'content 'text))
     (when (string? text) (acp-append-chunk! 'thought text))]
    [(equal? kind "tool_call")
     (define id (get u 'toolCallId))
     (define title (or (get u 'title) "tool"))
     (define status (or (get u 'status) "pending"))
     (define locations (acp-locations u))
     (define b (acp-push-entry! 'tool (acp-tool-text title status)))
     (set! *acp-tools* (hash-insert *acp-tools* id (list b title status locations)))
     (acp-follow! locations)]
    [(equal? kind "tool_call_update")
     (define tool (hash-try-get *acp-tools* (get u 'toolCallId)))
     (when tool
       (define title (or (get u 'title) (list-ref tool 1)))
       (define status (or (get u 'status) (list-ref tool 2)))
       (define new-locations (acp-locations u))
       (define locations (if (pair? new-locations) new-locations (list-ref tool 3)))
       (set-box! (list-ref tool 0) (acp-tool-text title status))
       (set! *acp-tools*
             (hash-insert *acp-tools* (get u 'toolCallId) (list (list-ref tool 0) title status locations)))
       ;; an edit tool has touched the file on disk by the time it completes
       (when (equal? status "completed")
         (for-each acp-reload-clean-doc! locations))
       (when (pair? new-locations) (acp-follow! new-locations))
       (acp-dirty!))]
    [(equal? kind "plan")
     (define text (acp-plan-text (or (get u 'entries) '())))
     (if *acp-plan-box*
         (begin (set-box! *acp-plan-box* text) (acp-dirty!))
         (set! *acp-plan-box* (acp-push-entry! 'plan text)))]
    [else void]))

;;; ---------------------------------------------------------------------------
;;; follow-along

;; tool_call.locations -> list of (cons path line-or-#f), limited to the workspace
;; so memory files and other outside paths are not opened
(define (acp-locations u)
  (define locs (get u 'locations))
  (if (list? locs)
      (filter (lambda (l) l)
              (map (lambda (loc)
                     (define path (get loc 'path))
                     (define line (get loc 'line))
                     (and (string? path)
                          *acp-cwd*
                          (starts-with? path *acp-cwd*)
                          (cons path (and (number? line) (max 1 (inexact->exact (round line)))))))
                   locs))
      '()))

(define (acp-find-doc path)
  (let loop ([ids (editor-all-documents)])
    (cond
      [(null? ids) #f]
      [(equal? (editor-document->path (car ids)) path) (car ids)]
      [else (loop (cdr ids))])))

;; leaves unsaved buffers alone
(define (acp-reload-clean-doc! loc)
  (define doc (acp-find-doc (car loc)))
  (when (and doc (not (editor-document-dirty? doc)))
    (editor-document-reload doc)))

(define (acp-follow! locations)
  (when (and *acp-follow?* (pair? locations))
    (define loc (car locations))
    (with-handler
     (lambda (err) (log::warn! (to-string "acp follow: " err)))
     (acp-reload-clean-doc! loc)
     (helix.open (car loc))
     (when (cdr loc)
       (helix.goto (number->string (cdr loc)))
       (align_view_center)))))

;;@doc
;; Toggle whether the editor follows the files the agent reads and edits.
(define (acp-follow-toggle)
  (set! *acp-follow?* (not *acp-follow?*))
  (set-status! (if *acp-follow?* "acp: follow on" "acp: follow off")))

(define (acp-tool-text title status)
  (define mark
    (cond [(equal? status "completed") "✓"]
          [(equal? status "failed") "✗"]
          [(equal? status "in_progress") "…"]
          [else "·"]))
  (string-append mark " " title))

(define (acp-plan-text entries)
  (string-join
   (map (lambda (e)
          (define st (get e 'status))
          (string-append (cond [(equal? st "completed") "[x] "]
                               [(equal? st "in_progress") "[>] "]
                               [else "[ ] "])
                         (or (get e 'content) "")))
        entries)
   "\n"))

(define (acp-handle-request id method params)
  (cond
    [(equal? method "session/request_permission")
     (set! *acp-permission*
           (list id (or (get params 'toolCall 'title) "tool") (or (get params 'options) '())))
     ;; the answer is typed in the panel, so pull focus there
     (acp-focus)]
    [else (acp-respond-error! id -32601 (string-append "method not found: " method))]))

(define (acp-answer-permission! option)
  (define id (car *acp-permission*))
  (set! *acp-permission* #f)
  (acp-respond! id
                (hash "outcome"
                      (if option
                          (hash "outcome" "selected" "optionId" (get option 'optionId))
                          (hash "outcome" "cancelled")))))

;;; ---------------------------------------------------------------------------
;;; text layout

(define (char-width ch)
  (define c (char->integer ch))
  (if (or (and (>= c #x1100) (<= c #x115F))
          (and (>= c #x2E80) (<= c #xA4CF))
          (and (>= c #xAC00) (<= c #xD7A3))
          (and (>= c #xF900) (<= c #xFAFF))
          (and (>= c #xFE30) (<= c #xFE4F))
          (and (>= c #xFF00) (<= c #xFF60))
          (and (>= c #xFFE0) (<= c #xFFE6))
          (and (>= c #x1F300) (<= c #x1FAFF))
          (>= c #x20000))
      2
      1))

(define (string-width s)
  (foldl (lambda (ch acc) (+ acc (char-width ch))) 0 (string->list s)))

;; greedy wrap by display width; returns a list of strings
(define (wrap-line s width)
  (let loop ([chars (string->list s)] [cur '()] [cur-w 0] [out '()])
    (cond
      [(null? chars) (reverse (cons (list->string (reverse cur)) out))]
      [else
       (define w (char-width (car chars)))
       (if (and (> (+ cur-w w) width) (pair? cur))
           (loop chars '() 0 (cons (list->string (reverse cur)) out))
           (loop (cdr chars) (cons (car chars) cur) (+ cur-w w) out))])))

(define (wrap-text s width)
  (apply append (map (lambda (l) (wrap-line l width)) (split-many s "\n"))))

(define (entry-prefix kind)
  (cond [(equal? kind 'user) "> "]
        [(equal? kind 'thought) "~ "]
        [(equal? kind 'error) "! "]
        [(equal? kind 'info) "# "]
        [else ""]))

;; oldest first: list of (cons kind line)
(define (acp-layout width)
  (if (and *acp-lines-cache* (= (car *acp-lines-cache*) width))
      (cdr *acp-lines-cache*)
      (let ([lines
             (apply append
                    (map (lambda (e)
                           (define kind (car e))
                           (define text (string-append (entry-prefix kind) (unbox (cadr e))))
                           (append (map (lambda (l) (cons kind l)) (wrap-text text width))
                                   (list (cons 'blank ""))))
                         (reverse *acp-entries*)))])
        (set! *acp-lines-cache* (cons width lines))
        lines)))

(define (take-last lst n)
  (define len (length lst))
  (if (<= len n) lst (list-tail lst (- len n))))

(define (drop-last lst n)
  (define len (length lst))
  (if (<= len n) '() (take lst (- len n))))

;;; ---------------------------------------------------------------------------
;;; rendering

(define (acp-redraw!)
  (when *acp-open?* (helix.redraw)))

(define (acp-panel-width rect)
  (max 20 (min *acp-width* (- (area-width rect) 20))))

(define (kind-style kind)
  (define scope
    (cond [(equal? kind 'user) "keyword"]
          [(equal? kind 'thought) "comment"]
          [(equal? kind 'tool) "function"]
          [(equal? kind 'plan) "string"]
          [(equal? kind 'error) "error"]
          [(equal? kind 'info) "comment"]
          [else "ui.text"]))
  (theme-scope-ref scope))

(define (acp-input-display width)
  (define s (string-replace *acp-input* "\n" "⏎"))
  ;; keep the tail visible
  (let loop ([chars (reverse (string->list s))] [acc '()] [w 0])
    (cond
      [(null? chars) (list->string acc)]
      [(> (+ w (char-width (car chars))) width) (list->string acc)]
      [else (loop (cdr chars) (cons (car chars) acc) (+ w (char-width (car chars))))])))

(define (acp-render state rect frame)
  (define w (acp-panel-width rect))
  (define x0 (- (area-width rect) w))
  (define y0 1) ; below the bufferline
  (define h (- (area-height rect) y0 1)) ; leave the command line row
  (set-editor-clip-right! w)

  (define bg (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define border-style (theme-scope-ref "ui.window"))
  (define dim (style-with-dim text-style))
  (buffer/clear-with frame (area x0 y0 w h) bg)

  ;; vertical divider
  (let loop ([y y0])
    (when (< y (+ y0 h))
      (frame-set-string! frame x0 y "│" border-style)
      (loop (+ y 1))))

  (define cx (+ x0 2))
  (define cw (- w 3))

  ;; header
  (define title-style (if *acp-focused?* (style-with-bold (theme-scope-ref "ui.text.focus")) dim))
  (frame-set-string! frame cx y0 (string-append "ACP · " *acp-status* (if *acp-follow?* " · follow" "")) title-style)

  ;; footer: permission prompt + input
  (define perm-lines
    (if *acp-permission*
        (append
         (wrap-text (string-append "? allow: " (cadr *acp-permission*)) cw)
         (let loop ([opts (caddr *acp-permission*)] [i 1] [out '()])
           (if (null? opts)
               (reverse (cons "  [esc] cancel" out))
               (loop (cdr opts) (+ i 1)
                     (cons (string-append "  [" (number->string i) "] " (or (get (car opts) 'name) "?"))
                           out)))))
        '()))
  (define input-y (+ y0 h -1))
  (define perm-y (- input-y (length perm-lines)))
  (define sep-y (- perm-y 1))
  (frame-set-string! frame cx sep-y (make-string cw #\─) border-style)
  (let loop ([ls perm-lines] [y perm-y])
    (when (pair? ls)
      (frame-set-string! frame cx y (car ls) (theme-scope-ref "warning"))
      (loop (cdr ls) (+ y 1))))
  (frame-set-string! frame cx input-y "❯ " (theme-scope-ref "keyword"))
  (frame-set-string! frame (+ cx 2) input-y (acp-input-display (- cw 3)) text-style)

  ;; transcript
  (define body-top (+ y0 2))
  (define body-h (max 0 (- sep-y body-top)))
  (define lines (acp-layout cw))
  (define max-scroll (max 0 (- (length lines) body-h)))
  (when (> *acp-scroll* max-scroll) (set! *acp-scroll* max-scroll))
  (define visible (take-last (drop-last lines *acp-scroll*) body-h))
  (let loop ([ls visible] [y body-top])
    (when (pair? ls)
      (frame-set-string! frame cx y (cdr (car ls)) (kind-style (car (car ls))))
      (loop (cdr ls) (+ y 1)))))

(define (acp-cursor state rect)
  (if (and *acp-focused?* (not *acp-permission*))
      (let* ([w (acp-panel-width rect)]
             [x0 (- (area-width rect) w)]
             [cw (- w 3)]
             [y (- (area-height rect) 2)])
        (position y (+ x0 2 2 (string-width (acp-input-display (- cw 3))))))
      #f))

;;; ---------------------------------------------------------------------------
;;; input

(define (acp-handle-permission-key event)
  (define ch (key-event-char event))
  (define opts (caddr *acp-permission*))
  (cond
    [(key-event-escape? event) (acp-answer-permission! #f)]
    [(and (char? ch) (char-digit? ch))
     (define i (- (char->integer ch) (char->integer #\1)))
     (when (and (>= i 0) (< i (length opts)))
       (acp-answer-permission! (list-ref opts i)))]
    [else void])
  event-result/consume)

(define (ctrl? event)
  (equal? (key-event-modifier event) key-modifier-ctrl))

(define (acp-handle-event state event)
  (define ch (and (key-event? event) (key-event-char event)))
  (cond
    [(paste-event? event)
     (set! *acp-input* (string-append *acp-input* (paste-event-string event)))
     event-result/consume]
    [(not (key-event? event)) event-result/ignore]
    [*acp-permission* (acp-handle-permission-key event)]
    [(key-event-escape? event) (acp-unfocus!) event-result/consume]
    [(and (ctrl? event) (equal? ch #\c)) (acp-cancel) event-result/consume]
    [(and (ctrl? event) (equal? ch #\u)) (set! *acp-input* "") event-result/consume]
    [(key-event-enter? event)
     (define text (trim *acp-input*))
     (unless (equal? text "")
       (set! *acp-input* "")
       (acp-send-prompt! text))
     event-result/consume]
    [(key-event-backspace? event)
     (define n (string-length *acp-input*))
     (when (> n 0) (set! *acp-input* (substring *acp-input* 0 (- n 1))))
     event-result/consume]
    [(key-event-page-up? event) (set! *acp-scroll* (+ *acp-scroll* 10)) event-result/consume]
    [(key-event-page-down? event) (set! *acp-scroll* (max 0 (- *acp-scroll* 10))) event-result/consume]
    [(and (char? ch) (not (ctrl? event)))
     (set! *acp-input* (string-append *acp-input* (string ch)))
     event-result/consume]
    [else event-result/consume]))

;;; ---------------------------------------------------------------------------
;;; components & commands

;; the bg component draws the panel and lets every event fall through to the editor;
;; the fg component is pushed on top only while the panel holds focus
(define (acp-make-bg)
  (new-component! "acp-bg" #f acp-render (hash "handle_event" (lambda (s e) event-result/ignore))))

(define (acp-make-fg)
  (new-component! "acp-fg"
                  #f
                  (lambda (s r f) void)
                  (hash "handle_event" acp-handle-event "cursor" acp-cursor)))

(define (acp-unfocus!)
  (when *acp-focused?*
    (set! *acp-focused?* #f)
    (pop-last-component-by-name! "acp-fg")))

;;@doc
;; Open the ACP sidebar (starting the agent if needed) and focus it.
(define (acp-open)
  (unless *acp-open?*
    (set! *acp-open?* #t)
    (push-component! (acp-make-bg)))
  (acp-start!)
  (acp-focus))

;;@doc
;; Focus the ACP sidebar input.
(define (acp-focus)
  (unless *acp-open?*
    (set! *acp-open?* #t)
    (push-component! (acp-make-bg)))
  (unless *acp-focused?*
    (set! *acp-focused?* #t)
    (push-component! (acp-make-fg))))

;;@doc
;; Hide the ACP sidebar. The agent keeps running.
(define (acp-close)
  (acp-unfocus!)
  (when *acp-open?*
    (set! *acp-open?* #f)
    (pop-last-component-by-name! "acp-bg")
    (set-editor-clip-right! 0)))

;;@doc
;; Toggle the ACP sidebar: open -> focus -> close.
(define (acp-toggle)
  (cond
    [(not *acp-open?*) (acp-open)]
    [(not *acp-focused?*) (acp-focus)]
    [else (acp-close)]))
