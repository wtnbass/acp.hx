;; acp.hx - Agent Client Protocol client for helix (steel)
;;
;; Spawns an ACP agent (claude-agent-acp by default), talks JSON-RPC over stdio
;; from a native reader thread, and renders the conversation in a right sidebar.

(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require "helix/static.scm")
(require "helix/ext.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require-builtin helix/core/text as text.)
(require-builtin steel/time)

(provide acp-open
         acp-close
         acp-toggle
         acp-focus
         acp-quit
         acp-restart
         acp-cancel
         acp-new-session
         acp-sessions
         acp-settings
         acp-mode
         acp-model
         acp-effort
         acp-cycle-mode
         acp-add-file
         acp-add-selection
         acp-add-image
         acp-follow-toggle
         acp-expand-toggle
         acp-diff
         acp-review
         acp-undo-edit
         acp-menu
         acp-wider
         acp-yank
         acp-insert-code
         acp-narrower
         acp-switch-agent
         acp-configure!)

;;; ===========================================================================
;;; config

(define *acp-command* "npx -y @agentclientprotocol/claude-agent-acp")
(define *acp-width* 64)
(define *acp-log* "/tmp/acp-hx.log")
(define *acp-follow?* #t)
(define *acp-agents*
  (list (cons "Claude Code" "npx -y @agentclientprotocol/claude-agent-acp")))

;;@doc
;; Configure acp.hx.
;;
;; * #:command - shell command that starts the ACP agent
;; * #:width   - sidebar width in columns
;; * #:log     - file that receives the agent's stderr
;; * #:follow  - 'on / 'off, whether the editor follows the files the agent touches
;; * #:agents  - list of (name . command) offered by :acp-switch-agent; the first
;;               one becomes the default unless #:command is given
(define (acp-configure! #:command [command #f] #:width [width #f] #:log [log #f] #:follow [follow #f]
                        #:agents [agents #f])
  (when (pair? agents)
    (set! *acp-agents* agents)
    (set! *acp-command* (cdr (car agents))))
  (when command (set! *acp-command* command))
  (when width (set! *acp-width* width))
  (when log (set! *acp-log* log))
  (when follow (set! *acp-follow?* (equal? follow 'on))))

;;; ===========================================================================
;;; small utilities

(define (get h . keys)
  (let loop ([v h] [ks keys])
    (cond
      [(null? ks) v]
      [(hash? v) (loop (hash-try-get v (car ks)) (cdr ks))]
      [else #f])))

(define (->int n)
  (if (number? n) (inexact->exact (round n)) 0))

(define (find-first pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) (car lst)]
        [else (find-first pred (cdr lst))]))

(define (index-where pred lst)
  (let loop ([l lst] [i 0])
    (cond [(null? l) #f]
          [(pred (car l)) i]
          [else (loop (cdr l) (+ i 1))])))

;; 0 .. n-1 (helix/static exports its own `range`)
(define (indices n)
  (let loop ([i (- n 1)] [acc '()])
    (if (< i 0) acc (loop (- i 1) (cons i acc)))))

(define (take-at-most lst n)
  (if (or (<= n 0) (null? lst)) '() (cons (car lst) (take-at-most (cdr lst) (- n 1)))))

(define (take-last lst n)
  (define len (length lst))
  (if (<= len n) lst (list-tail lst (- len n))))

(define (string-blank? s)
  (equal? (trim s) ""))

(define (char-width ch)
  (define c (char->integer ch))
  (cond
    [(< c #x1100) 1]
    [(or (<= c #x115F)
         (and (>= c #x2E80) (<= c #xA4CF))
         (and (>= c #xAC00) (<= c #xD7A3))
         (and (>= c #xF900) (<= c #xFAFF))
         (and (>= c #xFE30) (<= c #xFE4F))
         (and (>= c #xFF00) (<= c #xFF60))
         (and (>= c #xFFE0) (<= c #xFFE6))
         (and (>= c #x1F300) (<= c #x1F64F))
         (and (>= c #x1F900) (<= c #x1F9FF))
         (>= c #x20000))
     2]
    [else 1]))

(define (string-width s)
  (foldl (lambda (ch acc) (+ acc (char-width ch))) 0 (string->list s)))

;; cut a string to at most `width` columns, adding an ellipsis when it was cut
(define (truncate-width s width)
  (if (<= (string-width s) width)
      s
      (let loop ([chars (string->list s)] [acc '()] [w 0])
        (define cw (if (null? chars) 0 (char-width (car chars))))
        (if (or (null? chars) (> (+ w cw) (- width 1)))
            (string-append (list->string (reverse acc)) "…")
            (loop (cdr chars) (cons (car chars) acc) (+ w cw))))))

(define (format-tokens n)
  (define n* (->int n))
  (cond [(>= n* 1000000) (string-append (number->string (/ (round (/ n* 100000)) 10.0)) "M")]
        [(>= n* 1000) (string-append (number->string (->int (/ n* 1000))) "k")]
        [else (number->string n*)]))

(define (format-percent ratio)
  (string-append (number->string (->int (* 100 ratio))) "%"))

(define (format-cost amount)
  ;; two decimals without printf
  (define cents (->int (* 100 amount)))
  (define d (quotient cents 100))
  (define c (remainder cents 100))
  (string-append "$" (number->string d) "." (if (< c 10) "0" "") (number->string c)))

(define (relative-path path)
  (if (and *acp-cwd* (starts-with? path (string-append *acp-cwd* "/")))
      (substring path (+ (string-length *acp-cwd*) 1) (string-length path))
      path))

(define (now-ms) (current-milliseconds))

;;; ===========================================================================
;;; state (only touched on the main thread)

(define *acp-proc* #f)
(define *acp-generation* 0) ; bumps per spawn so a dead agent's exit can't reset its successor
(define *acp-stdin* #f)
(define *acp-cwd* #f)
(define *acp-agent-title* "Agent")
(define *acp-session-id* #f)
(define *acp-session-title* #f)
(define *acp-status* 'stopped) ; 'stopped 'starting 'ready
(define *acp-busy* 0)          ; prompt turns in flight
(define *acp-turn-start* 0)

(define *acp-next-id* 0)
(define *acp-handlers* (hash)) ; request id -> (lambda (result) ...)

(define *acp-config-options* '()) ; configOptions from the agent
(define *acp-commands* '())       ; available slash commands, list of hash
(define *acp-usage* #f)           ; last usage_update
(define *acp-rate-limit* #f)      ; _claude/rateLimit meta

;; transcript: newest first
(struct Entry (kind data cache))
;; data: box of hash, cache: box of (list width expand lines)
(define *acp-entries* '())
(define *acp-tools* (hash)) ; toolCallId -> Entry
(define *acp-plan-entry* #f)

(define *acp-expand?* #f)
(define *acp-scroll* 0)

;; input is a zipper of chars: before the cursor (reversed) and after it
(define *acp-before* '())
(define *acp-after* '())
(define *acp-history* '())
(define *acp-history-pos* -1)
(define *acp-attachments* '()) ; list of hash: label + content block
(define *acp-notes* '()) ; things the agent should hear with the next prompt
(define *acp-completion-index* 0)
(define *acp-files* '()) ; workspace files for @ completion

(define *acp-permission* #f) ; hash: id title content options index

(define *acp-open?* #f)
(define *acp-focused?* #f)
(define *acp-cursor-pos* #f)
(define *acp-panel-area* #f)
(define *acp-spinner-frame* 0)
(define *acp-ticking?* #f)

;;; ===========================================================================
;;; transcript entries

(define (make-entry kind data)
  (Entry kind (box data) (box #f)))

(define (entry-get e key) (hash-try-get (unbox (Entry-data e)) key))

(define (entry-set! e key value)
  (set-box! (Entry-data e) (hash-insert (unbox (Entry-data e)) key value))
  (set-box! (Entry-cache e) #f))

(define (acp-push-entry! kind data)
  (define e (make-entry kind data))
  (set! *acp-entries* (cons e *acp-entries*))
  e)

(define (acp-push-text! kind text)
  (acp-push-entry! kind (hash 'text text)))

;; streaming chunks extend the newest entry when it has the same kind
(define (acp-append-chunk! kind text)
  (if (and (pair? *acp-entries*) (equal? (Entry-kind (car *acp-entries*)) kind))
      (let ([e (car *acp-entries*)])
        (entry-set! e 'text (string-append (entry-get e 'text) text)))
      (acp-push-text! kind text)))

(define (acp-info! text) (acp-push-text! 'info text))
(define (acp-error! text) (acp-push-text! 'error text))

(define (acp-reset-transcript!)
  (set! *acp-entries* '())
  (set! *acp-tools* (hash))
  (set! *acp-plan-entry* #f)
  (set! *acp-scroll* 0))

(define (acp-invalidate-all!)
  (for-each (lambda (e) (set-box! (Entry-cache e) #f)) *acp-entries*))

;;; ===========================================================================
;;; JSON-RPC transport

(define (acp-write! msg)
  (when *acp-stdin*
    (with-handler (lambda (err) (acp-error! (to-string "write failed: " err)))
                  ;; write-line! would print the string with quotes
                  (write-string (string-append (value->jsexpr-string msg) "\n") *acp-stdin*)
                  (flush-output-port *acp-stdin*))))

(define (acp-request! method params on-result [on-error #f])
  (set! *acp-next-id* (+ *acp-next-id* 1))
  ;; string ids: string->jsexpr turns every JSON number into a float, so an int id
  ;; would never match its response
  (define id (string-append "acp-" (number->string *acp-next-id*)))
  (set! *acp-handlers* (hash-insert *acp-handlers* id (cons on-result on-error)))
  (acp-write! (hash "jsonrpc" "2.0" "id" id "method" method "params" params)))

(define (acp-notify! method params)
  (acp-write! (hash "jsonrpc" "2.0" "method" method "params" params)))

(define (acp-respond! id result)
  (acp-write! (hash "jsonrpc" "2.0" "id" id "result" result)))

(define (acp-respond-error! id code message)
  (acp-write! (hash "jsonrpc" "2.0" "id" id "error" (hash "code" code "message" message))))

;; runs on a native thread; every message hops to the main thread
(define (acp-reader-loop port generation)
  (let loop ()
    (define line (with-handler (lambda (_) (eof-object)) (read-line-from-port port)))
    (if (eof-object? line)
        (hx.with-context (lambda () (acp-on-exit generation)))
        (begin
          (define msg (with-handler (lambda (_) #f) (string->jsexpr line)))
          (when (hash? msg)
            (hx.with-context (lambda () (acp-dispatch msg))))
          (loop)))))

(define (acp-dispatch msg)
  (with-handler
   (lambda (err) (acp-error! (to-string "acp.hx: " err)))
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
        [(not handler) void]
        [(hash? err)
         (if (cdr handler)
             ((cdr handler) err)
             (acp-error! (to-string (or (get err 'message) "request failed"))))]
        [else ((car handler) (get msg 'result))])]))
  (acp-redraw!))

(define (acp-on-exit generation)
  (when (= generation *acp-generation*)
    (set! *acp-proc* #f)
    (set! *acp-stdin* #f)
    (set! *acp-session-id* #f)
    (set! *acp-status* 'stopped)
    (set! *acp-busy* 0)
    (set! *acp-permission* #f)
    (acp-error! (string-append "agent exited" (acp-log-tail) "\n(log: " *acp-log* ")"))
    (acp-redraw!)))

;; last lines of the agent's stderr, to explain why it died
(define (acp-log-tail)
  (with-handler
   (lambda (_) "")
   (let* ([port (open-input-file *acp-log*)]
          [text (read-port-to-string port)]
          [lines (filter (lambda (l) (not (string-blank? l))) (split-many text "\n"))])
     (if (null? lines) "" (string-append ":\n" (string-join (take-last lines 3) "\n"))))))

;;; ===========================================================================
;;; agent lifecycle and sessions

(define (acp-start!)
  (unless *acp-proc*
    (set! *acp-status* 'starting)
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
          (set! *acp-generation* (+ *acp-generation* 1))
          (let ([generation *acp-generation*])
            (spawn-native-thread (lambda () (acp-reader-loop stdout generation))))
          (acp-load-files! cwd)
          (acp-initialize!))
        (begin
          (set! *acp-status* 'stopped)
          (acp-error! (to-string "spawn failed: " (Err->value result)))))))

;; list workspace files off the main thread, via git so ignored files stay out
(define (acp-load-files! cwd)
  (spawn-native-thread
   (lambda ()
     (define files
       (with-handler
        (lambda (_) '())
        ;; stderr must not reach the terminal helix draws on
        (let ([r (~> (command "sh" (list "-c" (string-append
                                              "git ls-files --cached --others --exclude-standard 2>/dev/null"
                                              " || find . -type f -not -path '*/.*' 2>/dev/null"
                                              " | sed 's|^\\./||' | head -20000")))
                     (with-current-dir cwd)
                     (with-stdout-piped)
                     (spawn-process))])
          (if (Ok? r)
              (filter (lambda (l) (not (equal? l "")))
                      (split-many (read-port-to-string (child-stdout (Ok->value r))) "\n"))
              '()))))
     (hx.with-context (lambda () (set! *acp-files* files))))))

(define (acp-initialize!)
  (acp-request!
   "initialize"
   (hash "protocolVersion" 1
         "clientCapabilities" (hash "fs" (hash "readTextFile" #f "writeTextFile" #f)
                                    "terminal" #f)
         "clientInfo" (hash "name" "acp.hx" "title" "acp.hx" "version" "0.2.0"))
   (lambda (result)
     (define title (get result 'agentInfo 'title))
     (when (string? title) (set! *acp-agent-title* title))
     (acp-new-session))))

(define (acp-apply-session-result! result)
  (define options (get result 'configOptions))
  (when (list? options) (set! *acp-config-options* options))
  (set! *acp-status* 'ready))

;;@doc
;; Start a fresh conversation.
(define (acp-new-session)
  (cond
    [(not *acp-proc*) (acp-open)]
    [else
     (acp-reset-transcript!)
     (set! *acp-session-id* #f)
     (set! *acp-session-title* #f)
     (set! *acp-usage* #f)
     (set! *acp-status* 'starting)
     (acp-request! "session/new"
                   (hash "cwd" *acp-cwd* "mcpServers" '())
                   (lambda (result)
                     (set! *acp-session-id* (get result 'sessionId))
                     (acp-apply-session-result! result))
                   (lambda (err)
                     (set! *acp-status* 'stopped)
                     (acp-error! (to-string "could not start a session: " (or (get err 'message) err)))
                     ;; ACP reserves -32000 for "authentication required"
                     (when (equal? (->int (get err 'code)) -32000)
                       (acp-info! "log in with the agent's own CLI first (for Claude Code: run `claude` and /login)"))))]))

;;@doc
;; Pick a previous conversation of this workspace and resume it.
(define (acp-sessions)
  (if (not *acp-proc*)
      (set-status! "acp: agent is not running")
      (acp-request!
       "session/list"
       (hash "cwd" *acp-cwd*)
       (lambda (result)
         (define sessions (or (get result 'sessions) '()))
         (if (null? sessions)
             (set-status! "acp: no previous sessions")
             (acp-pick! "Resume session"
                        (map (lambda (s)
                               (define updated (or (get s 'updatedAt) ""))
                               (list (or (get s 'title) (get s 'sessionId))
                                     (if (>= (string-length updated) 16)
                                         (string-replace (substring updated 0 16) "T" " ")
                                         updated)
                                     (get s 'sessionId)
                                     (equal? (get s 'sessionId) *acp-session-id*)))
                             sessions)
                        acp-load-session!))))))

(define (acp-load-session! session-id)
  (acp-reset-transcript!)
  (set! *acp-session-id* session-id)
  (set! *acp-session-title* #f)
  (set! *acp-usage* #f)
  (set! *acp-status* 'starting)
  (acp-request! "session/load"
                (hash "sessionId" session-id "cwd" *acp-cwd* "mcpServers" '())
                (lambda (result)
                  (acp-apply-session-result! result)
                  (acp-info! "session resumed"))))

;;; ===========================================================================
;;; prompts

(define (acp-busy?) (> *acp-busy* 0))

(define (acp-send-prompt! text)
  (cond
    [(not *acp-session-id*) (set-status! "acp: session is not ready yet")]
    [else
     (define attachments *acp-attachments*)
     (set! *acp-attachments* '())
     (acp-push-entry! 'user (hash 'text text 'attachments (map (lambda (a) (get a 'label)) attachments)))
     (when (not (acp-busy?)) (set! *acp-turn-start* (now-ms)))
     (set! *acp-busy* (+ *acp-busy* 1))
     (set! *acp-scroll* 0)
     (acp-start-ticker!)
     (acp-request! "session/prompt"
                   (hash "sessionId" *acp-session-id*
                         "prompt" (append (map (lambda (a) (get a 'block)) attachments)
                                          (mention-blocks text)
                                          (map (lambda (n) (hash "type" "text" "text" n)) (acp-take-notes!))
                                          (list (hash "type" "text" "text" text))))
                   (lambda (result)
                     (acp-turn-finished!)
                     (define reason (get result 'stopReason))
                     (cond
                       [(equal? reason "cancelled") (acp-info! "interrupted")]
                       [(and (string? reason) (not (equal? reason "end_turn")))
                        (acp-info! (string-append "stopped: " reason))]
                       [else void]))
                   (lambda (err)
                     (acp-turn-finished!)
                     (acp-error! (to-string (or (get err 'message) "prompt failed")))))]))

(define (acp-turn-finished!)
  (set! *acp-busy* (max 0 (- *acp-busy* 1)))
  ;; the panel may be hidden or unfocused while the agent works
  (when (and (not (acp-busy?)) (not *acp-focused?*))
    (set-status! (string-append "acp: " *acp-agent-title* " finished")))
  (when (and (not (acp-busy?)) *acp-permission*)
    (set! *acp-permission* #f)))

;;@doc
;; Interrupt the running turn.
(define (acp-cancel)
  (when (and *acp-session-id* (acp-busy?))
    (acp-notify! "session/cancel" (hash "sessionId" *acp-session-id*))
    ;; a pending permission prompt is answered as cancelled per the protocol
    (when *acp-permission* (acp-answer-permission! #f))))

;; spinner + elapsed time while a turn runs
(define (acp-start-ticker!)
  (unless *acp-ticking?*
    (set! *acp-ticking?* #t)
    (enqueue-thread-local-callback-with-delay 120 acp-tick!)))

(define (acp-tick!)
  (if (acp-busy?)
      (begin
        (set! *acp-spinner-frame* (+ *acp-spinner-frame* 1))
        (acp-redraw!)
        (enqueue-thread-local-callback-with-delay 120 acp-tick!))
      (begin
        (set! *acp-ticking?* #f)
        (acp-redraw!))))

;;; ===========================================================================
;;; config options (mode / model / effort / ...)

(define (acp-option-by-category category)
  (find-first (lambda (o) (equal? (get o 'category) category)) *acp-config-options*))

(define (acp-option-by-id id)
  (find-first (lambda (o) (equal? (get o 'id) id)) *acp-config-options*))

(define (acp-option-value-name option)
  (define current (get option 'currentValue))
  (define v (find-first (lambda (x) (equal? (get x 'value) current)) (or (get option 'options) '())))
  (if v (get v 'name) (to-string current)))

(define (acp-replace-option-value! id value)
  (set! *acp-config-options*
        (map (lambda (o) (if (equal? (get o 'id) id) (hash-insert o 'currentValue value) o))
             *acp-config-options*)))

(define (acp-set-option! id value)
  (when *acp-session-id*
    (acp-replace-option-value! id value)
    (acp-request! "session/set_config_option"
                  (hash "sessionId" *acp-session-id* "configId" id "value" value)
                  (lambda (result)
                    (define options (get result 'configOptions))
                    (when (list? options) (set! *acp-config-options* options))))))

(define (acp-pick-option! option)
  (when option
    (acp-pick! (get option 'name)
               (map (lambda (v)
                      (list (get v 'name)
                            (or (get v 'description) "")
                            (get v 'value)
                            (equal? (get v 'value) (get option 'currentValue))))
                    (or (get option 'options) '()))
               (lambda (value) (acp-set-option! (get option 'id) value)))))

(define (acp-pick-category! category)
  (define option (acp-option-by-category category))
  (if option
      (acp-pick-option! option)
      (set-status! (string-append "acp: the agent has no " category " option"))))

;;@doc
;; Pick the session mode (permission behaviour).
(define (acp-mode) (acp-pick-category! "mode"))

;;@doc
;; Pick the model.
(define (acp-model) (acp-pick-category! "model"))

;;@doc
;; Pick the reasoning effort.
(define (acp-effort) (acp-pick-category! "thought_level"))

;;@doc
;; Pick any of the agent's session settings.
(define (acp-settings)
  (if (null? *acp-config-options*)
      (set-status! "acp: no settings yet")
      (acp-pick! "Settings"
                 (map (lambda (o) (list (get o 'name) (acp-option-value-name o) (get o 'id) #f))
                      *acp-config-options*)
                 (lambda (id) (acp-pick-option! (acp-option-by-id id))))))

;;@doc
;; Switch to the next session mode, skipping full-access modes.
(define (acp-cycle-mode)
  (define option (acp-option-by-category "mode"))
  (when option
    (define values
      (filter (lambda (v) (not (equal? (get v '_meta 'kind) "full_access")))
              (or (get option 'options) '())))
    (define i (or (index-where (lambda (v) (equal? (get v 'value) (get option 'currentValue))) values) -1))
    (when (pair? values)
      (acp-set-option! (get option 'id) (get (list-ref values (modulo (+ i 1) (length values))) 'value)))))

;;; ===========================================================================
;;; session/update

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
    [(equal? kind "user_message_chunk")
     ;; only sent while replaying a loaded session
     (define text (get u 'content 'text))
     (when (string? text) (acp-append-chunk! 'user text))]
    [(equal? kind "tool_call") (acp-tool-call! u)]
    [(equal? kind "tool_call_update") (acp-tool-call-update! u)]
    [(equal? kind "plan")
     (define entries (or (get u 'entries) '()))
     (if *acp-plan-entry*
         (entry-set! *acp-plan-entry* 'entries entries)
         (set! *acp-plan-entry* (acp-push-entry! 'plan (hash 'entries entries))))]
    [(equal? kind "config_option_update")
     (define options (get u 'configOptions))
     (when (list? options) (set! *acp-config-options* options))]
    [(equal? kind "current_mode_update")
     (define mode (acp-option-by-category "mode"))
     (when mode (acp-replace-option-value! (get mode 'id) (get u 'currentModeId)))]
    [(equal? kind "available_commands_update")
     (set! *acp-commands* (or (get u 'availableCommands) '()))]
    [(equal? kind "usage_update")
     (set! *acp-usage* u)
     (define rl (get u '_meta '_claude/rateLimit))
     (when (hash? rl) (set! *acp-rate-limit* rl))]
    [(equal? kind "session_info_update")
     (define title (get u 'title))
     (when (string? title) (set! *acp-session-title* title))]
    [else void]))

;; ACP replaces a field when an update carries it
(define (acp-tool-merge! e u)
  (for-each (lambda (key)
              (define v (hash-try-get u key))
              (when (and v (not (void? v))) (entry-set! e key v)))
            '(title status kind content locations rawInput)))

(define (acp-tool-call! u)
  (define id (get u 'toolCallId))
  (define e (acp-push-entry! 'tool (hash 'status "pending")))
  (acp-tool-merge! e u)
  (set! *acp-tools* (hash-insert *acp-tools* id e))
  (acp-follow! (acp-locations u)))

(define (acp-tool-call-update! u)
  (define e (hash-try-get *acp-tools* (get u 'toolCallId)))
  (if (not e)
      (acp-tool-call! u)
      (begin
        (acp-tool-merge! e u)
        (define status (get u 'status))
        ;; an edit tool has touched the file on disk by the time it completes
        (when (equal? status "completed")
          (for-each acp-reload-clean-doc! (acp-locations (unbox (Entry-data e)))))
        (acp-follow! (acp-locations u)))))

;;; ===========================================================================
;;; permission requests

(define (acp-handle-request id method params)
  (cond
    [(equal? method "session/request_permission")
     (define tool (get params 'toolCall))
     (set! *acp-permission*
           (hash 'id id
                 'title (or (get tool 'title) "tool")
                 'content (or (get tool 'content) '())
                 'options (or (get params 'options) '())
                 'index 0))
     (set! *acp-scroll* 0)
     ;; the answer is typed in the panel, so pull focus there
     (acp-focus)]
    [else (acp-respond-error! id -32601 (string-append "method not found: " method))]))

(define (acp-answer-permission! option)
  (define id (get *acp-permission* 'id))
  (set! *acp-permission* #f)
  (acp-respond! id
                (hash "outcome"
                      (if option
                          (hash "outcome" "selected" "optionId" (get option 'optionId))
                          (hash "outcome" "cancelled")))))

;;; ===========================================================================
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
                          (cons path (and (number? line) (max 1 (->int line))))))
                   locs))
      '()))

(define (acp-find-doc path)
  (find-first (lambda (id) (equal? (editor-document->path id) path)) (editor-all-documents)))

;; leaves unsaved buffers alone
(define (acp-reload-clean-doc! loc)
  (define doc (acp-find-doc (car loc)))
  (when (and doc (not (editor-document-dirty? doc)))
    (editor-document-reload doc)))

(define (acp-open-location! loc)
  (with-handler
   (lambda (err) (log::warn! (to-string "acp open: " err)))
   (acp-reload-clean-doc! loc)
   (helix.open (car loc))
   (when (cdr loc)
     (helix.goto (number->string (cdr loc)))
     (align_view_center))))

(define (acp-follow! locations)
  (when (and *acp-follow?* (pair? locations))
    (acp-open-location! (car locations))))

;; clicking a tool call opens the file it touched
;; the header row of a tool call or thought toggles its full output; other rows
;; of a tool call open the file it touched
(define (acp-click-transcript! event)
  (define hit (assoc (event-mouse-row event) *acp-row-entries*))
  (define e (and hit (car (cdr hit))))
  (define header? (and hit (= (cdr (cdr hit)) 0)))
  (define kind (and e (Entry-kind e)))
  (define locs (if (equal? kind 'tool) (acp-locations (unbox (Entry-data e))) '()))
  (cond
    [(and (member kind '(tool thought)) (or header? (null? locs)))
     (entry-set! e 'expanded (not (entry-get e 'expanded)))
     #t]
    [(pair? locs) (acp-open-location! (car locs)) #t]
    [else #f]))

;;@doc
;; Toggle whether the editor follows the files the agent reads and edits.
(define (acp-follow-toggle)
  (set! *acp-follow?* (not *acp-follow?*))
  (set-status! (if *acp-follow?* "acp: follow on" "acp: follow off")))

;;; ===========================================================================
;;; diff review

;; unified-style text for the diff blocks of a tool call
(define (text-lines t)
  (define ls (if (string? t) (split-many t "\n") '()))
  (if (and (pair? ls) (equal? (last ls) "")) (reverse (cdr (reverse ls))) ls))

(define (diff-block-text c)
  (define old (text-lines (get c 'oldText)))
  (define new (text-lines (get c 'newText)))
  (define prefix
    (let loop ([a old] [b new] [n 0])
      (if (and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b) (+ n 1)) n)))
  (define old* (list-tail old prefix))
  (define new* (list-tail new prefix))
  (define suffix
    (let loop ([a (reverse old*)] [b (reverse new*)] [n 0])
      (if (and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b) (+ n 1)) n)))
  (define rel (relative-path (or (get c 'path) "")))
  (string-join
   (append (list (string-append "--- a/" rel) (string-append "+++ b/" rel)
                 (string-append "@@ -1," (number->string (length old)) " +1," (number->string (length new)) " @@"))
           (map (lambda (l) (string-append " " l)) (take-at-most old prefix))
           (map (lambda (l) (string-append "-" l)) (take-at-most old* (- (length old*) suffix)))
           (map (lambda (l) (string-append "+" l)) (take-at-most new* (- (length new*) suffix)))
           (map (lambda (l) (string-append " " l)) (take-last old* suffix)))
   "\n"))

(define (diff-blocks content)
  (filter (lambda (c) (equal? (get c 'type) "diff")) (or content '())))

(define *acp-diff-count* 0)

;; details open as real files: a scratch buffer filled via insert_string panics
;; helix when it is closed
(define (text-blocks content)
  (filter (lambda (c) (and (equal? (get c 'type) "content") (string? (get c 'content 'text))))
          (or content '())))

(define (acp-write-tmp! name text)
  (define dir "/tmp/acp-hx")
  (with-handler (lambda (_) void) (create-directory! dir))
  (set! *acp-diff-count* (+ *acp-diff-count* 1))
  (define path (string-append dir "/" (number->string *acp-diff-count*) "-" name))
  (define port (open-output-file path #:exists 'truncate))
  (write-string text port)
  (close-output-port port)
  path)

(define (acp-open-diff! title content)
  (define blocks (diff-blocks content))
  (define texts (text-blocks content))
  (cond
    [(pair? blocks) (acp-open-diff-blocks! blocks)]
    ;; plans and other prose open as markdown
    [(pair? texts)
     (helix.open (acp-write-tmp! "details.md"
                                 (string-join (map (lambda (c) (get c 'content 'text)) texts) "\n\n")))]
    [else (set-status! "acp: nothing to show")]))

(define (acp-open-diff-blocks! blocks)
  (define name (last (split-many (or (get (car blocks) 'path) "edit") "/")))
  (helix.open (acp-write-tmp! (string-append name ".diff")
                              (string-append (string-join (map diff-block-text blocks) "\n") "\n"))))

;;@doc
;; Open the pending permission's diff or plan, or the latest edit's diff.
(define (acp-diff)
  (cond
    [*acp-permission*
     (acp-unfocus!)
     (acp-open-diff! (get *acp-permission* 'title) (get *acp-permission* 'content))]
    [else
     (define e (find-first (lambda (e) (and (equal? (Entry-kind e) 'tool)
                                            (pair? (diff-blocks (entry-get e 'content)))))
                           *acp-entries*))
     (if e
         (begin (acp-unfocus!) (acp-open-diff! (or (entry-get e 'title) "edit") (entry-get e 'content)))
         (set-status! "acp: no edits yet"))]))

;;@doc
;; Open every edit the agent made in this session as one diff, oldest first.
(define (acp-review)
  (define edits
    (filter (lambda (e) (and (equal? (Entry-kind e) 'tool)
                             (equal? (entry-get e 'status) "completed")
                             (pair? (diff-blocks (entry-get e 'content)))))
            (reverse *acp-entries*)))
  (if (null? edits)
      (set-status! "acp: no edits in this session")
      (begin
        (acp-unfocus!)
        (helix.open
         (acp-write-tmp! "review.diff"
                         (string-append
                          (string-join (map (lambda (e)
                                              (string-append "# " (or (entry-get e 'title) "edit") "\n"
                                                             (string-join (map diff-block-text (diff-blocks (entry-get e 'content))) "\n")))
                                            edits)
                                       "\n")
                          "\n"))))))

;;; ===========================================================================
;;; undoing edits

(define (acp-take-notes!)
  (define notes (reverse *acp-notes*))
  (set! *acp-notes* '())
  notes)

(define (read-file path)
  (read-port-to-string (open-input-file path)))

(define (write-file path text)
  (define port (open-output-file path #:exists 'truncate))
  (write-string text port)
  (close-output-port port))

(define (count-occurrences haystack needle)
  (- (length (split-many haystack needle)) 1))

;; put one diff block back; #t when its new text was found exactly once
(define (revert-block! c)
  (define path (get c 'path))
  (define old (or (get c 'oldText) ""))
  (define new (get c 'newText))
  (with-handler
   (lambda (_) #f)
   (and (string? path) (string? new) (not (equal? new ""))
        (let ([content (read-file path)])
          (and (= (count-occurrences content new) 1)
               (begin
                 (write-file path (string-replace content new (if (void? old) "" old)))
                 (acp-reload-clean-doc! (cons path #f))
                 #t))))))

;;@doc
;; Revert the agent's most recent edit that has not been reverted yet.
;; The agent is told about it with the next prompt.
(define (acp-undo-edit)
  (define e (find-first (lambda (e) (and (equal? (Entry-kind e) 'tool)
                                         (equal? (entry-get e 'status) "completed")
                                         (not (entry-get e 'reverted))
                                         (pair? (diff-blocks (entry-get e 'content)))))
                        *acp-entries*))
  (cond
    [(not e) (set-status! "acp: no edit to undo")]
    [else
     (define blocks (reverse (diff-blocks (entry-get e 'content))))
     (define ok? (foldl (lambda (c acc) (and (revert-block! c) acc)) #t blocks))
     (define title (or (entry-get e 'title) "edit"))
     (if ok?
         (begin
           (entry-set! e 'reverted #t)
           (entry-set! e 'title (string-append title " (reverted)"))
           (set! *acp-notes* (cons (string-append "Note: the user reverted your edit \"" title
                                                  "\"; the file is back to its previous content.")
                                   *acp-notes*))
           (set-status! (string-append "acp: reverted " title)))
         (set-status! (string-append "acp: could not revert " title " (the file changed since)")))]))

;;; ===========================================================================
;;; context attachments

(define (acp-attach! label block)
  (set! *acp-attachments* (append *acp-attachments* (list (hash 'label label 'block block))))
  (acp-redraw!))

;;@doc
;; Attach the current file to the next prompt.
(define (acp-add-file)
  (define path (cx->current-file))
  (if (not path)
      (set-status! "acp: the buffer has no file")
      (acp-attach! (string-append "@" (relative-path path))
                   (hash "type" "resource_link"
                         "uri" (string-append "file://" path)
                         "name" (relative-path path)))))

;;@doc
;; Attach an image file (png, jpg, gif, webp) to the next prompt: `:acp-add-image path`
(define (acp-add-image path)
  (define full (if (starts-with? path "/") path (string-append (or *acp-cwd* (helix-find-workspace)) "/" path)))
  (define lower (string-downcase full))
  (define mime
    (cond [(ends-with? lower ".png") "image/png"]
          [(or (ends-with? lower ".jpg") (ends-with? lower ".jpeg")) "image/jpeg"]
          [(ends-with? lower ".gif") "image/gif"]
          [(ends-with? lower ".webp") "image/webp"]
          [else #f]))
  (define data
    (and mime
         (with-handler
          (lambda (_) #f)
          (let ([r (~> (command "base64" (list "-i" full)) (with-stdout-piped) (spawn-process))])
            (and (Ok? r)
                 (let ([out (string-replace (read-port-to-string (child-stdout (Ok->value r))) "\n" "")])
                   (and (not (equal? out "")) out)))))))
  (cond
    [(not mime) (set-status! "acp: only png, jpg, gif and webp images are supported")]
    [(not data) (set-status! (string-append "acp: could not read " full))]
    [else
     (acp-attach! (string-append "🖼 " (relative-path full))
                  (hash "type" "image" "mimeType" mime "data" data "uri" (string-append "file://" full)))
     (set-status! (string-append "acp: attached " (relative-path full)))]))

;;@doc
;; Attach the primary selection (with its line range) to the next prompt.
(define (acp-add-selection)
  (define path (cx->current-file))
  (define doc-id (editor->doc-id (editor-focus)))
  (define rope (editor->text doc-id))
  (define sel (selection->primary-range (current-selection-object)))
  (define from (range->from sel))
  (define to (range->to sel))
  (define l1 (+ 1 (text.rope-char->line rope from)))
  (define l2 (+ 1 (text.rope-char->line rope (max from (- to 1)))))
  (define body (text.rope->string (text.rope->slice rope from to)))
  (define name (if path (relative-path path) "buffer"))
  (define label (string-append "@" name ":" (number->string l1)
                               (if (= l1 l2) "" (string-append "-" (number->string l2)))))
  (acp-attach! label
               (hash "type" "resource"
                     "resource" (hash "uri" (string-append "file://" (or path name)
                                                           "#L" (number->string l1)
                                                           "-" (number->string l2))
                                      "mimeType" "text/plain"
                                      "text" body)))
  (set-status! (string-append "acp: attached " label)))

;;; ===========================================================================
;;; layout: a line is a list of (cons string style-symbol)

(define (seg text style) (cons text style))

(define (segs-width segs)
  (foldl (lambda (s acc) (+ acc (string-width (car s)))) 0 segs))

;; merge consecutive cells of the same style back into segments
(define (cells->segs cells)
  (let loop ([cells cells] [cur '()] [style #f] [out '()])
    (cond
      [(null? cells)
       (reverse (if (null? cur) out (cons (seg (list->string (reverse cur)) style) out)))]
      [(or (null? cur) (equal? (cdr (car cells)) style))
       (loop (cdr cells) (cons (car (car cells)) cur) (cdr (car cells)) out)]
      [else
       (loop (cdr cells) (list (car (car cells))) (cdr (car cells))
             (cons (seg (list->string (reverse cur)) style) out))])))

;; how many cells sit after the last space of a reversed cell list (#f if none nearby)
(define (last-space-offset rev-cells)
  (let loop ([l rev-cells] [i 0])
    (cond [(or (null? l) (> i 24)) #f]
          [(equal? (car (car l)) #\space) i]
          [else (loop (cdr l) (+ i 1))])))

(define (word-char? ch)
  (and (< (char->integer ch) 128) (not (char-whitespace? ch))))

;; wrap styled segments to `width`, prefixing continuation lines with `indent`
(define (wrap-segs segs width indent)
  (define indent-w (segs-width indent))
  (define cells
    (apply append
           (map (lambda (s) (map (lambda (c) (cons c (cdr s))) (string->list (car s)))) segs)))
  (let loop ([cells cells] [cur '()] [w 0] [out '()] [first? #t])
    (define limit (max 4 (if first? width (- width indent-w))))
    (define (emit rev) (if first? (cells->segs (reverse rev)) (append indent (cells->segs (reverse rev)))))
    (cond
      [(null? cells) (reverse (cons (emit cur) out))]
      [else
       (define cw (char-width (car (car cells))))
       (if (and (> (+ w cw) limit) (pair? cur))
           (let ([sp (last-space-offset cur)])
             ;; only move whole latin words; CJK text may break anywhere
             (if (and sp (< sp (- (length cur) 1))
                      (word-char? (car (car cells)))
                      (word-char? (car (car cur))))
                 ;; break after the last space and carry the partial word over
                 (loop (append (reverse (take cur sp)) cells) '() 0
                       (cons (emit (list-tail cur (+ sp 1))) out) #f)
                 (loop (if (equal? (car (car cells)) #\space) (cdr cells) cells) '() 0
                       (cons (emit cur) out) #f)))
           (loop (cdr cells) (cons (car cells) cur) (+ w cw) out first?))])))

;; inline markdown: `code`, **bold**, *italic* / _italic_ and [text](url)
(define (inline-segs text base)
  (define (flush acc style out)
    (if (null? acc) out (cons (seg (list->string (reverse acc)) style) out)))
  ;; index of the first `ch` in the char list, or #f
  (define (find-char cs ch)
    (let loop ([cs cs] [i 0])
      (cond [(null? cs) #f] [(equal? (car cs) ch) i] [else (loop (cdr cs) (+ i 1))])))
  (let loop ([cs (string->list text)] [acc '()] [out '()] [bold? #f] [italic? #f])
    (define style (cond [bold? 'bold] [italic? 'italic] [else base]))
    (cond
      [(null? cs) (reverse (flush acc style out))]
      ;; `code`
      [(and (equal? (car cs) #\`) (find-char (cdr cs) #\`))
       => (lambda (i)
            (loop (list-tail (cdr cs) (+ i 1)) '()
                  (cons (seg (list->string (take (cdr cs) i)) 'code) (flush acc style out))
                  bold? italic?))]
      ;; **bold**
      [(and (equal? (car cs) #\*) (pair? (cdr cs)) (equal? (cadr cs) #\*))
       (loop (cddr cs) '() (flush acc style out) (not bold?) italic?)]
      ;; *italic* only when it opens before a word or closes after one
      [(and (or (equal? (car cs) #\*) (and (equal? (car cs) #\_) (or italic? (null? acc) (char-whitespace? (car acc)))))
            (or italic? (and (pair? (cdr cs)) (not (char-whitespace? (cadr cs))) (find-char (cdr cs) (car cs)))))
       (loop (cdr cs) '() (flush acc style out) bold? (not italic?))]
      ;; [text](url)
      [(and (equal? (car cs) #\[) (find-char (cdr cs) #\]))
       => (lambda (i)
            (define rest (list-tail (cdr cs) (+ i 1)))
            (define close (and (pair? rest) (equal? (car rest) #\() (find-char (cdr rest) #\))))
            (if close
                (loop (list-tail (cdr rest) (+ close 1)) '()
                      (cons (seg (list->string (take (cdr cs) i)) 'link) (flush acc style out))
                      bold? italic?)
                (loop (cdr cs) (cons (car cs) acc) out bold? italic?)))]
      [else (loop (cdr cs) (cons (car cs) acc) out bold? italic?)])))

(define (heading-level line)
  (let loop ([i 0])
    (if (and (< i (string-length line)) (equal? (string-ref line i) #\#)) (loop (+ i 1)) i)))

;; markdown text -> wrapped lines; every line is prefixed by `indent`
;; wrapped markdown lines memoized by (kind, width, raw line): while a message
;; streams only its last line changes, so everything above it is reused
(define *acp-md-memo* (hash))

(define (markdown-line line code? inner)
  (define key (string-append (if code? "C" "T") (number->string inner) "|" line))
  (define hit (hash-try-get *acp-md-memo* key))
  (or hit
      (let ([wrapped (markdown-line-uncached line code? inner)])
        (when (> (hash-length *acp-md-memo*) 4000) (set! *acp-md-memo* (hash)))
        (set! *acp-md-memo* (hash-insert *acp-md-memo* key wrapped))
        wrapped)))

(define (markdown-line-uncached line code? inner)
  (define trimmed (trim line))
  (if code?
      (wrap-segs (list (seg "│ " 'dim) (seg line 'code)) inner (list (seg "│ " 'dim)))
      (let* ([level (heading-level trimmed)]
             [segs
              (cond
                [(and (> level 0) (< level 7))
                 (list (seg (trim (substring trimmed level (string-length trimmed))) 'heading))]
                [(numbered-item trimmed)
                 => (lambda (n)
                      (cons (seg (string-append (make-string (- (string-length line) (string-length (trim-start line))) #\space)
                                                (substring trimmed 0 n))
                                 'list-mark)
                            (inline-segs (substring trimmed n (string-length trimmed)) 'text)))]
                [(or (starts-with? trimmed "- ") (starts-with? trimmed "* "))
                 (cons (seg (string-append (make-string (- (string-length line) (string-length (trim-start line))) #\space)
                                           "• ")
                            'list-mark)
                       (inline-segs (substring trimmed 2 (string-length trimmed)) 'text))]
                [(starts-with? trimmed "> ")
                 (cons (seg "▎ " 'dim) (inline-segs (substring trimmed 2 (string-length trimmed)) 'dim))]
                [(equal? trimmed "---") (list (seg (make-string (max 1 (min inner 24)) #\─) 'dim))]
                [else (inline-segs line 'text)])]
             [hang (if (and (pair? segs) (member (cdr (car segs)) '(dim list-mark)))
                       (list (seg (make-string (string-width (car (car segs))) #\space) 'dim))
                       '())])
        (wrap-segs segs inner hang))))

;; markdown table rows -> aligned lines, or one wrapped line per row when too wide
(define (table-cells row)
  (define parts (map trim (split-many (trim row) "|")))
  ;; drop the empty strings before the first and after the last pipe
  (define inner (if (and (pair? parts) (equal? (car parts) "")) (cdr parts) parts))
  (if (and (pair? inner) (equal? (last inner) "")) (reverse (cdr (reverse inner))) inner))

(define (separator-row? cells)
  (and (pair? cells)
       (null? (filter (lambda (c) (not (and (> (string-length c) 0)
                                           (null? (filter (lambda (ch) (not (member ch '(#\- #\:))))
                                                          (string->list c))))))
                      cells))))

(define (table-lines rows inner)
  (define parsed (map table-cells rows))
  (define body (filter (lambda (r) (not (separator-row? r))) parsed))
  (define cols (foldl (lambda (r acc) (max acc (length r))) 0 body))
  (define cell-segs (map (lambda (r i) (map (lambda (c) (inline-segs c (if (= i 0) 'bold 'text))) r))
                         body (indices (length body))))
  (define widths
    (map (lambda (k)
           (foldl (lambda (r acc) (if (< k (length r)) (max acc (segs-width (list-ref r k))) acc)) 0 cell-segs))
         (indices cols)))
  (define total (+ (foldl + 0 widths) (* 3 (max 0 (- cols 1)))))
  (define (pad segs w) (append segs (list (seg (make-string (max 0 (- w (segs-width segs))) #\space) 'text))))
  (if (> total inner)
      ;; too wide: one card per row, the first cell as its title and the rest as
      ;; "header: value" lines
      (let ([header (if (pair? cell-segs) (car cell-segs) '())])
        (apply append
               (map (lambda (r)
                      (append
                       (wrap-segs (cons (seg "▸ " 'list-mark) (if (pair? r) (car r) '())) inner (list (seg "  " 'dim)))
                       (apply append
                              (map (lambda (c k)
                                     (define name (if (< (+ k 1) (length header)) (list-ref header (+ k 1)) '()))
                                     (wrap-segs (append (list (seg "  " 'dim))
                                                        (map (lambda (sg) (seg (car sg) 'dim)) name)
                                                        (list (seg ": " 'dim))
                                                        c)
                                                inner (list (seg "    " 'dim))))
                                   (if (pair? r) (cdr r) '())
                                   (indices (max 0 (- (length r) 1)))))))
                    (if (pair? cell-segs) (cdr cell-segs) '()))))
      (apply append
             (map (lambda (r i)
                    (define line
                      (apply append (map (lambda (k)
                                           (define c (if (< k (length r)) (list-ref r k) '()))
                                           (if (= k 0) (pad c (list-ref widths k))
                                               (cons (seg " │ " 'dim) (pad c (list-ref widths k)))))
                                         (indices cols))))
                    (if (= i 0)
                        (list line (list (seg (string-join (map (lambda (w) (make-string w #\─)) widths) "─┼─") 'dim)))
                        (list line)))
                  cell-segs (indices (length cell-segs))))))

;; "12. item" -> length of the "12. " marker
(define (numbered-item trimmed)
  (let loop ([i 0])
    (cond
      [(and (< i (string-length trimmed)) (char-digit? (string-ref trimmed i))) (loop (+ i 1))]
      [(and (> i 0) (< (+ i 1) (string-length trimmed))
            (equal? (string-ref trimmed i) #\.) (equal? (string-ref trimmed (+ i 1)) #\space))
       (+ i 2)]
      [else #f])))

;; a line that starts its own block rather than continuing a paragraph
(define (block-start? trimmed)
  (or (equal? trimmed "")
      (starts-with? trimmed "```")
      (starts-with? trimmed "#")
      (starts-with? trimmed "- ")
      (starts-with? trimmed "* ")
      (starts-with? trimmed "> ")
      (starts-with? trimmed "|")
      (equal? trimmed "---")
      (numbered-item trimmed)))

;; markdown soft breaks: join paragraph lines, with a space only between latin words
(define (join-soft-breaks lines)
  (let loop ([ls lines] [code? #f] [out '()])
    (cond
      [(null? ls) (reverse out)]
      [else
       (define line (car ls))
       (define trimmed (trim line))
       (define prev (and (pair? out) (car out)))
       (cond
         [(starts-with? trimmed "```") (loop (cdr ls) (not code?) (cons line out))]
         [code? (loop (cdr ls) #t (cons line out))]
         [(and prev
               (not (block-start? trimmed))
               (not (equal? (trim prev) ""))
               (not (starts-with? (trim prev) "```"))
               (not (starts-with? (trim prev) "#"))
               (not (equal? (trim prev) "---"))
               (not (ends-with? prev "  ")))
          (define a (string-ref prev (- (string-length prev) 1)))
          (define b (string-ref trimmed 0))
          (loop (cdr ls) #f
                (cons (string-append prev (if (and (word-char? a) (word-char? b)) " " "") trimmed) (cdr out)))]
         [else (loop (cdr ls) #f (cons line out))])])))

;; markdown text -> wrapped lines; every line is prefixed by `indent`
(define (markdown-lines text width indent)
  (define inner (- width (segs-width indent)))
  (let loop ([ls (join-soft-breaks (split-many (string-replace text "\t" "  ") "\n"))] [code? #f] [out '()])
    (cond
      [(null? ls) (reverse out)]
      [(starts-with? (trim (car ls)) "```") (loop (cdr ls) (not code?) out)]
      [(and (not code?) (starts-with? (trim (car ls)) "|"))
       ;; a table is laid out as a whole block
       (define rows (let take-rows ([ls ls] [acc '()])
                      (if (and (pair? ls) (starts-with? (trim (car ls)) "|"))
                          (take-rows (cdr ls) (cons (car ls) acc))
                          (reverse acc))))
       (loop (list-tail ls (length rows)) #f
             (append (reverse (map (lambda (l) (append indent l)) (table-lines rows inner))) out))]
      [else
       (define wrapped (markdown-line (car ls) code? inner))
       (loop (cdr ls) code? (append (reverse (map (lambda (l) (append indent l)) wrapped)) out))])))

(define (plain-lines text width indent style)
  (define inner (- width (segs-width indent)))
  (set! text (string-replace text "\t" "  "))
  (apply append
         (map (lambda (l) (map (lambda (w) (append indent w)) (wrap-segs (list (seg l style)) inner '())))
              (split-many text "\n"))))

(define (strip-fences text)
  (define lines (filter (lambda (l) (not (starts-with? (trim l) "```"))) (split-many text "\n")))
  (string-join lines "\n"))

(define *acp-expand-entry?* #f) ; set while laying out an entry the user expanded

(define (expanded?) (or *acp-expand?* *acp-expand-entry?*))

(define (collapse lines limit)
  (if (or (expanded?) (<= (length lines) limit))
      lines
      (append (take-at-most lines limit)
              (list (list (seg (string-append "    … +" (number->string (- (length lines) limit))
                                              " lines (^t to expand)")
                               'dim))))))

(define (collapse-title lines)
  (if (or (expanded?) (<= (length lines) 2))
      lines
      (let ([head (take-at-most lines 2)])
        (append (take-at-most head 1)
                (list (append (cadr head) (list (seg " …" 'dim))))))))

;; compact line diff: trims the common head and tail and keeps one line of context
(define (diff-lines old-text new-text width)
  (define old (if (string? old-text) (split-many old-text "\n") '()))
  (define new (if (string? new-text) (split-many new-text "\n") '()))
  (define prefix
    (let loop ([a old] [b new] [n 0])
      (if (and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b) (+ n 1)) n)))
  (define old* (list-tail old prefix))
  (define new* (list-tail new prefix))
  (define suffix
    (let loop ([a (reverse old*)] [b (reverse new*)] [n 0])
      (if (and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b) (+ n 1)) n)))
  (define removed (take-at-most old* (- (length old*) suffix)))
  (define added (take-at-most new* (- (length new*) suffix)))
  (define before (if (> prefix 0) (list (list-ref old (- prefix 1))) '()))
  (define after (if (> suffix 0) (list (list-ref old* (- (length old*) suffix))) '()))
  (define (row mark text style)
    (list (seg "    " 'dim) (seg (truncate-width (string-append mark text) (- width 4)) style)))
  (append (map (lambda (l) (row "  " l 'dim)) before)
          (map (lambda (l) (row "- " l 'diff-del)) removed)
          (map (lambda (l) (row "+ " l 'diff-add)) added)
          (map (lambda (l) (row "  " l 'dim)) after)))

(define (diff-stat old-text new-text)
  (define old (text-lines old-text))
  (define new (text-lines new-text))
  (define prefix
    (let loop ([a old] [b new] [n 0])
      (if (and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b) (+ n 1)) n)))
  (define old* (list-tail old prefix))
  (define new* (list-tail new prefix))
  (define suffix
    (let loop ([a (reverse old*)] [b (reverse new*)] [n 0])
      (if (and (pair? a) (pair? b) (equal? (car a) (car b))) (loop (cdr a) (cdr b) (+ n 1)) n)))
  (string-append "+" (number->string (- (length new*) suffix)) " -" (number->string (- (length old*) suffix))))

(define (tool-status-style status)
  (cond [(equal? status "completed") 'ok]
        [(equal? status "failed") 'error]
        [(equal? status "in_progress") 'running]
        [else 'pending]))

(define (tool-content-lines content width [limit 4])
  (apply append
         (map (lambda (c)
                (define type (get c 'type))
                (cond
                  [(equal? type "diff")
                   (define path (or (get c 'path) ""))
                   (cons (list (seg "  ⎿ " 'dim)
                               (seg (relative-path path) 'text)
                               (seg (string-append "  " (diff-stat (get c 'oldText) (get c 'newText))) 'dim))
                         (collapse (diff-lines (get c 'oldText) (get c 'newText) width) (max 12 limit)))]
                  [(equal? type "content")
                   (define text (get c 'content 'text))
                   (cond
                     [(or (not (string? text)) (string-blank? (strip-fences text))) '()]
                     ;; command and file output arrives fenced; prose (plans, notes) is markdown
                     [(starts-with? (trim text) "```")
                      (collapse (plain-lines (strip-fences text) width (list (seg "    " 'dim)) 'dim) limit)]
                     [else (collapse (markdown-lines (trim text) width (list (seg "    " 'dim))) limit)])]
                  [else '()]))
              content)))

(define (plan-lines entries width)
  (apply append
         (map (lambda (p)
                (define st (get p 'status))
                (define mark (cond [(equal? st "completed") "☒ "]
                                   [(equal? st "in_progress") "◐ "]
                                   [else "☐ "]))
                (define style (cond [(equal? st "completed") 'dim]
                                    [(equal? st "in_progress") 'running]
                                    [else 'text]))
                (wrap-segs (list (seg (string-append "  " mark) style) (seg (or (get p 'content) "") style))
                           width (list (seg "    " style))))
              entries)))

(define (entry-lines-uncached e width)
  (define kind (Entry-kind e))
  (define text (or (entry-get e 'text) ""))
  (cond
    [(equal? kind 'user)
     (append
      (apply append
             (map (lambda (l i)
                    (wrap-segs (list (seg (if (= i 0) "❯ " "  ") 'user-mark) (seg l 'user))
                               width (list (seg "  " 'user))))
                  (split-many text "\n")
                  (indices (length (split-many text "\n")))))
      (map (lambda (a) (list (seg "  " 'dim) (seg a 'attachment)))
           (or (entry-get e 'attachments) '())))]
    [(equal? kind 'agent)
     (define lines (markdown-lines (trim text) width (list (seg "  " 'text))))
     (if (null? lines)
         '()
         (cons (cons (seg "⏺ " 'agent-mark) (cdr (car lines))) (cdr lines)))]
    [(equal? kind 'thought)
     (cons (list (seg "✻ Thinking" 'thought))
           (collapse (plain-lines (trim text) width (list (seg "  " 'thought)) 'thought) 3))]
    [(equal? kind 'tool)
     (define status (or (entry-get e 'status) "pending"))
     (define title (string-replace (or (entry-get e 'title) "tool") "\n" " "))
     (append
      ;; multi-line shell commands would otherwise flood the transcript
      (collapse-title (wrap-segs (list (seg "⏺ " (tool-status-style status)) (seg title 'tool-title))
                                 width (list (seg "  " 'tool-title))))
      (tool-content-lines (or (entry-get e 'content) '()) width))]
    [(equal? kind 'plan)
     (cons (list (seg "⏺ " 'agent-mark) (seg "Plan" 'tool-title))
           (plan-lines (or (entry-get e 'entries) '()) width))]
    [(equal? kind 'error) (plain-lines text width (list (seg "! " 'error)) 'error)]
    [else (plain-lines text width (list (seg "· " 'dim)) 'dim)]))

(define (entry-lines e width)
  (define cache (unbox (Entry-cache e)))
  (if (and cache (= (car cache) width) (equal? (cadr cache) *acp-expand?*))
      (caddr cache)
      (let ([lines (begin (set! *acp-expand-entry?* (entry-get e 'expanded))
                          (entry-lines-uncached e width))])
        (set! *acp-expand-entry?* #f)
        (set-box! (Entry-cache e) (list width *acp-expand?* lines))
        lines)))

;; the last `needed` transcript lines, walking entries newest first
;; each line is paired with its entry so clicks can find what they hit
(define (transcript-tail width needed)
  (let loop ([es *acp-entries*] [acc '()] [n 0])
    (if (or (null? es) (>= n needed))
        acc
        (let* ([e (car es)]
               [ls (append (map (lambda (l i) (cons (cons e i) l)) (entry-lines e width)
                                (indices (length (entry-lines e width))))
                           (list (cons #f '())))])
          (loop (cdr es) (append ls acc) (+ n (length ls)))))))

(define *acp-row-entries* '()) ; (y entry . line-index) of the rows drawn last frame
(define *acp-last-total* #f)

(define (transcript-length width)
  (foldl (lambda (e acc) (+ acc 1 (length (entry-lines e width)))) 0 *acp-entries*))

;;; ===========================================================================
;;; rendering

(define (acp-redraw!)
  (when *acp-open?* (helix.redraw)))

(define (acp-panel-width rect)
  (max 30 (min *acp-width* (- (area-width rect) 30))))

(define (style-of sym)
  (cond
    [(equal? sym 'text) (theme-scope-ref "ui.text")]
    [(equal? sym 'dim) (style-with-dim (theme-scope-ref "ui.text"))]
    [(equal? sym 'bold) (style-with-bold (theme-scope-ref "ui.text"))]
    [(equal? sym 'italic) (style-with-italics (theme-scope-ref "ui.text"))]
    [(equal? sym 'list-mark) (theme-scope-ref "markup.list")]
    [(equal? sym 'link) (theme-scope-ref "markup.link.text")]
    [(equal? sym 'code) (theme-scope-ref "markup.raw")]
    [(equal? sym 'heading) (style-with-bold (theme-scope-ref "markup.heading"))]
    [(equal? sym 'user) (style-with-bold (theme-scope-ref "ui.text"))]
    [(equal? sym 'user-mark) (style-with-bold (theme-scope-ref "keyword"))]
    [(equal? sym 'agent-mark) (theme-scope-ref "ui.text")]
    [(equal? sym 'attachment) (theme-scope-ref "markup.link.url")]
    [(equal? sym 'thought) (style-with-italics (theme-scope-ref "comment"))]
    [(equal? sym 'tool-title) (style-with-bold (theme-scope-ref "ui.text"))]
    [(equal? sym 'ok) (theme-scope-ref "diff.plus")]
    [(equal? sym 'error) (theme-scope-ref "error")]
    [(equal? sym 'running) (theme-scope-ref "warning")]
    [(equal? sym 'pending) (style-with-dim (theme-scope-ref "ui.text"))]
    [(equal? sym 'diff-add) (theme-scope-ref "diff.plus")]
    [(equal? sym 'diff-del) (theme-scope-ref "diff.minus")]
    [(equal? sym 'accent) (theme-scope-ref "keyword")]
    [(equal? sym 'info) (theme-scope-ref "info")]
    [(equal? sym 'warning) (theme-scope-ref "warning")]
    [(equal? sym 'selected) (theme-scope-ref "ui.menu.selected")]
    [(equal? sym 'border) (theme-scope-ref "ui.window")]
    [else (theme-scope-ref "ui.text")]))

;; draw segments starting at x, clipped to `width` columns; returns the end column
(define (draw-segs frame x y segs width bg)
  (let loop ([segs segs] [cx x] [left width])
    (if (or (null? segs) (<= left 0))
        cx
        (let* ([s (car segs)]
               [text (truncate-width (car s) left)]
               [w (string-width text)])
          (frame-set-string! frame cx y text (if bg (style-bg (style-of (cdr s)) bg) (style-of (cdr s))))
          (loop (cdr segs) (+ cx w) (- left w))))))

(define (mode-style option)
  (define current (get option 'currentValue))
  (define v (find-first (lambda (x) (equal? (get x 'value) current)) (or (get option 'options) '())))
  (define kind (and v (get v '_meta 'kind)))
  (cond [(equal? kind "plan") 'info]
        [(equal? kind "full_access") 'error]
        [(equal? current "acceptEdits") 'warning]
        [(equal? kind "auto_review") 'accent]
        [else 'dim]))

(define (status-segs)
  (define mode (acp-option-by-category "mode"))
  (define model (acp-option-by-category "model"))
  (define effort (acp-option-by-category "thought_level"))
  (define fast (acp-option-by-id "fast"))
  (define (sep) (seg "  " 'dim))
  (append
   (if mode (list (seg (string-append "⏵⏵ " (acp-option-value-name mode)) (mode-style mode)) (sep)) '())
   (if model (list (seg (string-append "◆ " (acp-option-value-name model)) 'text) (sep)) '())
   (if effort (list (seg (string-append "◇ " (acp-option-value-name effort)) 'text) (sep)) '())
   (if (and fast (equal? (get fast 'currentValue) "on")) (list (seg "⚡fast" 'warning) (sep)) '())
   (if *acp-follow?* (list (seg "◉ follow" 'dim)) '())))

(define (usage-segs)
  (define used (get *acp-usage* 'used))
  (define size (get *acp-usage* 'size))
  (define cost (get *acp-usage* 'cost 'amount))
  (define five (get *acp-rate-limit* 'unifiedWindows 'five_hour 'utilization))
  (define week (get *acp-rate-limit* 'unifiedWindows 'seven_day 'utilization))
  (define ratio (if (and (number? used) (number? size) (> size 0)) (/ used size) #f))
  (append
   (if ratio
       (let ([style (cond [(> ratio 0.8) 'error] [(> ratio 0.5) 'warning] [else 'dim])]
             [filled (min 6 (->int (ceiling (* 6 ratio))))])
         (list (seg "ctx " 'dim)
               (seg (make-string filled #\▰) style)
               (seg (make-string (- 6 filled) #\▱) 'dim)
               (seg (string-append " " (format-tokens used) "/" (format-tokens size) " " (format-percent ratio)) style)
               (seg "  " 'dim)))
       '())
   (if (number? cost) (list (seg (format-cost cost) 'dim) (seg "  " 'dim)) '())
   (if (number? five) (list (seg (string-append "5h " (format-percent five)) (if (> five 0.8) 'warning 'dim))
                            (seg "  " 'dim)) '())
   (if (number? week) (list (seg (string-append "7d " (format-percent week)) (if (> week 0.8) 'warning 'dim))) '())))

(define *spinner* (list "·" "✢" "✳" "✶" "✻" "✽" "✻" "✶" "✳" "✢"))

(define (busy-segs)
  (define secs (quotient (- (now-ms) *acp-turn-start*) 1000))
  (list (seg (string-append (list-ref *spinner* (modulo *acp-spinner-frame* (length *spinner*))) " ") 'running)
        (seg "Working… " 'running)
        (seg (string-append "(" (number->string secs) "s"
                            (if (> *acp-busy* 1) (string-append " · " (number->string (- *acp-busy* 1)) " queued") "")
                            " · ^c to interrupt)")
             'dim)))

;; wrap plain chars into row strings, breaking on newlines and width
(define (wrap-chars chars width)
  (let loop ([cs chars] [row '()] [w 0] [rows '()])
    (cond
      [(null? cs) (reverse (cons (list->string (reverse row)) rows))]
      [(equal? (car cs) #\newline) (loop (cdr cs) '() 0 (cons (list->string (reverse row)) rows))]
      [else
       (define cw (char-width (car cs)))
       (if (> (+ w cw) width)
           (loop (cdr cs) (list (car cs)) cw (cons (list->string (reverse row)) rows))
           (loop (cdr cs) (cons (car cs) row) (+ w cw) rows))])))

;; input rows, and the (row . col) of the cursor
(define (input-layout width)
  (define before (reverse *acp-before*))
  (define rows (wrap-chars (append before *acp-after*) width))
  (define before-rows (wrap-chars before width))
  (define col (string-width (last before-rows)))
  (cons rows
        (if (and (>= col width) (pair? *acp-after*))
            (cons (length before-rows) 0)
            (cons (- (length before-rows) 1) col))))

(define (input-string) (list->string (append (reverse *acp-before*) *acp-after*)))

;; the whitespace-delimited word right before the cursor
(define (current-token)
  (let loop ([b *acp-before*] [acc '()])
    (if (or (null? b) (char-whitespace? (car b)))
        (list->string acc)
        (loop (cdr b) (cons (car b) acc)))))

(define (replace-current-token! text)
  (define n (string-length (current-token)))
  (set! *acp-before* (list-tail *acp-before* n))
  (input-insert! text))

;; files ranked: basename prefix, then path prefix, then substring
(define (file-matches q)
  (define ql (string-downcase q))
  (define (score f)
    (define fl (string-downcase f))
    (define base (last (split-many fl "/")))
    (cond [(starts-with? base ql) 0]
          [(starts-with? fl ql) 1]
          [(string-contains? fl ql) 2]
          [else #f]))
  (let loop ([fs *acp-files*] [a '()] [b '()] [c '()] [n 0])
    (cond
      [(or (null? fs) (>= n 200)) (take-at-most (append (reverse a) (reverse b) (reverse c)) 6)]
      [else
       (define sc (score (car fs)))
       (cond [(equal? sc 0) (loop (cdr fs) (cons (car fs) a) b c (+ n 1))]
             [(equal? sc 1) (loop (cdr fs) a (cons (car fs) b) c (+ n 1))]
             [(equal? sc 2) (loop (cdr fs) a b (cons (car fs) c) (+ n 1))]
             [else (loop (cdr fs) a b c n)])])))

;; list of hash: label detail apply submit?
(define (completion-items)
  (define s (input-string))
  (define token (current-token))
  (cond
    [(and (starts-with? s "/") (not (string-contains? s " ")) (not (string-contains? s "\n")))
     (define q (string-downcase (substring s 1 (string-length s))))
     (map (lambda (c)
            (hash 'label (string-append "/" (get c 'name))
                  'detail (or (get c 'description) "")
                  'apply (lambda () (input-set! (string-append "/" (get c 'name) " ")))
                  'submit? #t))
          (take-at-most (filter (lambda (c) (string-contains? (string-downcase (or (get c 'name) "")) q))
                                *acp-commands*)
                        6))]
    [(and (starts-with? token "@") (null? (filter (lambda (c) (not (char-whitespace? c))) (take-at-most *acp-after* 1))))
     (map (lambda (f)
            (hash 'label (string-append "@" f)
                  'detail ""
                  'apply (lambda () (replace-current-token! (string-append "@" f " ")))
                  'submit? #f))
          (file-matches (substring token 1 (string-length token))))]
    [else '()]))

;; resource links for every @path in the prompt that names a workspace file
(define (mention-blocks text)
  (define words (split-many (string-replace text "\n" " ") " "))
  (define paths
    (filter (lambda (w) (and (> (string-length w) 1) (starts-with? w "@")
                             (member (substring w 1 (string-length w)) *acp-files*)))
            words))
  (map (lambda (w)
         (define rel (substring w 1 (string-length w)))
         (hash "type" "resource_link" "uri" (string-append "file://" *acp-cwd* "/" rel) "name" rel))
       paths))

(define (permission-lines width)
  (define p *acp-permission*)
  (define opts (get p 'options))
  (append
   (list (list (seg "? " 'warning) (seg (truncate-width (get p 'title) (- width 2)) 'tool-title)))
   (tool-content-lines (get p 'content) width
                       (if *acp-panel-area* (max 6 (- (quotient (area-height *acp-panel-area*) 2) 6)) 10))
   (map (lambda (o i)
          (define selected? (= i (get p 'index)))
          (list (seg (if selected? " ❯ " "   ") 'accent)
                (seg (string-append (number->string (+ i 1)) ". " (or (get o 'name) "?"))
                     (if selected? 'accent 'text))))
        opts (indices (length opts)))
   (list (list (seg "   ↑↓ select · enter confirm · d details · esc reject" 'dim)))))

(define (hint-segs)
  (list (seg "⏎ send · ^p actions · ⇧⇥ mode · ^o settings · ^r sessions · esc editor" 'dim)))

(define (acp-render state rect frame)
  (define t0 (now-ms))
  (acp-render-panel rect frame)
  (define dt (- (now-ms) t0))
  (when (> dt 30) (log::warn! (string-append "acp.hx: slow render " (number->string dt) "ms"))))

(define (acp-render-panel rect frame)
  (define w (acp-panel-width rect))
  (define x0 (- (area-width rect) w))
  (define y0 1) ; below the bufferline
  (define h (- (area-height rect) y0 1)) ; leave the command line row
  (set! *acp-panel-area* (area x0 y0 w h))
  (set-editor-clip-right! w)

  (define bg (theme-scope-ref "ui.background"))
  (define border (style-of 'border))
  (buffer/clear-with frame (area x0 y0 w h) bg)
  (let loop ([y y0])
    (when (< y (+ y0 h))
      (frame-set-string! frame x0 y "│" border)
      (loop (+ y 1))))

  (define cx (+ x0 2))
  (define cw (- w 3))
  (define rule (list (seg (make-string cw #\─) 'border)))

  ;; header
  (define status-text
    (cond [(equal? *acp-status* 'starting) "starting…"]
          [(equal? *acp-status* 'stopped) "stopped"]
          [else ""]))
  (draw-segs frame cx y0
             (list (seg "✻ " 'accent)
                   (seg *acp-agent-title* (if *acp-focused?* 'heading 'bold))
                   (seg (if *acp-session-title* (string-append "  " *acp-session-title*) "") 'dim)
                   (seg (if (equal? status-text "") "" (string-append "  " status-text)) 'warning))
             cw #f)
  (with-handler (lambda (err) (draw-segs frame cx (+ y0 1) (list (seg (to-string err) 'error)) cw #f))
                (draw-segs frame cx (+ y0 1) (status-segs) cw #f))
  (with-handler (lambda (err) (draw-segs frame cx (+ y0 2) (list (seg (to-string err) 'error)) cw #f))
                (draw-segs frame cx (+ y0 2) (usage-segs) cw #f))
  (draw-segs frame cx (+ y0 3) rule cw #f)
  (define body-top (+ y0 4))

  ;; footer, bottom-up
  (define bottom (+ y0 h -1))
  (draw-segs frame cx bottom (hint-segs) cw #f)
  (define footer-top
    (if *acp-permission*
        (let* ([ls (permission-lines cw)]
               [top (- bottom (length ls))])
          (draw-segs frame cx (- top 1) rule cw #f)
          (let loop ([ls ls] [y top])
            (when (pair? ls)
              (draw-segs frame cx y (car ls) cw #f)
              (loop (cdr ls) (+ y 1))))
          (set! *acp-cursor-pos* #f)
          (- top 1))
        (let* ([layout (input-layout (- cw 2))]
               [rows (take-last (car layout) 6)]
               [hidden (- (length (car layout)) (length rows))]
               [cursor (cdr layout)]
               [input-top (- bottom 1 (length rows))])
          (draw-segs frame cx (- bottom 1) rule cw #f)
          (if (and (null? *acp-before*) (null? *acp-after*))
              (draw-segs frame cx input-top
                         (list (seg "❯ " 'user-mark) (seg "Message the agent… (/ for commands)" 'dim)) cw #f)
              (let loop ([rs rows] [y input-top] [first? (= hidden 0)])
                (when (pair? rs)
                  (draw-segs frame cx y (list (seg (if first? "❯ " "  ") 'user-mark) (seg (car rs) 'text)) cw #f)
                  (loop (cdr rs) (+ y 1) #f))))
          (set! *acp-cursor-pos*
                (position (+ input-top (- (car cursor) hidden)) (+ cx 2 (cdr cursor))))
          ;; attachments
          (define attach-y (- input-top 1))
          (define top
            (if (null? *acp-attachments*)
                input-top
                (begin
                  (draw-segs frame cx attach-y
                             (cons (seg "+ " 'dim)
                                   (map (lambda (a) (seg (string-append (get a 'label) " ") 'attachment))
                                        *acp-attachments*))
                             cw #f)
                  attach-y)))
          ;; slash command / @file completion
          (define items (completion-items))
          (define comp-top (- top (length items)))
          (let loop ([is items] [i 0] [y comp-top])
            (when (pair? is)
              (define selected? (= i (min *acp-completion-index* (- (length items) 1))))
              (draw-segs frame cx y
                         (list (seg (get (car is) 'label) (if selected? 'accent 'text))
                               (seg (if (equal? (get (car is) 'detail) "") "" (string-append "  " (get (car is) 'detail))) 'dim))
                         cw (if selected? (style->bg (style-of 'selected)) #f))
              (loop (cdr is) (+ i 1) (+ y 1))))
          (draw-segs frame cx (- comp-top 1) rule cw #f)
          (- comp-top 1))))

  ;; transcript; while scrolled up, keep the view anchored as new lines arrive
  (if (> *acp-scroll* 0)
      (let ([total (transcript-length cw)])
        (when (and *acp-last-total* (> total *acp-last-total*))
          (set! *acp-scroll* (+ *acp-scroll* (- total *acp-last-total*))))
        (set! *acp-last-total* total))
      (set! *acp-last-total* #f))
  (define busy-row (if (acp-busy?) 1 0))
  (define body-h (max 0 (- footer-top body-top busy-row)))
  (define lines (transcript-tail cw (+ body-h *acp-scroll* 1)))
  (define max-scroll (max 0 (- (length lines) body-h)))
  (when (> *acp-scroll* max-scroll) (set! *acp-scroll* max-scroll))
  (define visible
    (take-last (take-at-most lines (- (length lines) *acp-scroll*)) body-h))
  (set! *acp-row-entries* '())
  (let loop ([ls visible] [y body-top])
    (when (pair? ls)
      (draw-segs frame cx y (cdr (car ls)) cw #f)
      (when (car (car ls)) (set! *acp-row-entries* (cons (cons y (car (car ls))) *acp-row-entries*)))
      (loop (cdr ls) (+ y 1))))
  (when (acp-busy?)
    (draw-segs frame cx (+ body-top (length visible)) (busy-segs) cw #f))
  (when (> *acp-scroll* 0)
    (draw-segs frame (+ x0 w -12) body-top (list (seg (string-append "↑ " (number->string *acp-scroll*) " more") 'dim)) 11 #f)))

(define (acp-cursor state rect)
  (if (and *acp-focused?* (not *acp-permission*) (not *acp-picker*)) *acp-cursor-pos* #f))

;;; ===========================================================================
;;; picker (settings, sessions)

;; items: list of (list label detail value current?)
(define *acp-picker* #f) ; hash: title items query index on-select

(define (acp-pick! title items on-select)
  (set! *acp-picker* (hash 'title title 'items items 'query "" 'index
                           (or (index-where (lambda (i) (list-ref i 3)) items) 0)
                           'on-select on-select))
  (unless *acp-open?* (acp-show!))
  (push-component! (new-component! "acp-picker" #f acp-render-picker
                                   (hash "handle_event" acp-picker-event))))

(define (picker-matches)
  (define q (string-downcase (get *acp-picker* 'query)))
  (filter (lambda (i) (or (equal? q "")
                          (string-contains? (string-downcase (to-string (car i))) q)))
          (get *acp-picker* 'items)))

(define (acp-render-picker state rect frame)
  (when (and *acp-picker* *acp-panel-area*)
    (define pa *acp-panel-area*)
    (define items (picker-matches))
    (define w (- (area-width pa) 4))
    (define h (min (+ (length items) 3) (- (area-height pa) 4)))
    (define x (+ (area-x pa) 2))
    (define y (+ (area-y pa) 4))
    (define box (area x y w h))
    (buffer/clear-with frame box (theme-scope-ref "ui.popup"))
    (block/render frame box (make-block (theme-scope-ref "ui.popup") (theme-scope-ref "ui.popup") "all" "rounded"))
    (define popup-bg (style->bg (theme-scope-ref "ui.popup")))
    (draw-segs frame (+ x 2) y (list (seg (string-append " " (get *acp-picker* 'title) " ") 'heading)) (- w 4) popup-bg)
    (draw-segs frame (+ x 2) (+ y 1) (list (seg "› " 'accent) (seg (get *acp-picker* 'query) 'text)) (- w 4) popup-bg)
    (define index (min (get *acp-picker* 'index) (max 0 (- (length items) 1))))
    (define visible-n (- h 3))
    (define start (max 0 (- index (- visible-n 1))))
    (let loop ([is (list-tail items (min start (length items)))] [i start] [row (+ y 2)])
      (when (and (pair? is) (< row (+ y h -1)))
        (define item (car is))
        (define selected? (= i index))
        (define bg (if selected? (style->bg (style-of 'selected)) popup-bg))
        (buffer/clear-with frame (area (+ x 1) row (- w 2) 1) (if selected? (style-of 'selected) (theme-scope-ref "ui.popup")))
        ;; detail sits flush right, clipped so the label keeps most of the row
        (define detail (truncate-width (to-string (cadr item)) (quotient (- w 6) 2)))
        (define detail-w (string-width detail))
        (draw-segs frame (+ x 2) row
                   (list (seg (if (list-ref item 3) "● " "  ") 'accent)
                         (seg (to-string (car item)) (if selected? 'bold 'text)))
                   (- w 5 detail-w) bg)
        (draw-segs frame (- (+ x w) 2 detail-w) row (list (seg detail 'dim)) detail-w bg)
        (loop (cdr is) (+ i 1) (+ row 1))))))

(define (acp-close-picker!)
  (set! *acp-picker* #f)
  (pop-last-component-by-name! "acp-picker"))

(define (acp-picker-event state event)
  (define ch (and (key-event? event) (key-event-char event)))
  (define items (picker-matches))
  (define index (get *acp-picker* 'index))
  (define (set-index! i) (set! *acp-picker* (hash-insert *acp-picker* 'index i)))
  (cond
    [(not (key-event? event)) event-result/consume]
    [(key-event-escape? event) (acp-close-picker!) event-result/consume]
    [(key-event-enter? event)
     (define on-select (get *acp-picker* 'on-select))
     (define item (and (pair? items) (list-ref items (min index (- (length items) 1)))))
     (acp-close-picker!)
     (when item (on-select (list-ref item 2)))
     event-result/consume]
    [(or (key-event-down? event) (and (ctrl? event) (equal? ch #\n)))
     (set-index! (min (+ index 1) (max 0 (- (length items) 1))))
     event-result/consume]
    [(or (key-event-up? event) (and (ctrl? event) (equal? ch #\p)))
     (set-index! (max 0 (- index 1)))
     event-result/consume]
    [(key-event-backspace? event)
     (define q (get *acp-picker* 'query))
     (set! *acp-picker* (hash-insert (hash-insert *acp-picker* 'query
                                                  (if (> (string-length q) 0)
                                                      (list->string (reverse (cdr (reverse (string->list q)))))
                                                      q))
                                     'index 0))
     event-result/consume]
    [(and (char? ch) (not (ctrl? event)))
     (set! *acp-picker* (hash-insert (hash-insert *acp-picker* 'query
                                                  (string-append (get *acp-picker* 'query) (string ch)))
                                     'index 0))
     event-result/consume]
    [else event-result/consume]))

;;; ===========================================================================
;;; input handling

(define (ctrl? event) (equal? (key-event-modifier event) key-modifier-ctrl))
(define (alt? event) (equal? (key-event-modifier event) key-modifier-alt))
(define (shift? event) (equal? (key-event-modifier event) key-modifier-shift))

(define (input-set! s)
  (set! *acp-before* (reverse (string->list s)))
  (set! *acp-after* '())
  (set! *acp-completion-index* 0))

(define (input-insert! s)
  (set! *acp-before* (append (reverse (string->list s)) *acp-before*))
  (set! *acp-completion-index* 0))

(define (input-empty?) (and (null? *acp-before*) (null? *acp-after*)))

(define (input-single-line?) (not (string-contains? (input-string) "\n")))

(define (input-delete-word!)
  (let loop ([b *acp-before*] [seen-word? #f])
    (cond
      [(null? b) (set! *acp-before* '())]
      [(char-whitespace? (car b)) (if seen-word? (set! *acp-before* b) (loop (cdr b) #f))]
      [else (loop (cdr b) #t)])))

(define (history-move! delta)
  (define n (length *acp-history*))
  (define pos (max -1 (min (- n 1) (+ *acp-history-pos* delta))))
  (set! *acp-history-pos* pos)
  (input-set! (if (< pos 0) "" (list-ref *acp-history* pos))))

(define (acp-submit!)
  (define text (trim (input-string)))
  (unless (equal? text "")
    (set! *acp-history* (cons text *acp-history*))
    (set! *acp-history-pos* -1)
    (input-set! "")
    (acp-send-prompt! text)))

(define (selected-completion)
  (define items (completion-items))
  (and (pair? items) (list-ref items (min *acp-completion-index* (- (length items) 1)))))

(define (acp-complete!)
  (define item (selected-completion))
  (when item ((get item 'apply))))

(define (acp-handle-permission-key event)
  (define ch (key-event-char event))
  (define opts (get *acp-permission* 'options))
  (define index (get *acp-permission* 'index))
  (define (set-index! i) (set! *acp-permission* (hash-insert *acp-permission* 'index i)))
  (cond
    [(key-event-escape? event)
     ;; esc rejects like Claude Code does, falling back to cancelling
     (acp-answer-permission! (find-first (lambda (o) (equal? (get o 'kind) "reject_once")) opts))]
    [(key-event-enter? event) (acp-answer-permission! (list-ref opts index))]
    [(or (key-event-down? event) (equal? ch #\j)) (set-index! (min (+ index 1) (- (length opts) 1)))]
    [(or (key-event-up? event) (equal? ch #\k)) (set-index! (max 0 (- index 1)))]
    [(and (char? ch) (char-digit? ch))
     (define i (- (char->integer ch) (char->integer #\1)))
     (when (and (>= i 0) (< i (length opts)))
       (acp-answer-permission! (list-ref opts i)))]
    [(and (ctrl? event) (equal? ch #\c)) (acp-cancel)]
    [(equal? ch #\d) (acp-diff)]
    [else void])
  event-result/consume)

(define (acp-scroll-by! n)
  (set! *acp-scroll* (max 0 (+ *acp-scroll* n))))

(define (mouse-scroll event)
  (cond [(not (mouse-event? event)) #f]
        [(equal? (event-mouse-kind event) 11) 'up]
        [(equal? (event-mouse-kind event) 10) 'down]
        [else #f]))

(define (in-panel? event)
  (and *acp-panel-area* (mouse-event? event)
       (>= (event-mouse-col event) (area-x *acp-panel-area*))))

(define (acp-handle-event state event)
  (define ch (and (key-event? event) (key-event-char event)))
  (define completing? (pair? (completion-items)))
  (cond
    [(paste-event? event) (input-insert! (paste-event-string event)) event-result/consume]
    [(mouse-event? event)
     (define dir (mouse-scroll event))
     (cond [(and dir (in-panel? event)) (acp-scroll-by! (if (equal? dir 'up) 3 -3)) event-result/consume]
           [(and (in-panel? event) (equal? (event-mouse-kind event) 0))
            (acp-click-transcript! event)
            event-result/consume]
           [(in-panel? event) event-result/consume]
           ;; clicking the editor hands focus back to it
           [else (acp-unfocus!) event-result/ignore])]
    [(not (key-event? event)) event-result/ignore]
    [*acp-permission* (acp-handle-permission-key event)]
    [(key-event-escape? event) (acp-unfocus!) event-result/consume]
    [(and (key-event-tab? event) (shift? event)) (acp-cycle-mode) event-result/consume]
    [(key-event-tab? event) (acp-complete!) event-result/consume]
    [(and (key-event-enter? event) (alt? event)) (input-insert! "\n") event-result/consume]
    [(key-event-enter? event)
     (define item (and completing? (selected-completion)))
     (cond
       [(not item) (acp-submit!)]
       ;; a slash command runs right away, a file mention keeps editing
       [(get item 'submit?) ((get item 'apply)) (acp-submit!)]
       [else ((get item 'apply))])
     event-result/consume]
    [(ctrl? event)
     (cond
       [(equal? ch #\c) (if (acp-busy?) (acp-cancel) (input-set! ""))]
       [(equal? ch #\j) (input-insert! "\n")]
       [(equal? ch #\o) (acp-settings)]
       [(equal? ch #\p) (acp-menu)]
       [(equal? ch #\r) (acp-sessions)]
       [(equal? ch #\n) (acp-new-session)]
       [(equal? ch #\t) (acp-expand-toggle)]
       [(equal? ch #\f) (acp-follow-toggle)]
       [(equal? ch #\u) (set! *acp-before* '())]
       [(equal? ch #\w) (input-delete-word!)]
       [(equal? ch #\a) (set! *acp-after* (append (reverse *acp-before*) *acp-after*)) (set! *acp-before* '())]
       [(equal? ch #\e) (set! *acp-before* (append (reverse *acp-after*) *acp-before*)) (set! *acp-after* '())]
       [(equal? ch #\d) (unless (null? *acp-after*) (set! *acp-after* (cdr *acp-after*)))]
       [else void])
     event-result/consume]
    [(key-event-backspace? event)
     (cond [(pair? *acp-before*) (set! *acp-before* (cdr *acp-before*))]
           [(and (input-empty?) (pair? *acp-attachments*))
            (set! *acp-attachments* (reverse (cdr (reverse *acp-attachments*))))]
           [else void])
     (set! *acp-completion-index* 0)
     event-result/consume]
    [(key-event-delete? event)
     (unless (null? *acp-after*) (set! *acp-after* (cdr *acp-after*)))
     event-result/consume]
    [(key-event-left? event)
     (when (pair? *acp-before*)
       (set! *acp-after* (cons (car *acp-before*) *acp-after*))
       (set! *acp-before* (cdr *acp-before*)))
     event-result/consume]
    [(key-event-right? event)
     (when (pair? *acp-after*)
       (set! *acp-before* (cons (car *acp-after*) *acp-before*))
       (set! *acp-after* (cdr *acp-after*)))
     event-result/consume]
    [(key-event-home? event)
     (set! *acp-after* (append (reverse *acp-before*) *acp-after*))
     (set! *acp-before* '())
     event-result/consume]
    [(key-event-end? event)
     (set! *acp-before* (append (reverse *acp-after*) *acp-before*))
     (set! *acp-after* '())
     event-result/consume]
    [(key-event-up? event)
     (cond [completing? (set! *acp-completion-index* (max 0 (- *acp-completion-index* 1)))]
           [(input-single-line?) (history-move! 1)]
           [else void])
     event-result/consume]
    [(key-event-down? event)
     (cond [completing? (set! *acp-completion-index* (+ *acp-completion-index* 1))]
           [(input-single-line?) (history-move! -1)]
           [else void])
     event-result/consume]
    [(key-event-page-up? event) (acp-scroll-by! 10) event-result/consume]
    [(key-event-page-down? event) (acp-scroll-by! -10) event-result/consume]
    [(char? ch) (input-insert! (string ch)) event-result/consume]
    [else event-result/consume]))

;; the bg component draws the panel; unfocused it only claims mouse wheel over the panel
(define (acp-bg-event state event)
  (define dir (mouse-scroll event))
  (cond
    [(and dir (in-panel? event))
     (acp-scroll-by! (if (equal? dir 'up) 3 -3))
     (acp-redraw!)
     event-result/consume]
    [(and (mouse-event? event) (in-panel? event) (equal? (event-mouse-kind event) 0))
     ;; left click opens a tool call's file, anywhere else focuses the panel
     (unless (acp-click-transcript! event) (acp-focus))
     event-result/consume]
    [else event-result/ignore]))

;;; ===========================================================================
;;; components & commands

(define (acp-make-bg)
  (new-component! "acp-bg" #f acp-render (hash "handle_event" acp-bg-event)))

(define (acp-make-fg)
  (new-component! "acp-fg"
                  #f
                  (lambda (s r f) void)
                  (hash "handle_event" acp-handle-event "cursor" acp-cursor)))

(define (acp-show!)
  (unless *acp-open?*
    (set! *acp-open?* #t)
    (push-component! (acp-make-bg))))

(define (acp-unfocus!)
  (when *acp-focused?*
    (set! *acp-focused?* #f)
    (pop-last-component-by-name! "acp-fg")))

;;@doc
;; Open the sidebar (starting the agent if needed) and focus it.
(define (acp-open)
  (acp-show!)
  (acp-start!)
  (acp-focus))

;;@doc
;; Focus the sidebar input.
(define (acp-focus)
  (acp-show!)
  (unless *acp-focused?*
    (set! *acp-focused?* #t)
    (push-component! (acp-make-fg))))

;;@doc
;; Hide the sidebar. The agent keeps running.
(define (acp-close)
  (acp-unfocus!)
  (when *acp-picker* (acp-close-picker!))
  (when *acp-open?*
    (set! *acp-open?* #f)
    (pop-last-component-by-name! "acp-bg")
    (set-editor-clip-right! 0)))

;;@doc
;; Toggle the sidebar: open -> focus -> close.
(define (acp-toggle)
  (cond
    [(not *acp-open?*) (acp-open)]
    [(not *acp-focused?*) (acp-focus)]
    [else (acp-close)]))

;;@doc
;; Stop the agent process.
(define (acp-quit)
  (when *acp-proc*
    (kill *acp-proc*)))

;;@doc
;; Restart the agent process with a fresh session.
(define (acp-restart)
  (acp-quit)
  (set! *acp-proc* #f)
  (set! *acp-stdin* #f)
  (set! *acp-session-id* #f)
  (set! *acp-busy* 0)
  (set! *acp-permission* #f)
  (set! *acp-config-options* '())
  (set! *acp-commands* '())
  (set! *acp-usage* #f)
  (set! *acp-session-title* #f)
  (acp-reset-transcript!)
  (acp-open))

;;@doc
;; Pick one of the configured agents and restart with it.
(define (acp-switch-agent)
  (acp-pick! "Agent"
             (map (lambda (a) (list (car a) (cdr a) (cdr a) (equal? (cdr a) *acp-command*))) *acp-agents*)
             (lambda (command)
               (set! *acp-command* command)
               (acp-restart))))

;;@doc
;; Toggle showing tool output, diffs and thinking in full.
(define (acp-expand-toggle)
  (set! *acp-expand?* (not *acp-expand?*))
  (acp-redraw!))

;;@doc
;; Widen the sidebar by 8 columns.
(define (acp-wider)
  (set! *acp-width* (+ *acp-width* 8))
  (acp-invalidate-all!))

;;@doc
;; Narrow the sidebar by 8 columns.
(define (acp-narrower)
  (set! *acp-width* (max 30 (- *acp-width* 8)))
  (acp-invalidate-all!))

;;; ===========================================================================
;;; reusing responses

(define (last-agent-text)
  (define e (find-first (lambda (e) (equal? (Entry-kind e) 'agent)) *acp-entries*))
  (and e (trim (entry-get e 'text))))

;; body of the last fenced code block in the text
(define (last-code-block text)
  (let loop ([ls (split-many text "\n")] [in? #f] [cur '()] [last-block #f])
    (cond
      [(null? ls) (if (and in? (pair? cur)) (string-join (reverse cur) "\n") last-block)]
      [(starts-with? (trim (car ls)) "```")
       (if in?
           (loop (cdr ls) #f '() (string-join (reverse cur) "\n"))
           (loop (cdr ls) #t '() last-block))]
      [in? (loop (cdr ls) #t (cons (car ls) cur) last-block)]
      [else (loop (cdr ls) #f cur last-block)])))

;;@doc
;; Copy the agent's last response to the system clipboard.
(define (acp-yank)
  (define text (last-agent-text))
  (if text
      (begin (set-register! #\+ (list text)) (set-status! "acp: copied the last response"))
      (set-status! "acp: no response yet")))

;;@doc
;; Paste the last code block of the agent's last response after the selection.
(define (acp-insert-code)
  (define text (last-agent-text))
  (define code (and text (last-code-block text)))
  (if code
      (begin
        (acp-unfocus!)
        (set-register! #\" (list (string-append code "\n")))
        (paste_after))
      (set-status! "acp: no code block in the last response")))

;;@doc
;; Pick any acp.hx action from a list that also shows its key.
(define (acp-menu)
  (define actions
    (list (list "Settings" "^o" acp-settings)
          (list "Switch mode" "⇧⇥" acp-cycle-mode)
          (list "Model" "" acp-model)
          (list "Effort" "" acp-effort)
          (list "New session" "^n" acp-new-session)
          (list "Resume session" "^r" acp-sessions)
          (list "Interrupt" "^c" acp-cancel)
          (list "Show diff / plan details" "d" acp-diff)
          (list "Review session edits" "" acp-review)
          (list "Undo last edit" "" acp-undo-edit)
          (list "Attach current file" "" acp-add-file)
          (list "Copy last response" "" acp-yank)
          (list "Insert last code block" "" acp-insert-code)
          (list "Expand all output" "^t" acp-expand-toggle)
          (list "Toggle follow-along" "^f" acp-follow-toggle)
          (list "Switch agent" "" acp-switch-agent)
          (list "Restart agent" "" acp-restart)
          (list "Wider panel" "" acp-wider)
          (list "Narrower panel" "" acp-narrower)
          (list "Close panel" "" acp-close)))
  (acp-pick! "Actions"
             (map (lambda (a) (list (car a) (cadr a) (caddr a) #f)) actions)
             (lambda (thunk) (thunk))))
