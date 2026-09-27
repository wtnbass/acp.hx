;; dev config: HELIX_STEEL_CONFIG=$PWD/dev hx
;; ACP_HX_AGENT overrides the agent command, e.g. ACP_HX_AGENT="node dev/fake-agent.mjs"
(require "../acp.scm")

(let ([agent (maybe-get-env-var "ACP_HX_AGENT")])
  (when (Ok? agent)
    (acp-configure! #:command (Ok->value agent))))
