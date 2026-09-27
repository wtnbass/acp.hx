#!/bin/sh
# End-to-end checks against dev/fake-agent.mjs, driven through tmux.
cd "$(dirname "$0")/.."
S=acphx-test
fail=0
screen() { tmux capture-pane -t $S -p; }
# poll the screen for up to 5s
expect() {
  i=0
  while [ $i -lt 25 ]; do
    if screen | grep -qF -- "$1"; then echo "ok   $2"; return; fi
    sleep 0.2; i=$((i + 1))
  done
  echo "FAIL $2 (missing: $1)"; fail=1
}
keys() { tmux send-keys -t $S "$@"; }
type_() { tmux send-keys -t $S -l "$1"; }

tmux kill-session -t $S 2>/dev/null
tmux new-session -d -s $S -x 200 -y 50 \
  "cd $PWD && ACP_HX_AGENT='node $PWD/dev/fake-agent.mjs' HELIX_STEEL_CONFIG=$PWD/dev hx 2>/tmp/acp-hx-test-stderr.log"
sleep 3
if tmux capture-pane -t $S -p | grep -q "error\["; then
  tmux capture-pane -t $S -p | grep -A8 "error\["; tmux kill-session -t $S; exit 1
fi

keys ":acp-open" Enter; sleep 2
expect "Fake Agent" "header shows the agent title"
expect "◆ Fake 1.0" "status shows the model"

type_ "/re"; sleep 0.5
expect "/review  Review the diff" "slash command completion"
keys C-u

type_ "plan"; keys Enter; sleep 1.2
expect "☒ Run the tests" "plan checklist"

type_ "all"; keys Enter; sleep 1.5
expect "? Edit src/main.rs" "permission prompt"
expect "+1 -1" "diff stat"
keys 1; sleep 1.5
expect "error[E0425]" "failed tool output"
expect "• first point" "markdown bullets"
expect '51k/200k 26%  $0.43' "usage line"
expect "Fake session (all)" "session title"

keys C-f; sleep 0.3
click() { type_ "$(printf '\033[<0;150;%sM\033[<0;150;%sm' "$1" "$1")"; sleep 0.8; }
row=$(tmux capture-pane -t $S -p | grep -n "Read README.md" | head -1 | cut -d: -f1)
expect "+4 lines (^t to expand)" "long tool output is collapsed"
click "$row"
expect "readme line 8" "clicking a tool header expands it"
# expanding pushes the header up because the view sticks to the bottom
row=$(tmux capture-pane -t $S -p | grep -n "Read README.md" | head -1 | cut -d: -f1)
click "$row"
if screen | grep -qF "readme line 8"; then echo "FAIL clicking again collapses it"; fail=1; else echo "ok   clicking again collapses it"; fi
row=$(tmux capture-pane -t $S -p | grep -n "Read README.md" | head -1 | cut -d: -f1)
click $((row + 1))
if tmux capture-pane -t $S -p | grep -q "NOR   README.md"; then echo "ok   clicking a tool call opens its file"; else echo "FAIL clicking a tool call opens its file"; fail=1; fi
keys ":acp-focus" Enter; sleep 0.3

keys Escape; sleep 0.3
keys ":acp-review" Enter; sleep 0.8
expect '+    println!("hello");' "review opens the session's edits"
keys ":bc" Enter; sleep 0.3
keys ":acp-focus" Enter; sleep 0.3

keys C-o; sleep 0.5; type_ "model"; keys Enter; sleep 0.3; keys Down Enter; sleep 0.8
expect "◆ Fake Turbo" "model picker"

keys BTab; sleep 0.8
expect "⏵⏵ Plan" "shift-tab cycles mode"

keys C-r; sleep 0.5; keys Enter; sleep 1
expect "an old answer" "resume a session"

type_ "md"; keys Enter; sleep 1

keys Escape; sleep 0.3
keys ":acp-insert-code" Enter; sleep 0.5
expect "fn main() {}" "insert the last code block"

keys ":acp-focus" Enter; sleep 0.3
type_ "crash"; keys Enter
expect "fake agent: simulated crash" "agent exit shows the log tail"

keys ":acp-switch-agent" Enter; sleep 0.5
expect "Claude Code" "agent picker lists agents"
keys Enter; sleep 1.5
expect "Fake Agent" "switching restarts the agent"

keys Escape; sleep 0.3
keys ":acp-close" Enter; sleep 0.5
if screen | grep -qF "Fake Agent"; then echo "FAIL close hides the panel"; fail=1; else echo "ok   close hides the panel"; fi

if [ -s /tmp/acp-hx-test-stderr.log ]; then echo "FAIL stderr output:"; cat /tmp/acp-hx-test-stderr.log; fail=1; fi
tmux kill-session -t $S
exit $fail
