#!/bin/sh
# Launch hx with the dev config in tmux and print any steel load error.
cd "$(dirname "$0")/.."
tmux kill-session -t acphx 2>/dev/null
tmux new-session -d -s acphx -x "${COLS:-200}" -y "${ROWS:-50}" "cd $PWD && HELIX_STEEL_CONFIG=$PWD/dev hx $*"
sleep 3
tmux capture-pane -t acphx -p | grep -A8 "error\[" || echo "loaded ok"
