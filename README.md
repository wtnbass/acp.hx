# acp.hx

An [Agent Client Protocol (ACP)](https://agentclientprotocol.com) client for [helix](https://helix-editor.com), written as a steel plugin. It runs a coding agent as a child process and gives you a chat sidebar on the right of the editor.

**Currently only Claude Code is supported**, through the [`@agentclientprotocol/claude-agent-acp`](https://www.npmjs.com/package/@agentclientprotocol/claude-agent-acp) adapter. Other ACP agents are untested.

This is an unofficial project and is not affiliated with Anthropic.

```
│ ✻ Claude Agent  Fix the greeting
│ ⏵⏵ Manual  ◆ Opus 5.5  ◇ Xhigh  ◉ follow
│ ctx ▰▱▱▱▱▱ 34k/1.0M 3%  $0.22  5h 36%  7d 14%
│ ──────────────────────────────────────────
│ ❯ change "hi" to "hello" in @src/main.rs
│ ⏺ Edit src/main.rs
│   ⎿ src/main.rs  +1 -1
│     - println!("hi");
│     + println!("hello");
│ ⏺ Done. The greeting now prints "hello".
│ ✳ Working… (3s · ^c to interrupt)
│ ──────────────────────────────────────────
│ ❯ Message the agent… (/ for commands)
│ ──────────────────────────────────────────
│ ⏎ send · ^p actions · ⇧⇥ mode · ^o settings …
```

## Requirements

- helix built with the steel plugin system ([mattwparas/helix, `steel-event-system` branch](https://github.com/mattwparas/helix/tree/steel-event-system))
- Node.js (`npx` fetches the adapter on first start)
- Claude Code, logged in (run `claude` and `/login` once)
- `git` for `@` file completion (falls back to `find` outside a git repository)

## Installation

Link the repository into your steel cogs directory:

```sh
ln -s /path/to/acp.hx ~/.local/share/steel/cogs/acp
```

In `init.scm`:

```scheme
(require "acp/acp.scm")
(acp-configure! #:width 64)
```

Example key bindings in `config.toml`:

```toml
[keys.normal.space]
a = ":acp-toggle"

[keys.normal.space.A]
f = ":acp-add-file"
d = ":acp-diff"
m = ":acp-mode"
s = ":acp-sessions"

[keys.select.space]
a = ":acp-add-selection"
```

Then run `:acp-open`.

### Options

`acp-configure!` accepts:

| Option | Default | Description |
| --- | --- | --- |
| `#:command` | `npx -y @agentclientprotocol/claude-agent-acp` | Command that starts the adapter. Point it at a global install to skip the `npx` startup. |
| `#:width` | `64` | Sidebar width in columns |
| `#:log` | `/tmp/acp-hx.log` | File that receives the agent's stderr |
| `#:follow` | `'on` | `'on` / `'off`: whether the editor follows the files the agent touches |
| `#:mcp-servers` | `'()` | ACP `McpServer` entries passed to every session |

## Keys in the panel

| Key | Action |
| --- | --- |
| Enter | Send (or accept the highlighted completion) |
| Alt-Enter / Ctrl-j | Newline |
| Esc | Back to the editor; the panel stays open |
| Ctrl-c | Interrupt the running turn, or clear the input |
| Shift-Tab | Cycle the mode (Manual → Accept edits → Plan → Auto) |
| Ctrl-p | Action menu: every action with its key |
| Ctrl-o | Settings: mode, model, effort, fast mode |
| Ctrl-r | Resume an earlier session |
| Ctrl-n | New session |
| Ctrl-t | Show tool output, diffs and thinking in full |
| Ctrl-f | Toggle follow-along |
| `/` | Slash command completion (Tab / Enter to accept) |
| `@` | Workspace file completion; mentioned files are sent as `resource_link` |
| ↑ / ↓ | Move through completions, or through input history |
| ← → Home End Ctrl-a Ctrl-e Ctrl-w Ctrl-u | Line editing |
| PageUp / PageDown, mouse wheel | Scroll |

While a permission request is pending: ↑ ↓ or number keys to choose, Enter to confirm, `d` to open the full diff or plan, Esc to reject.

Clicking the header row of a tool call or a thought expands or collapses it. Clicking the body of a tool call opens the file it touched at the right line.

## Commands

| Command | Action |
| --- | --- |
| `:acp-open` / `:acp-focus` / `:acp-close` / `:acp-toggle` | Show, focus or hide the panel |
| `:acp-menu` | Open the action menu |
| `:acp-new-session` / `:acp-sessions` | Start a new session / resume an earlier one |
| `:acp-settings` / `:acp-mode` / `:acp-model` / `:acp-effort` / `:acp-cycle-mode` | Change session settings |
| `:acp-add-file` / `:acp-add-selection` | Attach the current file / selection to the next prompt and focus the panel |
| `:acp-add-image <path>` | Attach an image (png, jpg, gif, webp) to the next prompt |
| `:acp-diff` | Open the pending permission's diff or plan, or the latest edit's diff |
| `:acp-review` | Open every edit the agent made in this session as one diff |
| `:acp-undo-edit` | Revert the latest edit (when its new text occurs exactly once in the file); the agent is told with the next prompt |
| `:acp-yank` | Copy the last response to the clipboard |
| `:acp-insert-code` | Paste the last code block of the last response after the selection |
| `:acp-cancel` | Interrupt the running turn |
| `:acp-retry` | Send the last prompt again |
| `:acp-follow-toggle` / `:acp-expand-toggle` | Toggle follow-along / full output |
| `:acp-wider` / `:acp-narrower` | Resize the panel |
| `:acp-restart` / `:acp-quit` | Restart / stop the agent |

## What the panel shows

- **Header**: agent name and session title
- **Status line**: mode, model, effort, fast mode and follow-along
- **Usage line**: context usage, session cost, and the 5-hour / 7-day rate limit usage
- **Transcript**: your prompts, markdown responses (headings, lists, tables, code, links), thinking, tool calls with status colors, diffs and output previews, and the task plan as a checklist

## Follow-along

The editor opens the files the agent reads or edits and jumps to the line it is working on.

- The first entry of a tool call's `locations` is opened with `:open`, and `:goto` is used when a line is given
- Paths outside the workspace (such as Claude's memory files) are ignored
- When a tool call completes, its files are reloaded if they are open and have no unsaved changes

## Feature coverage

Compared with the Claude integrations in Zed and VS Code:

| Feature | Status |
| --- | --- |
| Streaming markdown responses | ✓ |
| Tool calls with status, diffs and output, expandable one by one | ✓ |
| Permission prompts with a diff preview | ✓ |
| Plan mode approval | ✓ (`d` opens the full plan) |
| Show and change mode, model, effort and fast mode | ✓ |
| Context usage, cost and rate limits | ✓ |
| Slash commands, `@` files, selections and images as context | ✓ |
| New and resumed sessions | ✓ |
| Follow the agent through the code | ✓ |
| Review all edits, undo the latest one | ✓ (`:acp-review` / `:acp-undo-edit`) |
| Interrupt, queue and retry prompts | ✓ |
| MCP servers | ✓ |
| Edit past messages, restore checkpoints | ✗ (ACP has no equivalent) |
| Accept or reject individual hunks in the editor | ✗ (use `:acp-review` and `:acp-undo-edit`) |
| Live streaming of shell output | ✗ (the adapter sends it when the command finishes) |

## Development

```sh
HELIX_STEEL_CONFIG=$PWD/dev hx
```

`dev/init.scm` loads the plugin from this checkout, so your own helix config is left alone.

- `dev/check.sh` starts helix with the dev config in tmux and prints any steel load error
- `dev/test.sh` runs end-to-end checks through tmux against `dev/fake-agent.mjs`, a scripted agent that plays plans, permissions, diffs, markdown, usage, sessions and a crash without calling a model
- `ACP_HX_AGENT="node dev/fake-agent.mjs" HELIX_STEEL_CONFIG=$PWD/dev hx` lets you drive the fake agent by hand; the first word of a prompt (`plan`, `tools`, `md`, `all`, `crash`) picks the scenario

### How it works

- The adapter's stdout is read line by line on a `spawn-native-thread`, and every message hops to the main thread through `hx.with-context`
- The panel is two components, like forest: a background one that draws and handles the wheel and clicks, and a foreground one pushed only while the panel has focus to take keys. `set-editor-clip-right!` shrinks the editor area
- Pickers are another component drawn over the panel
- Markdown is wrapped line by line and memoized, so a streaming message only re-wraps its last line

### ACP surface

- `initialize`, `session/new`, `session/load`, `session/list`, `session/prompt`, `session/cancel`, `session/set_config_option`
- `session/update`: `agent_message_chunk`, `agent_thought_chunk`, `user_message_chunk`, `tool_call`, `tool_call_update`, `plan`, `config_option_update`, `current_mode_update`, `available_commands_update`, `usage_update`, `session_info_update`
- `session/request_permission`
- Prompt content: `text`, `resource_link`, `resource` (selections), `image`
- `fs/*` and `terminal/*` are declared unsupported in the client capabilities

When the agent exits, the last lines of its stderr log are shown in the panel. An authentication error (-32000) from `session/new` comes with a hint to log in through the `claude` CLI.

### Gotchas

- `write-line!` prints strings with quotes, so messages are written with `write-string`
- `string->jsexpr` turns every JSON number into a float, so request ids are strings
- `helix/static.scm` exports `range`, which shadows any function of that name
- Closing a buffer created with `:new` and filled with `insert_string` panics helix, so diffs are written to temporary files under `/tmp/acp-hx/` and opened from there
- Steel load errors only flash in the status line at startup; use `dev/check.sh` to see them
