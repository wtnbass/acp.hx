# acp.hx

helix (steel plugin) から [ACP (Agent Client Protocol)](https://agentclientprotocol.com) のエージェントを起動し、右サイドバーでチャットする PoC。
デフォルトのエージェントは `npx -y @agentclientprotocol/claude-agent-acp`（Claude Code）。

## 試す

```sh
HELIX_STEEL_CONFIG=$PWD/dev hx
```

`:acp-open` でパネルを開く。

| キー (パネル focus 中) | 動作 |
| --- | --- |
| 文字入力 / Enter | プロンプト送信 |
| Esc | エディタに focus を戻す（パネルは残る） |
| Ctrl-c | 実行中のターンをキャンセル (`session/cancel`) |
| Ctrl-u | 入力をクリア |
| PageUp / PageDown | スクロール |
| 1-9 / Esc | permission リクエストに回答 / キャンセル |

コマンド: `:acp-open` `:acp-focus` `:acp-close` `:acp-toggle` `:acp-cancel` `:acp-follow-toggle`

設定例 (`init.scm`):

```scheme
(require "acp/acp.scm")
(acp-configure! #:width 70 #:command "npx -y @agentclientprotocol/claude-agent-acp")
```

エージェントの stderr は `/tmp/acp-hx.log` に出る。

## follow-along

エージェントが読んだり編集したりしているファイルを、エディタ側で自動的に開いて該当行へ移動する（デフォルトで有効、パネル見出しに `· follow` と表示される）。

- `tool_call` / `tool_call_update` の `locations` の先頭を `:open` し、`line` があれば `:goto` して画面中央に寄せる
- ワークスペース外のパス（Claude のメモリファイルなど）は追わない
- ツールが `completed` になったら、`locations` のファイルが開いていて未保存の変更が無ければ reload する（エージェントの編集をバッファに反映するため）
- `:acp-follow-toggle` か `(acp-configure! #:follow 'off)` で無効化

## 構成

- 子プロセスの stdout を `spawn-native-thread` 上で `read-line-from-port` し、`hx.with-context` でメインスレッドに渡して状態を更新する
- パネルは forest と同じく bg component（描画のみ、イベントは素通し）と fg component（focus 中だけ push してキーを受ける）の 2 枚構成。`set-editor-clip-right!` でエディタ領域を縮める

## 対応している ACP の範囲

- `initialize` → `session/new` → `session/prompt`
- `session/update`: `agent_message_chunk` / `agent_thought_chunk` / `tool_call` / `tool_call_update` / `plan`
- `session/request_permission`
- `tool_call.locations`（follow-along）
- `fs/*` と `terminal/*` は capability を false で宣言し、未対応

## ハマりどころ

- `write-line!` は文字列を quote 付きで書くので `write-string` を使う
- `string->jsexpr` は JSON の数値をすべて float にするため、リクエスト ID は文字列にしている
