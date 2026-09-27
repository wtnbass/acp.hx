# acp.hx

helix (steel plugin) の右サイドバーで Claude Code と対話するプラグイン。
[ACP (Agent Client Protocol)](https://agentclientprotocol.com) のアダプタ `@agentclientprotocol/claude-agent-acp` を子プロセスとして起動し、JSON-RPC で話す。
Claude Code 専用で、ほかの ACP エージェントでの動作は確認していない。

```
│ ✻ Claude Agent  Rust 入門 Markdown 作成
│ ⏵⏵ Manual  ◆ Opus 5.5  ◇ Xhigh  ◉ follow
│ ctx 34k/1.0M 3%  $0.22  5h 36%  7d 14%
│ ──────────────────────────────────────────
│ ❯ sample.txt の line 20 を置換して
│ ⏺ Edit sample.txt
│   ⎿ sample.txt  +1 -1
│     - line 20
│     + LINE TWENTY
│ ⏺ 置換しました。
│ ✳ Working… (3s · ^c to interrupt)
│ ──────────────────────────────────────────
│ ❯ @README.md を要約して
│ ──────────────────────────────────────────
│ ⏎ send · ⇧⇥ mode · ^o settings · ^r sessions …
```

## 試す

```sh
HELIX_STEEL_CONFIG=$PWD/dev hx
```

`:acp-open` でパネルを開く。

- `dev/check.sh`: 同じ設定を tmux で起動し、steel の読み込みエラーがあれば表示する
- `dev/test.sh`: 台本どおりに振る舞う偽エージェント `dev/fake-agent.mjs` を相手に、plan・permission・diff・usage・ピッカー・セッション再開などを tmux 越しに確認する
- `ACP_HX_AGENT="node dev/fake-agent.mjs" HELIX_STEEL_CONFIG=$PWD/dev hx` で偽エージェントを手で触れる（プロンプトの先頭語 `plan` / `tools` / `md` / `all` で場面を選ぶ）

## 普段の設定に入れる

```sh
ln -s ~/ghq/github.com/wtnbass/acp.hx ~/.local/share/steel/cogs/acp
```

`init.scm`:

```scheme
(require "acp/acp.scm")
(acp-configure! #:width 64)
```

`config.toml` のキー割り当て例:

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

## パネル内のキー

| キー | 動作 |
| --- | --- |
| Enter | 送信（補完候補が出ていれば確定） |
| Alt-Enter / Ctrl-j | 改行 |
| Esc | エディタに focus を戻す（パネルは残る） |
| Ctrl-c | 実行中のターンを中断 / 入力をクリア |
| Shift-Tab | mode を順に切り替え（Manual → Accept edits → Plan → Auto） |
| Ctrl-p | アクション一覧（すべての操作とキー割り当て） |
| Ctrl-o | 設定ピッカー（mode / model / effort / fast） |
| Ctrl-r | 過去のセッションを選んで再開 |
| Ctrl-n | 新しいセッション |
| Ctrl-t | ツール出力・diff・thinking の全文表示を切り替え |
| Ctrl-f | follow-along の切り替え |
| `/` | スラッシュコマンドの補完（Tab / Enter で確定） |
| `@` | ワークスペースのファイル補完。送信時に `resource_link` として添付 |
| ↑ / ↓ | 補完候補の選択、または入力履歴 |
| ← → Home End Ctrl-a Ctrl-e Ctrl-w Ctrl-u | 行編集 |
| PageUp / PageDown・ホイール | スクロール |

permission のリクエスト中は ↑↓ / 数字キーで選択、Enter で確定、`d` で diff 全体や計画の本文を開く、Esc で拒否。

トランスクリプトのツール呼び出しや thinking の見出し行をクリックすると、その項目だけ全文表示を切り替える。ツール呼び出しの本文の行をクリックすると、そのツールが触ったファイルの該当行を開く。

## コマンド

| コマンド | 動作 |
| --- | --- |
| `:acp-open` / `:acp-focus` / `:acp-close` / `:acp-toggle` | パネルの表示と focus |
| `:acp-menu` | アクション一覧を開く |
| `:acp-new-session` / `:acp-sessions` | 新規セッション / 過去のセッションを再開 |
| `:acp-settings` / `:acp-mode` / `:acp-model` / `:acp-effort` / `:acp-cycle-mode` | 設定の変更 |
| `:acp-add-file` / `:acp-add-selection` | 現在のファイル / 選択範囲を次のプロンプトに添付し、パネルに focus を移す |
| `:acp-add-image <path>` | 画像（png / jpg / gif / webp）を次のプロンプトに添付 |
| `:acp-diff` | 保留中の permission の diff / 計画、または直近の編集の diff を開く |
| `:acp-review` | このセッションでエージェントが行った編集をまとめて 1 つの diff で開く |
| `:acp-undo-edit` | 直近の編集を元に戻す（編集後のテキストがファイル中に 1 か所だけある場合）。戻したことは次のプロンプトでエージェントに伝える |
| `:acp-yank` | 直近の返答をクリップボードにコピー |
| `:acp-insert-code` | 直近の返答の最後のコードブロックを選択範囲の後ろに貼り付け |
| `:acp-cancel` | 実行中のターンを中断 |
| `:acp-retry` | 直前のプロンプトを送り直す |
| `:acp-follow-toggle` / `:acp-expand-toggle` | follow-along / 全文表示の切り替え |
| `:acp-wider` / `:acp-narrower` | パネル幅の変更 |
| `:acp-restart` / `:acp-quit` | エージェントの再起動 / 停止 |

`acp-configure!` のオプション: `#:command`（`claude-agent-acp` の起動コマンド。グローバルにインストールした場合などに変える）、`#:width`、`#:log`（エージェントの stderr の出力先、既定は `/tmp/acp-hx.log`）、`#:follow`（`'on` / `'off`）、`#:mcp-servers`（各セッションに渡す ACP の McpServer のリスト）。

## 表示している情報

- ヘッダー: エージェント名、セッションタイトル（`session_info_update`）
- 1 行目: mode・model・effort・fast mode（`configOptions`）、follow の状態
- 2 行目: context の使用量と割合、セッションのコスト、5 時間 / 7 日のレート制限の使用率（`usage_update`）
- 本文: ユーザー入力、Markdown で整形した返答、thinking、ツール呼び出し（状態の色、diff、出力のプレビュー）、plan のチェックリスト

## Zed / VS Code の Claude 連携との対応

| 機能 | acp.hx |
| --- | --- |
| ストリーミング表示・Markdown（見出し・リスト・表・コード・リンク） | ✓ |
| ツール呼び出しの表示（状態・diff・出力）と個別の展開 | ✓ |
| 編集・コマンド実行の許可（diff プレビュー付き） | ✓ |
| Plan モードの計画承認 | ✓（計画本文は `d` で全文表示） |
| mode / model / effort / fast mode の表示と変更 | ✓ |
| context 使用量・コスト・レート制限の表示 | ✓ |
| スラッシュコマンド・`@` ファイル・選択範囲・画像の添付 | ✓ |
| セッションの新規作成・再開 | ✓ |
| エージェントの作業位置の追従（follow-along） | ✓ |
| 編集のまとめ表示・直近の編集の取り消し | ✓（`:acp-review` / `:acp-undo-edit`） |
| 中断・プロンプトのキューイング・再送 | ✓ |
| MCP サーバーの指定 | ✓ |
| 過去メッセージの編集・チェックポイントへの巻き戻し | ✗（ACP に該当する仕組みがない） |
| エディタ内でのハンク単位の accept / reject | ✗（`:acp-review` の diff 表示と `:acp-undo-edit` で代替） |
| Bash 出力の逐次表示 | ✗（アダプタが完了時にまとめて送るため） |

## follow-along

エージェントが読んだり編集したりしているファイルを、エディタ側で自動的に開いて該当行へ移動する。

- `tool_call` / `tool_call_update` の `locations` の先頭を `:open` し、`line` があれば `:goto` して画面中央に寄せる
- ワークスペース外のパス（Claude のメモリファイルなど）は追わない
- ツールが `completed` になったら、`locations` のファイルが開いていて未保存の変更が無ければ reload する

## 構成

- 子プロセスの stdout を `spawn-native-thread` 上で `read-line-from-port` し、`hx.with-context` でメインスレッドに渡して状態を更新する
- パネルは forest と同じく bg component（描画、ホイール、クリック）と fg component（focus 中だけ push してキーを受ける）の 2 枚構成。`set-editor-clip-right!` でエディタ領域を縮める
- ピッカーは別 component をパネルの上に重ねる
- 返答は Markdown の行単位で折り返し結果をキャッシュし、ストリーミング中は最後の行だけ組み直す

## 対応している ACP の範囲

- `initialize` / `session/new` / `session/load` / `session/list` / `session/prompt` / `session/cancel` / `session/set_config_option`
- `session/update`: `agent_message_chunk` / `agent_thought_chunk` / `user_message_chunk` / `tool_call` / `tool_call_update` / `plan` / `config_option_update` / `current_mode_update` / `available_commands_update` / `usage_update` / `session_info_update`
- `session/request_permission`
- プロンプトの content: `text` / `resource_link`（`@` とファイル添付） / `resource`（選択範囲） / `image`
- `fs/*` と `terminal/*` は capability を false で宣言し、未対応

エージェントが終了したときは、stderr のログの末尾 3 行をパネルに表示する。`session/new` が認証エラー（-32000）を返したときは、エージェント側の CLI でログインするよう案内する。

## ハマりどころ

- `write-line!` は文字列を quote 付きで書くので `write-string` を使う
- `string->jsexpr` は JSON の数値をすべて float にするため、リクエスト ID は文字列にしている
- `helix/static.scm` が `range` を export しているので、同名の関数は使えない
- `:new` で作ったバッファに `insert_string` した後で閉じると helix が panic するので、diff は一時ファイル（`/tmp/acp-hx/*.diff`）に書いて開く
- steel の読み込みエラーは起動時のステータスに一瞬出るだけなので、`dev/check.sh` で確認する
