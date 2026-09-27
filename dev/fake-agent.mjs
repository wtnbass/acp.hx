// Scripted ACP agent for exercising acp.hx without a model.
// Each prompt plays the scenario named by its first word (default: "all").
import readline from "node:readline";

const out = (msg) => process.stdout.write(JSON.stringify({ jsonrpc: "2.0", ...msg }) + "\n");
const update = (sessionId, update) => out({ method: "session/update", params: { sessionId, update } });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const pending = new Map();
let nextId = 1;
const request = (method, params) =>
  new Promise((resolve) => {
    const id = `fake-${nextId++}`;
    pending.set(id, resolve);
    out({ id, method, params });
  });

let configOptions = [
  { id: "mode", name: "Mode", category: "mode", type: "select", currentValue: "default",
    options: [{ value: "default", name: "Manual" }, { value: "plan", name: "Plan", _meta: { kind: "plan" } }] },
  { id: "model", name: "Model", category: "model", type: "select", currentValue: "fake",
    options: [{ value: "fake", name: "Fake 1.0" }, { value: "faster", name: "Fake Turbo" }] },
];

async function scenario(sid, name) {
  if (name === "plan" || name === "all") {
    const entries = ["Read the code", "Write the patch", "Run the tests"].map((content) => ({ content, priority: "medium", status: "pending" }));
    for (let i = 0; i <= entries.length; i++) {
      update(sid, { sessionUpdate: "plan", entries: entries.map((e, j) => ({ ...e, status: j < i ? "completed" : j === i ? "in_progress" : "pending" })) });
      await sleep(150);
    }
  }
  if (name === "tools" || name === "all") {
    update(sid, { sessionUpdate: "agent_thought_chunk", content: { type: "text", text: "Thinking about the change.\nLine two of thought." } });
    update(sid, { sessionUpdate: "tool_call", toolCallId: "t1", title: "Read src/main.rs", kind: "read", status: "pending" });
    update(sid, { sessionUpdate: "tool_call_update", toolCallId: "t1", status: "completed",
      content: [{ type: "content", content: { type: "text", text: "```\nfn main() {\n    println!(\"hi\");\n}\n```" } }] });
    update(sid, { sessionUpdate: "tool_call", toolCallId: "t2", title: "Edit src/main.rs", kind: "edit", status: "pending",
      content: [{ type: "diff", path: "/tmp/src/main.rs", oldText: "fn main() {\n    println!(\"hi\");\n}", newText: "fn main() {\n    println!(\"hello\");\n}" }] });
    const answer = await request("session/request_permission", {
      sessionId: sid,
      toolCall: { toolCallId: "t2", title: "Edit src/main.rs",
        content: [{ type: "diff", path: "/tmp/src/main.rs", oldText: "    println!(\"hi\");", newText: "    println!(\"hello\");" }] },
      options: [{ optionId: "allow", name: "Yes", kind: "allow_once" }, { optionId: "reject", name: "No", kind: "reject_once" }],
    });
    const ok = answer.outcome?.optionId === "allow";
    update(sid, { sessionUpdate: "tool_call_update", toolCallId: "t2", status: ok ? "completed" : "failed" });
    update(sid, { sessionUpdate: "tool_call", toolCallId: "t3", title: "cargo test", kind: "execute", status: "failed",
      content: [{ type: "content", content: { type: "text", text: "```console\nerror[E0425]: cannot find value `x`\n```" } }] });
  }
  if (name === "md" || name === "all") {
    const text = "## Summary\n\nChanged **one** line in `main.rs`, see [docs](https://example.com).\n\n- first *point*\n- second point\n\n```rust\nfn main() {}\n```\n";
    for (const chunk of text.match(/.{1,12}/gs)) {
      update(sid, { sessionUpdate: "agent_message_chunk", content: { type: "text", text: chunk } });
      await sleep(20);
    }
  }
  update(sid, { sessionUpdate: "usage_update", used: 51234, size: 200000, cost: { amount: 0.4321, currency: "USD" } });
  update(sid, { sessionUpdate: "session_info_update", title: `Fake session (${name})` });
}

readline.createInterface({ input: process.stdin }).on("line", async (line) => {
  const msg = JSON.parse(line);
  if (msg.id !== undefined && !msg.method) {
    pending.get(msg.id)?.(msg.result ?? {});
    pending.delete(msg.id);
    return;
  }
  const { id, method, params } = msg;
  if (method === "initialize") out({ id, result: { protocolVersion: 1, agentInfo: { name: "fake", title: "Fake Agent", version: "0" }, agentCapabilities: {} } });
  else if (method === "session/new") {
    out({ id, result: { sessionId: "fake-session", configOptions } });
    update("fake-session", { sessionUpdate: "available_commands_update", availableCommands: [{ name: "review", description: "Review the diff" }, { name: "init", description: "Create CLAUDE.md" }] });
  } else if (method === "session/set_config_option") {
    configOptions = configOptions.map((o) => (o.id === params.configId ? { ...o, currentValue: params.value } : o));
    out({ id, result: { configOptions } });
  } else if (method === "session/list") out({ id, result: { sessions: [{ sessionId: "old", title: "An old session", updatedAt: "2026-09-01T10:00:00Z" }] } });
  else if (method === "session/load") {
    update(params.sessionId, { sessionUpdate: "user_message_chunk", content: { type: "text", text: "an old question" } });
    update(params.sessionId, { sessionUpdate: "agent_message_chunk", content: { type: "text", text: "an old answer" } });
    out({ id, result: { configOptions } });
  } else if (method === "session/prompt") {
    const text = params.prompt.find((b) => b.type === "text")?.text ?? "";
    const links = params.prompt.filter((b) => b.type !== "text").map((b) => b.uri ?? b.resource?.uri);
    if (links.length) update(params.sessionId, { sessionUpdate: "agent_message_chunk", content: { type: "text", text: `attachments: ${links.join(", ")}\n\n` } });
    await scenario(params.sessionId, text.split(/\s+/)[0] || "all");
    out({ id, result: { stopReason: "end_turn" } });
  } else if (method === "session/cancel") {
  } else if (id !== undefined) out({ id, error: { code: -32601, message: `unknown ${method}` } });
});
