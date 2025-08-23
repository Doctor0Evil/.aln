/**
 * ALN Codex Scroll Index
 * Pinned changelog + index, sorted NEWEST → OLDEST
 * - Uses commit message body as release blurb
 * - Highlights formal GitHub release tags (vX.Y)
 */

import fs from "fs";
import fetch from "node-fetch";

const token = process.env.DISCORD_BOT_TOKEN;
const channelId = process.env.DISCORD_CHANNEL_ID;
const messageId = process.env.DISCORD_INDEX_MESSAGE_ID;
const rootMessageId = process.env.DISCORD_MESSAGE_ID;
const serverId = process.env.DISCORD_SERVER_ID;

const version = process.env.ALN_VERSION || "vX.Y.Z";
const date = new Date().toISOString().split("T")[0];

// Use commit message body if captured by workflow
let commitMessage = process.env.GITHUB_COMMIT_MESSAGE;
if (!commitMessage) {
  commitMessage =
    process.env.GITHUB_EVENT_NAME === "push"
      ? `Commit ${process.env.GITHUB_SHA?.substring(0, 7)} to ${process.env.GITHUB_REF}`
      : "README Scroll updated.";
}

const logPath = ".scroll-hash/scroll-index.json";
let index = [];
if (fs.existsSync(logPath)) {
  index = JSON.parse(fs.readFileSync(logPath, "utf-8"));
}

// Detect if the version looks like a tagged release (vX.Y or vX.Y.Z)
const releaseRegex = /^v\d+(\.\d+){1,2}$/;
const isRelease = releaseRegex.test(version);

// Ensure entry exists
if (!index.find(e => e.version === version)) {
  index.push({
    version,
    date,
    link: `https://discord.com/channels/${serverId}/${channelId}/${rootMessageId}`,
    blurb: commitMessage,
    release: isRelease,
  });
  fs.mkdirSync(".scroll-hash", { recursive: true });
  fs.writeFileSync(logPath, JSON.stringify(index, null, 2));
}

// Sort NEWEST → OLDEST
index.sort((a, b) => new Date(b.date) - new Date(a.date));

// Render index into Discord message
function render(entries) {
  return [
    "📚 **ALN Scroll Index + Changelog (Newest → Oldest)**",
    "",
    ...entries.map((e) => {
      const icon = e.release ? "👑" : "•";
      return `${icon} **${e.version}** — ${e.date}\n  ↪ [Jump](${e.link})\n  └ ${e.blurb}`;
    }),
  ].join("\n");
}

async function updateIndex(content) {
  const url = `https://discord.com/api/v10/channels/${channelId}/messages/${messageId}`;
  const res = await fetch(url, {
    method: "PATCH",
    headers: { Authorization: `Bot ${token}`, "Content-Type": "application/json" },
    body: JSON.stringify({ content }),
  });
  if (!res.ok) throw new Error(`❌ Failed index update: ${res.status} - ${await res.text()}`);
  console.log("✅ Scroll Index Updated");
}

updateIndex(render(index)).catch(console.error);
