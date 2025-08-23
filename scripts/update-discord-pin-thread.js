/**
 * ALN Codex → Discord Sync
 * - Root pinned message holds part 1
 * - Overflow in a thread, cleared+reposted each sync
 * - Both root + thread are pinned
 * - Thread auto-unarchives (keep-alive)
 */

import fetch from "node-fetch";

const token = process.env.DISCORD_BOT_TOKEN;
const channelId = process.env.DISCORD_CHANNEL_ID;
const messageId = process.env.DISCORD_MESSAGE_ID;
const scroll = process.env.PINNED_SCROLL;

if (!scroll) throw new Error("❌ Missing PINNED_SCROLL content.");

// --- Helpers ---
async function api(url, opts = {}) {
  const res = await fetch(url, {
    ...opts,
    headers: {
      Authorization: `Bot ${token}`,
      "Content-Type": "application/json",
      ...(opts.headers || {}),
    },
  });
  if (!res.ok) throw new Error(`Discord API ${res.status}: ${await res.text()}`);
  return res.json().catch(() => ({}));
}

function chunkText(text, size = 1900) {
  const res = [];
  for (let i = 0; i < text.length; i += size) {
    res.push(text.substring(i, i + size));
  }
  return res;
}

// --- Steps ---
async function updateRoot(content) {
  await api(`https://discord.com/api/v10/channels/${channelId}/messages/${messageId}`, {
    method: "PATCH",
    body: JSON.stringify({ content }),
  });
  console.log("✅ Root scroll updated");
}

async function findThread() {
  const data = await api(`https://discord.com/api/v10/channels/${channelId}/threads/archived/public`);
  // Look among both active+archived
  const candidate = [...(data.threads || [])].find((t) => t.name.startsWith("🧵 ALN Pinned Scroll"));
  if (candidate) console.log(`🔍 Found thread ${candidate.id}`);
  return candidate?.id || null;
}

async function createThread() {
  const thread = await api(`https://discord.com/api/v10/channels/${channelId}/messages/${messageId}/threads`, {
    method: "POST",
    body: JSON.stringify({
      name: "🧵 ALN Pinned Scroll (auto-synced)",
      auto_archive_duration: 10080, // 7 days
    }),
  });
  console.log("🧵 Created thread " + thread.id);
  return thread.id;
}

async function unarchiveThread(threadId) {
  await api(`https://discord.com/api/v10/channels/${threadId}`, {
    method: "PATCH",
    body: JSON.stringify({ archived: false }),
  });
  console.log("⏳ Thread auto-unarchived / refreshed");
}

async function clearThread(threadId) {
  const messages = await api(`https://discord.com/api/v10/channels/${threadId}/messages`);
  for (const msg of messages) {
    await fetch(`https://discord.com/api/v10/channels/${threadId}/messages/${msg.id}`, {
      method: "DELETE",
      headers: { Authorization: `Bot ${token}` },
    });
  }
  console.log("🧹 Cleared old thread content");
}

async function postToThread(threadId, content) {
  await api(`https://discord.com/api/v10/channels/${threadId}/messages`, {
    method: "POST",
    body: JSON.stringify({ content }),
  });
}

async function pinThread(threadId) {
  await fetch(`https://discord.com/api/v10/channels/${threadId}/pins/${messageId}`, {
    method: "PUT",
    headers: { Authorization: `Bot ${token}` },
  });
  console.log("📌 Thread pinned alongside root");
}

async function run() {
  const parts = chunkText(scroll);

  // Update root message
  await updateRoot(parts[0] + `\n\n📌 *(Auto-synced, part 1/${parts.length})*`);

  if (parts.length === 1) return;

  // Ensure thread
  let threadId = await findThread();
  if (!threadId) {
    threadId = await createThread();
  } else {
    await unarchiveThread(threadId);
    await clearThread(threadId);
  }

  // Post continuation chunks
  for (let i = 1; i < parts.length; i++) {
    await postToThread(threadId, `📜 *(Cont’d ${i + 1}/${parts.length})*\n\n${parts[i]}`);
  }

  await pinThread(threadId);
  console.log("✅ Synced + Thread kept alive.");
}

run().catch(console.error);
