/**
 * ALN Codex → Discord Sync
 * Keeps the Discord Pinned Scroll updated with README.md edits.
 *
 * Requirements:
 *   - DISCORD_BOT_TOKEN (bot token in GitHub Secrets)
 *   - DISCORD_CHANNEL_ID (channel where pinned message lives)
 *   - DISCORD_MESSAGE_ID (ID of the pinned scroll message)
 */

import fs from "fs";
import fetch from "node-fetch";

const token = process.env.DISCORD_BOT_TOKEN;
const channelId = process.env.DISCORD_CHANNEL_ID;
const messageId = process.env.DISCORD_MESSAGE_ID;

// Step 1: Load README
const readme = fs.readFileSync("README.md", "utf-8");

// Step 2: Extract Discord Scroll Section
const startMarker = "# 💬 Discord Pinned Scroll";
const endMarker = "---";
const startIndex = readme.indexOf(startMarker);
const endIndex = readme.indexOf(endMarker, startIndex);

if (startIndex === -1 || endIndex === -1) {
  throw new Error("Pinned Scroll section not found in README.md");
}

const scroll = readme
  .substring(startIndex, endIndex)
  .trim();

// Step 3: Update the pinned message
async function updateScroll() {
  const url = `https://discord.com/api/v10/channels/${channelId}/messages/${messageId}`;
  const res = await fetch(url, {
    method: "PATCH",
    headers: {
      "Authorization": `Bot ${token}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      content: scroll.slice(0, 1900) + "\n\n📌 *(Auto-synced from README)*"
    }),
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(`Failed to update Discord message: ${res.status} - ${text}`);
  }

  console.log("✅ Successfully updated Discord pinned scroll");
}

updateScroll().catch(console.error);
