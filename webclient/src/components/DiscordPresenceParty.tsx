```tsx
import React, { useState, useEffect } from "react";
// Simulate fetch from API or discord.js presence REST/WebSocket endpoint
const DISCORD_PARTY_API = "/api/discord/presence";

export function DiscordPresenceParty() {
  const [active, setActive] = useState([]);
  const [party, setParty] = useState([]);

  useEffect(() => {
    // Poll or subscribe
    async function update() {
      // Replace fetch with WebSocket for full real-time sync
      const data = await fetch(DISCORD_PARTY_API).then(r => r.json());
      setActive(data.activeUsers || []);
      setParty(data.currentParty || []);
    }
    update();
    const t = setInterval(update, 7000);
    return () => clearInterval(t);
  }, []);

  return (
    <div className="discord-presence">
      <h3>🎤 Discord Live Party Room</h3>
      <p>
        <b>Active detectives:</b> {active.length === 0 ? "None" : active.join(", ")}
      </p>
      <p>
        <b>Current Hunt Party:</b>{" "}
        {party.length === 0 ? "Not in session" : party.join(", ")}{" "}
        <button onClick={() => alert("Party join not implemented in stub!")}>Join Party</button>
      </p>
      <a
        href="https://discord.gg/YOUR_DISCORD_INVITE"
        target="_blank"
        rel="noopener noreferrer"
        style={{ fontWeight: "bold", color: "#5865F2" }}
      >
        Join our Discord!
      </a>
    </div>
  );
}
```
