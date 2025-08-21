```js
const express = require("express");
const app = express();
app.use(express.json());

// Logs any action in-game (explore, roll, vote, cast, etc.)
app.post("/log", (req, res) => {
  // Save log to DB or stream to event log
  res.status(200).json({ status: "logged" });
});

// Save a player's full state
app.post("/saveplayer", (req, res) => {
  // Save player data
  res.status(200).json({ saved: true });
});

// Load player state by username
app.get("/loadplayer", (req, res) => {
  // Fetch player from DB
  res.status(200).json({ player: { username: "Alice", ... } });
});

// Merchant Catalog (item lookup)
app.get("/item/:id", (req, res) => {
  // Lookup item and return detail
});

// List items by filter (e.g., category, rarity)
app.get("/items", (req, res) => {
  // Return filtered list
});

module.exports = app;
```
