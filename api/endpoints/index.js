// api/endpoints/index.js
const express = require('express');
const app = express();

app.use(express.json());

// POST /log
app.post('/log', (req, res) => {
  const { user, action, details, time } = req.body;
  // Save to DB or stream to event log
  console.log(`[LOG] ${user} -> ${action}: ${details}`);
  res.status(200).json({ status: 'logged' });
});

// POST /save_player
app.post('/save_player', (req, res) => {
  const { username, ...data } = req.body;
  // Save to database
  console.log(`💾 Saved player: ${username}`);
  res.status(200).json({ saved: true });
});

// GET /load_player
app.get('/load_player', (req, res) => {
  const { user } = req.query;
  // Fetch from DB
  const player = { /* mock data */ };
  res.status(200).json(player);
});

module.exports = app;
