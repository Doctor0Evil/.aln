```js
const express = require('express');
const app = express();
const http = require('http').Server(app);
const io = require('socket.io')(http, { cors: { origin: "*" } });
const manifest = require('./dir_manifest.json');

app.use(express.json());

let presence = {}; // path → [users]

/* Fuzzy string match utility (simple version) */
function fuzzyMatch(input, options) {
  input = input.toLowerCase();
  let scored = options.map(opt => {
    let score = 0;
    if (opt === input) score = 100;
    else if (opt.includes(input)) score = 75;
    else score = -Math.abs(opt.length - input.length);
    return { opt, score };
  });
  scored.sort((a,b) => b.score - a.score);
  return scored[0].score > 50 ? scored.opt : null;
}

// Directory "ls"
app.get('/api/dir', (req, res) => {
  const path = req.query.path || '/';
  const listing = manifest[path] || [];
  res.status(200).json({ path, listing });
});

// Fuzzy cd/ls
app.get('/api/nav', (req, res) => {
  const input = req.query.input;
  if (!input) return res.status(400).send("No input.");
  if (manifest[input]) return res.json({ path: input, listing: manifest[input] });
  // Fuzzy match fallback
  let best = fuzzyMatch(input, Object.keys(manifest));
  if (best) return res.json({ path: best, listing: manifest[best], corrected: true });
  return res.status(404).send("No matching path.");
});

// Notify/open a dir (broadcast)
app.post('/api/broadcast/open', (req, res) => {
  const { user, path, platform } = req.body;
  io.emit('dir_open', { user, path, platform, time: Date.now() });
  // Track presence
  if (!presence[path]) presence[path] = [];
  if (!presence[path].includes(user)) presence[path].push(user);
  res.json({ status: "broadcasted" });
});

// Sync event layer; could broadcast or trigger hooks
app.post('/api/sync/event', (req, res) => {
  io.emit('sync_event', req.body);
  res.json({ status: "synced" });
});

// Who is present at a path
app.get('/api/presence', (req, res) => {
  const path = req.query.path || '/';
  res.json(presence[path] || []);
});

http.listen(3030, () => console.log('API/server on :3030'));
module.exports = app;
```
