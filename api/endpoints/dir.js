```js
// Returns the contents of any given directory path.
const express = require('express');
const app = express();
const fsManifest = require('./dir_manifest.json'); // Mocked or generated manifest

app.get('/api/dir', (req, res) => {
  const path = req.query.path || '/';
  const listing = fsManifest[path] || [];
  res.status(200).json({ path, listing });
});

module.exports = app;
```
