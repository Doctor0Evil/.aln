// apiendpoints/lore.js

const express = require('express');
const router = express.Router();

// Timeline endpoint
router.get('/timeline', (req, res) => {
  // Generate timeline data from LORE system
  const timeline = LoreComposer_generate_timeline_json();
  res.status(200).json(timeline);
});

// Location-based lore
router.get('/location/:location', (req, res) => {
  const location = req.params.location;
  const entries = LoreComposer_lore_by_location(location);
  res.status(200).json(entries);
});

// Vote endpoint
router.post('/:id/vote', (req, res) => {
  const { lore_id, up } = req.body;
  LoreComposer_vote(lore_id, up);
  res.status(200).json({ status: 'voted' });
});

module.exports = router;
