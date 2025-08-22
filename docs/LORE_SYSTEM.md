```markdown
# 🌍 Next-Level Lore System Specification

## Timeline Visualization
- **Data Additions**: `era`, `date`, `parent_event_id`
- **API**: `GET /lore/timeline?filter=...`
- **UI**: Scrollable timeline with Vis.js

## Interactive Lore Map Widgets
- **Data Additions**: `location` (name/coordinates)
- **API**: `GET /lore/location/:locationName`
- **UI**: World map with hover/click events

## Discussion, Voting & Retcon History
- **Data Additions**: `votes`, `comments`, `history`
- **API**: `POST /lore/:id/vote`, `GET /lore/:id/history`
- **UI**: Vote controls + Retcon Ledger

## Procedural & Player-Generated Lore
- **Data Additions**: `proposed_by`, `status`, `suggested_edits`
- **API**: `POST /lore/propose`, `PATCH /lore/:id/approve`
- **UI**: Proposals dashboard

## Community Sagas & Group Authorship
- **Data Additions**: `saga_id`, `contributors`, `drafts`
- **API**: `GET /lore/saga/:saga_id`
- **UI**: Saga browser with draft comparison

## Personal Lore Collections & Favourites
- **Data Additions**: `user_profile.favourites`, `user_profile.collections`
- **API**: `POST /user/:id/favourite/:lore_id`
- **UI**: "Your Lore" dashboard
```
