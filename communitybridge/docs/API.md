```markdown
## IOM API Reference

### Submit Memory
`POST /api/memory`
```
{
  "memory": "misheard lyric, 90s ad jingle",
  "tags": ["jingle", "90s"],
  "meta": {...},
  "files": []
}
```

### Suggest Match
`POST /api/memory/{id}/match`
```
{
  "url": "...",
  "details": "...",
  "contributor": "user42"
}
```

### Confirm Match (OP only)
`POST /api/memory/{id}/confirm`
```
{ "match_index": 1, "confirmer": "OP_user" }
```

### Query/Search + AI
`GET /api/query?q=clouds+rolling+by`
- Returns YAML/JSON array of matches or `ai_suggestion`

---
