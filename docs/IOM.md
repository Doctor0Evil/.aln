```markdown
# Internet Archive of Music (IOM)

- **Concept**: A massive data pool where **players archive & recover lost cultural content** (music, videos, lore fragments).  
- **Process**:  
  1. User requests a missing piece.  
  2. Community “flags” with upvotes when correct.  
  3. Entry becomes permanent in the **IOM Codex Vault**.  

ALN Structure:  

```aln
@IOM {
  request: "Lost 1980s synthwave track"
  status: "open"
  contributions: [
    {user: "scribe42", link: "...", votes: 5},
    {user: "bard14", link: "...", votes: 12}
  ]
  resolved: true
  archivePath: /IOM/1980s/Synthwave/Tracks
}
```
