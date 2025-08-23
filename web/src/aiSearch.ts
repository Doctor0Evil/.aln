```typescript
export async function aiSearch(query: string) {
  // Connect to your real AI/Perplexity API, fallback to local mock if offline
  return fetch(`/api/aihelper?q=${encodeURIComponent(query)}`)
    .then(r=>r.json())
}
```
