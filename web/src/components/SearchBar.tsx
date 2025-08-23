```tsx
import React, { useState } from "react";
import { aiSearch } from "../api/iom-api";

export function SearchBar() {
  const [q, setQ] = useState("");
  const [results, setResults] = useState(null);
  const [aiResult, setAIResult] = useState(null);

  async function doSearch() {
    setResults(null); setAIResult(null);
    const human = await fetch(`/api/query?q=${encodeURIComponent(q)}`).then(r=>r.json());
    setResults(human.matches);
    if (human.matches.length === 0) {
      const ai = await aiSearch(q);
      setAIResult(ai);
    }
  }

  return (
    <div>
      <input
        type="text"
        placeholder="Search IOM: lyric, memory, artist, fuzzy clue…"
        value={q}
        onChange={e=>setQ(e.target.value)}
        onKeyDown={e=>e.key==='Enter'&&doSearch()}
        style={{width: "60%"}}
      />
      <button onClick={doSearch}>Search</button>
      {results && <ResultList results={results}/>}
      {aiResult && (
        <div className="ai-suggestion">
          <p><b>AI Suggestion:</b> {aiResult.suggestion}</p>
          <a href={aiResult.url} rel="noopener noreferrer" target="_blank">
            View Match
          </a>
        </div>
      )}
    </div>
  );
}
```
