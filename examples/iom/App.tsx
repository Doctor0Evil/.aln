```tsx
import React from "react";
import { SearchBar } from "./components/SearchBar";
import { ResultList } from "./components/ResultList";
import { MemorySubmission } from "./components/MemorySubmission";
import { SuggestionPanel } from "./components/SuggestionPanel";
import { CometWidget } from "./components/CometWidget";
import "./styles/main.css";

export default function App() {
  // Routing logic omitted for brevity; use React Router or a simple tab state.
  return (
    <div>
      <CometWidget />
      <h1>IOM: Internet Archive of Music</h1>
      <SearchBar />
      <MemorySubmission />
      <ResultList />
      <SuggestionPanel />
    </div>
  );
}
```
