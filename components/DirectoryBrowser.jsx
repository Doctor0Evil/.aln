```jsx
import React, { useState, useEffect } from "react";

function DirectoryBrowser({ base = "/" }) {
  const [path, setPath] = useState(base);
  const [contents, setContents] = useState([]);

  useEffect(() => {
    fetch(`/api/dir?path=${encodeURIComponent(path)}`)
      .then(res => res.json())
      .then(data => setContents(data.listing));
  }, [path]);

  function handleClick(entry) {
    // If entry is directory, go deeper, otherwise open file (expand as needed)
    setPath(p => p.endsWith("/") ? p + entry : p + "/" + entry);
  }

  function handleUp() {
    if (path !== "/") {
      setPath(path.split("/").slice(0, -1).join("/") || "/");
    }
  }

  return (
    <div>
      <div>
        <button onClick={handleUp} disabled={path === "/"}>⬆️ Up</button>
        <span style={{ marginLeft: 12 }}>Current: {path}</span>
      </div>
      <ul>
        {contents.map(entry => (
          <li key={entry} style={{ cursor: "pointer" }} onClick={() => handleClick(entry)}>
            {entry}
          </li>
        ))}
      </ul>
    </div>
  );
}

export default DirectoryBrowser;
```
