```jsx
import React, { useState, useEffect } from "react";
import io from "socket.io-client";
const socket = io("http://localhost:3030");

function DirectoryBrowser({ base = "/" }) {
  const [path, setPath] = useState(base);
  const [listing, setListing] = useState([]);
  const [input, setInput] = useState("");
  const [suggested, setSuggested] = useState(null);
  const [presence, setPresence] = useState([]);

  // Load directory and people present
  useEffect(() => {
    fetch(`/api/dir?path=${encodeURIComponent(path)}`)
      .then(res => res.json())
      .then(data => setListing(data.listing));
    fetch(`/api/presence?path=${encodeURIComponent(path)}`)
      .then(res => res.json())
      .then(users => setPresence(users));
    socket.emit("join_path", path);
    socket.on("dir_open", payload => {
      if (payload.path === path && !presence.includes(payload.user)) {
        setPresence(p => [...new Set([...p, payload.user])]);
      }
    });
    return () => {};
    // eslint-disable-next-line
  }, [path]);

  // Fuzzy navigation
  const handleNav = () => {
    fetch(`/api/nav?input=${encodeURIComponent(input)}`)
      .then(async res => {
        if (res.ok) {
          const data = await res.json();
          setPath(data.path);
          setSuggested(data.corrected ? data.path : null);
        } else {
          setSuggested(null);
          alert("Path not found!");
        }
      });
  };

  // "cd .."
  function handleUp() {
    if (path !== "/") {
      const parts = path.split("/");
      parts.pop();
      setPath(parts.join("/") || "/");
    }
  }

  return (
    <div>
      <div>
        <button onClick={handleUp} disabled={path === "/"}>⬆️ Up</button>
        <span style={{ marginLeft: 12, fontWeight: "bold" }}>At: {path}</span>
        <span style={{ marginLeft: 32, color: "#0070f3" }}>
          {presence.length > 0 && "Users here: " + presence.join(", ")}
        </span>
      </div>
      <ul>
        {listing.map(entry => (
          <li key={entry} style={{ cursor: "pointer" }} onClick={() => setPath(path.endsWith("/") ? path + entry : path + "/" + entry)}>
            {entry}
          </li>
        ))}
      </ul>
      <hr />
      <input
        placeholder="cd or fuzzy ls (try typo/partial)..."
        value={input}
        onChange={e => setInput(e.target.value)}
        style={{ marginRight: 8 }}
      />
      <button onClick={handleNav}>Go</button>
      {suggested && (
        <div style={{ color: "#e85c00", marginTop: 10 }}>
          Auto-corrected to: <b>{suggested}</b>
        </div>
      )}
    </div>
  );
}
export default DirectoryBrowser;
```
