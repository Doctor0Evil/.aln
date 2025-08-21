```tsx
import React, { useState } from "react";

export function VaultUnlock() {
  const [unlocked, setUnlocked] = useState(false);
  const [passcode, setPasscode] = useState("");

  function checkUnlock() {
    // In real app: verify code via backend/API
    if (passcode.toLowerCase() === "aurora") {
      setUnlocked(true);
    } else {
      alert("Incorrect magic phrase! Try again.");
    }
  }
  return (
    <div className="vault-unlock">
      <h3>🔑 Vault Unlock</h3>
      {unlocked ? (
        <div>
          <p>Congratulations! You’ve unlocked the Vault of Legendary Cases.</p>
          <a href="/#/vault" style={{ color: "#c454fc" }}>
            Enter the Vault
          </a>
        </div>
      ) : (
        <div>
          <input
            value={passcode}
            onChange={e => setPasscode(e.target.value)}
            placeholder="Enter secret code…"
          />
          <button onClick={checkUnlock}>Unlock</button>
        </div>
      )}
    </div>
  );
}
```
