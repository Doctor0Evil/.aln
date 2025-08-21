```tsx
import React from "react";
import { SECRET_COMMANDS } from "../comet/secretCommands";

export function CometWidget() {
  // Only show if detected in Comet browser (can sniff UA or window.Comet present)
  return (
    <div className="comet-widget">
      <strong>✨ Comet Browser Power Tips:</strong>
      <ul>
        {SECRET_COMMANDS.map((cmd) => (
          <li key={cmd.trigger}>
            <kbd>{cmd.trigger}</kbd> – {cmd.description}
          </li>
        ))}
      </ul>
    </div>
  );
}
```
