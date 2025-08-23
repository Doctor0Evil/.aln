```typescript
// Comet Secret Commands (pseudo-API handler)

export const SECRET_COMMANDS = [
  {
    trigger: "iom://find",
    description: "Open IOM search instantly (lost song discovery)",
    action: () => window.open("/#/search", "_self")
  },
  {
    trigger: "iom://submit",
    description: "Skip to memory submission for new clue or lost video",
    action: () => window.open("/#/submit", "_self")
  },
  {
    trigger: "iom://random",
    description: "Jump to a random IOM mystery",
    action: async () => {
      const cases = await fetch("/api/memory?random=1").then(r=>r.json())
      window.open(`/#/memory/${cases[0].id}`, "_self")
    }
  }
];
// Comet can listen for these in address bar, or inject helper to intercept/trigger
```
