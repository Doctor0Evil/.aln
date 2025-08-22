```markdown
# 🏛️ ALNFantasia — Grimoire RuleBook
This is the Governance Charter: the laws by which worlds expand and persist.

---

## ⚖️ Governance System

- Decisions are made via **Grimoire Points (GP)**.  
- GP may be earned through:
  - Quests completed
  - Community contributions (lore, code, art)
  - Winning skill checks, multiplayer duels

---

## 📜 Voting Rules

- **Voting Period**: 7 days  
- **Quorum Requirement**: ≥ 10% of total active GP  
- **Pass Threshold**: ≥ 60% Yes votes  
- **Fail/Disqualify**: Quorum not met, or ≥ 40% Reject  

---

## 🪄 Proposal Flow (ALN Syntax)

```
@VOTE {
  id: "expansion-prehistoric-epoch"
  description: "Add Prehistoric Civilization Era with resource gathering quests."
  quorum: 10%
  threshold: 60%
  status: "OPEN"
  votes: {yes: 1230, no: 45, abstain: 60}
}
```

When status = `PASSED` → **AI Autonomous Development Trigger**:  
ALN interprets `@CREATE` blocks and **auto-generates** content (quests, APIs, 2D tilesets, maps).  

---

## 💠 GP Ledger

- Each GP is tracked at `/Governance/GP/UserID.json`.  
- Example:
```
{
 "userID": "scribe42",
 "gp": 1540,
 "roles": ["Lorekeeper", "Questmaster"],
 "voteHistory": ["expansion-prehistoric-epoch"]
}
```

---

## 🧩 Proposals

Types of Proposals:
1. **Lore Additions** (new realms, quests, NPCs)
2. **Mechanics Additions** (new dice, gameplay rules)
3. **Governance Adjustments** (change charter laws)

---

## 📚 Codex Permanence

- Passed proposals are committed to **Grimoire Canon**.  
- **Sanctum Canon Rule**: Unanimous acceptance prevents future alteration.  
```
