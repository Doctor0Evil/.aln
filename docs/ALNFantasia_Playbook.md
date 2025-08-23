```markdown
# 📖 ALNFantasia Playbook

Welcome, Adventurer!  
This is the Player’s Codex — the manual that guides your journey through worlds of lore, quests, dice, and community-driven creation.

---

## 🗺️ Directory Navigation & Commands

Everything in ALNFantasia is **directory-based** like a magical filesystem.

- `cd /World/Medieval/Empire` → Enter the Medieval Empire realm  
- `ls` → Lists available quests, NPCs, or actions  
- `QUEST.START /World/Zombies/City/Ruins` → Begin a survival quest

**Command Syntax (ALN-based):**

- `DICE.ROLL.d20` → Roll 20-sided dice (combat, quests, fate)  
- `LOOT.SEARCH /path` → Finds items, relics, resources  
- `BATTLE.START /World/Spy/Mission007` → Trigger battle/stealth sequence  
- `PLAYER.STATS` → View character sheet & GP balance  
- `LOG.SYNC` → Syncs your gameplay log with Codex servers  

---

## 🎲 Dice & Game Rules

- **d6 Loot Rolls**: Resources, traps, events  
- **d10 Skills**: Crafting, persuasion, stealth  
- **d20 Combat**: Strategy, duels, boss fights  

Modifiers from player’s **Stats** apply (Strength, Intellect, Willpower).

---

## 🧑‍🤝‍🧑 Multiplayer Play

- Each player has a **persistent character file** (`/Inventory/UserID.json`).  
- Actions are **logged, synced, and mirrored** via APIs to central servers.  
- All changes are reflected across AI platforms for consistent progression.  

---

## 🌌 Example Play Flow

```
> cd /World/Prehistoric/Tribe/Hunt
> QUEST.START MammothChase
> DICE.ROLL.d20
[You rolled 18 — Success! The tribe secures enough meat for the clan.]
> LOOT.SEARCH
[Found: 2x Tusks, 3x Food Rations]
> LOG.SYNC
[Progress logged to Codex; 25 GP earned]
```

---

## 🌍 Realms & Cultures

Different subdirectories align with **thematic cultures**:

- `/Zombies/City/Ruins/Survival`
- `/PostApocalypse/Scavenger`
- `/Medieval/Empire/Strategy`
- `/Spy/Agency/Mission007`
- `/Prehistoric/Epoch/Tribes`
- `/CIA/Assassin/Missions`
- `/CivilWar/Revolution/Flags`
- `/BattlefieldAI/UAV/Tactics`
- `/Underwater/Naval/Submarine`

---

## 🧾 Quest Logs

Every action is preserved in the **Codex of Lore**.  
Your deeds echo forever, influencing future stories and expansions.
```
