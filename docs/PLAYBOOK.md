```markdown
# ALNFantasia Playbook

## 🎲 Dice & Quests
- `DICE.ROLL.d20` → Standard d20 roll (combat checks, lore triggers).
- `DICE.ROLL.LOOT.d6` → Loot Table / Resource discovery mechanic.
- `DICE.ROLL.SKILL.d10` → Used for skill challenges.

## 🧭 Directory-Based World Navigation
Instead of flat commands, we use a **path-based navigation system** in ALN syntax:

```aln
@NAV {
  /World/Medieval/Empire/Quests/CastleSiege
  /World/Zombies/City/Ruins/Survival
  /World/Spy/Agency/Mission007
  /World/PostApocalypse/Scavenger/LootMap
  /World/Prehistoric/Tribe/ResourceGathering
}
```

Players can `cd` into any directory to explore:
- Example:
  - `cd /World/Medieval/Empire` → Move to an empire-strategy realm.
  - `ls` → List subquests/items.
  - `PLAY QuestName` → Launch quest.

## 📜 Example Commands
- `QUEST.START /World/Zombies/City/Ruins`
- `LOOT.SEARCH /World/PostApocalypse/Scavenger`
- `BATTLE.START /World/Medieval/Army/Skirmish`
- `LOG.PROGRESS` → Syncs with player stats.
```
