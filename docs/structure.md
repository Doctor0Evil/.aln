```
/src/packages/
    DICE.aln         # All dice mechanics
    PLAYER.aln       # Stats, GP, spells, mutations
    LORE.aln         # Shared world narrative log
    SYNC.aln         # API sync: log, save, load
    GOVERNANCE.aln   # Voting, proposals
    QUEST_ENGINE.aln # Encounters, procedural rooms
    QUEST_STORY.aln  # Branching quest narratives
    ARTIFACTS.aln    # Items, inventory, merchant links
    NAV.aln          # Navigation UX, directory, shop routes

/apischema/
    player.schema.json
    log.schema.json
    item.schema.json

/apiendpoints/
    index.js               # API stub
    [routes: log.js, saveplayer.js, ...]
/examples/
    multi-sync-demo.aln    # Demo: 2 player session synced
    queststart.aln         # Quest start sample session
    merchant-demo.aln      # Merchant/shop interaction demo
```
