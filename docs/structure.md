```
```aln
# src/packages/GOVERNANCE.aln (Existing file - this is the implementation)
SETTINGS
    GP_CURRENCY string = "Grimoire Points"
    VOTING_PERIOD int = 7 * 86400  # 7 days in seconds
    QUORUM_PERCENT float = 0.01
    PASS_THRESHOLD float = 0.60

ACTION propose
    INPUT title string, type string, details string, voting_period string = "7d"
    EXEC
        proposal_id = RANDOM.id()
        PROPOSAL[proposal_id] = {
            "title": title,
            "type": type,
            "details": details,
            "voting_period": voting_period,
            "status": "active"
        }
        LOG 📜 Proposal #{proposal_id} created: {title}
        RETURN proposal_id

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
