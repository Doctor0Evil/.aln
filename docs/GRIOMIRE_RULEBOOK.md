```markdown
# Grimoire Rulebook

## 🪄 Grimoire Points (GP)
- **Earn GP**: Contributing content, winning dice challenges, proposing features.
- **Spend GP**: Voting on proposals, unlocking rare assets, forging new rules.

## ⚖️ Governance Rules
- **Voting Period**: 7 days (configurable).
- **Quorum**: 10% of active GP to qualify.
- **Passing Threshold**: ≥ 60% Yes votes.
- **Disqualify/Reject**: Insufficient quorum OR ≥ 40% reject.

## 📌 Voting Example (ALN Syntax)
```aln
@VOTE {
  id: "world-expansion-epoch-prehistoric"
  description: "Add Prehistoric Civilization Era with resource-gathering quests."
  quorum: 10%
  threshold: 60%
  status: "OPEN"
  currentVotes: { yes: 98%, no: 1%, quorum: 1% }
}
```
