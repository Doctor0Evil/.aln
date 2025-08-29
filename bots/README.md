# github.aln.bots

Manifest-driven GitHub App that listens to repo events and responds to `/aln.*` commands in issues/PRs.

## Quick start

1. Install dependencies: `npm ci`
2. Run local (needs ngrok or tunnel):  
   - `ALN_APP_ID=... ALN_PRIVATE_KEY="..." ALN_WEBHOOK_SECRET=..." npm start`
3. Expose port 3000 and set the GitHub App webhook to your public URL.

## Commands
- /aln.game.dice { "sides": 20 }
- /aln.game.quest { "action": "explore", "location": "cave" }
- /aln.game.hangman { "letter": "A" }
- /aln.community.santaGift { "target": "@user", "gift": "rare-item" }
- /utility.syncTree
