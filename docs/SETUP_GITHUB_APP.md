#### /docs/SETUP_GITHUB_APP.md
```md
# Setup: GitHub App for github.aln.bots

1. Create App:
   - Settings → Developer settings → GitHub Apps → New GitHub App
   - Webhook URL: your public URL (ngrok or your host) + `/api/github/webhooks`
   - Webhook secret: set and save; reuse as `ALN_WEBHOOK_SECRET`
   - Permissions:
     - Repository contents: Read/Write
     - Issues: Read/Write
     - Pull requests: Read/Write
     - Checks: Read/Write
     - Metadata: Read
   - Subscribe to events: issues, issue_comment, pull_request, pull_request_review, check_suite, check_run, push

2. Generate Private Key:
   - Download PEM; set as `ALN_PRIVATE_KEY` (keep line breaks)

3. Install App:
   - Install on your organization/repository

4. Run locally:
   - `npm ci` inside `/bots`
   - Expose port 3000 (e.g., `ngrok http 3000`)
   - Set env vars: `ALN_APP_ID`, `ALN_PRIVATE_KEY`, `ALN_WEBHOOK_SECRET`
   - `npm start`

5. Test commands in an issue/PR comment:
   - `/aln.game.dice { "sides": 20 }`
   - `/utility.syncTree`
```
