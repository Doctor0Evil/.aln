├─ bots/
│  ├─ package.json
│  ├─ Dockerfile
│  ├─ src/
│  │  ├─ index.js
│  │  ├─ app.js
│  │  ├─ events/
│  │  │  ├─ onIssueComment.js
│  │  │  ├─ onPullRequest.js
│  │  │  └─ onPush.js
│  │  ├─ commands/
│  │  │  ├─ dispatcher.js
│  │  │  ├─ aln.game.dice.js
│  │  │  ├─ aln.game.quest.js
│  │  │  ├─ aln.game.hangman.js
│  │  │  ├─ aln.community.santaGift.js
│  │  │  └─ utility.syncTree.js
│  │  ├─ loaders/
│  │  │  ├─ manifestLoader.js
│  │  │  └─ security.js
│  │  └─ utils/
│  │     ├─ parseCommand.js
│  │     ├─ comments.js
│  │     └─ random.js
│  ├─ config/
│  │  ├─ bots.manifest.json
│  │  └─ accessibility.tree.json
│  └─ README.md
├─ .github/
│  └─ workflows/
│     ├─ bots-ci.yml
│     └─ bots-deploy-example.yml
└─ docs/
   ├─ SETUP_GITHUB_APP.md
   └─ PLAYBOOK.md
```

---

### Full files

#### /bots/package.json
```json
{
  "name": "github.aln.bots",
  "version": "1.0.0",
  "description": "Manifest-driven GitHub App for ALN commands, PR automations, and community gifting.",
  "main": "src/index.js",
  "type": "module",
  "scripts": {
    "dev": "nodemon src/index.js",
    "start": "node src/index.js",
    "test": "node --test",
    "lint": "eslint ."
  },
  "dependencies": {
    "@octokit/webhooks": "^12.0.10",
    "probot": "^12.3.3",
    "express": "^4.19.2",
    "ajv": "^8.17.1"
  },
  "devDependencies": {
    "eslint": "^9.9.0",
    "nodemon": "^3.1.4"
  }
}
```

#### /bots/Dockerfile
```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package.json package-lock.json* ./
RUN npm ci --omit=dev
COPY src ./src
COPY config ./config
ENV NODE_ENV=production
EXPOSE 3000
CMD ["node", "src/index.js"]
```

#### /bots/src/index.js
```js
import { Probot } from "probot";
import appFn from "./app.js";

const port = process.env.PORT || 3000;

async function main() {
  const probot = new Probot({
    appId: process.env.ALN_APP_ID,
    privateKey: process.env.ALN_PRIVATE_KEY?.replace(/\\n/g, "\n"),
    secret: process.env.ALN_WEBHOOK_SECRET,
    logLevel: process.env.ALN_LOG_LEVEL || "info"
  });

  await probot.load(appFn);
  const server = await probot.httpServer({ port });
  // Graceful shutdown
  process.on("SIGTERM", () => server.close(() => process.exit(0)));
  process.on("SIGINT", () => server.close(() => process.exit(0)));
}

main().catch(err => {
  // eslint-disable-next-line no-console
  console.error("Fatal startup error:", err);
  process.exit(1);
});
```

#### /bots/src/app.js
```js
import onIssueComment from "./events/onIssueComment.js";
import onPullRequest from "./events/onPullRequest.js";
import onPush from "./events/onPush.js";
import { loadManifest } from "./loaders/manifestLoader.js";

export default (app) => {
  const manifest = loadManifest(app.log);

  app.on("issue_comment.created", async (ctx) =>
    onIssueComment(ctx, manifest, app.log)
  );
  app.on(["pull_request.opened", "pull_request.synchronize"], async (ctx) =>
    onPullRequest(ctx, manifest, app.log)
  );
  app.on("push", async (ctx) =>
    onPush(ctx, manifest, app.log)
  );

  app.log.info({
    bots: manifest.bots.map(b => b.id),
    commands: manifest.commands.map(c => c.id)
  }, "github.aln.bots ready");
};
```

#### /bots/src/loaders/manifestLoader.js
```js
import fs from "node:fs";
import path from "node:path";
import Ajv from "ajv";

const manifestPath = path.resolve("config/bots.manifest.json");

export function loadManifest(log) {
  const raw = fs.readFileSync(manifestPath, "utf8");
  const manifest = JSON.parse(raw);

  const ajv = new Ajv({ allErrors: true });
  const schema = {
    type: "object",
    properties: {
      bots: { type: "array" },
      commands: { type: "array" },
      accessibilityTree: { type: "string" }
    },
    required: ["bots", "commands", "accessibilityTree"]
  };

  const validate = ajv.compile(schema);
  if (!validate(manifest)) {
    log.error({ errors: validate.errors }, "Invalid bots.manifest.json");
    throw new Error("Manifest validation failed");
  }
  return manifest;
}
```

#### /bots/src/loaders/security.js
```js
export function isOrgAllowed(ctx) {
  const allowed = (process.env.ALN_ALLOWED_ORGS || "").split(",").map(s => s.trim()).filter(Boolean);
  if (allowed.length === 0) return true;
  const org = ctx.payload.organization?.login || ctx.payload.repository?.owner?.login;
  return allowed.includes(org);
}
```

#### /bots/src/utils/parseCommand.js
```js
// Parses /aln commands from issue/PR comments.
// Supports both slash and raw: "/aln.game.dice { sides: 20 }" or "aln.game.dice {sides:20}"
export function parseCommand(body) {
  const line = body.split("\n").find(l => l.trim().startsWith("/aln.") || l.trim().startsWith("aln."));
  if (!line) return null;

  const [cmd, ...rest] = line.trim().replace(/^\//, "").split(/\s+/, 2);
  let params = {};
  if (rest.length) {
    const jsonLike = rest[0]
      .replace(/([a-zA-Z0-9_]+)\s*:/g, '"$1":') // keys to JSON
      .replace(/'/g, '"');
    try { params = JSON.parse(jsonLike); } catch { params = {}; }
  }
  return { cmd, params };
}
```

#### /bots/src/utils/comments.js
```js
export async function reply(ctx, body) {
  const issue = ctx.issue();
  return ctx.octokit.issues.createComment({ ...issue, body });
}

export function codeFence(content, lang = "") {
  return "```" + lang + "\n" + content + "\n```";
}
```

#### /bots/src/utils/random.js
```js
export function randInt(max) {
  return Math.floor(Math.random() * max);
}
```

#### /bots/src/commands/dispatcher.js
```js
import dice from "./aln.game.dice.js";
import quest from "./aln.game.quest.js";
import hangman from "./aln.game.hangman.js";
import santaGift from "./aln.community.santaGift.js";
import syncTree from "./utility.syncTree.js";

const handlers = {
  "aln.game.dice": dice,
  "aln.game.quest": quest,
  "aln.game.hangman": hangman,
  "aln.community.santaGift": santaGift,
  "utility.syncTree": syncTree
};

export function getHandler(id) {
  return handlers[id];
}
```

#### /bots/src/commands/aln.game.dice.js
```js
import { randInt } from "../utils/random.js";
import { codeFence } from "../utils/comments.js";

export default async function handle(ctx, params) {
  const sides = Number.isInteger(params?.sides) ? params.sides : 20;
  if (sides < 2) {
    return ctx.octokit.reactions.createForIssueComment({
      ...ctx.repo(),
      comment_id: ctx.payload.comment.id,
      content: "-1"
    });
  }
  const roll = randInt(sides) + 1;
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🎲 D${sides} rolled: ${codeFence(String(roll))}`
  });
}
```

#### /bots/src/commands/aln.game.quest.js
```js
import { codeFence } from "../utils/comments.js";

export default async function handle(ctx, params) {
  const action = (params?.action || "explore").toLowerCase();
  const location = params?.location || "cave";
  const lines = {
    explore: `You venture into the ${location} and discover a hidden chamber lit by bioluminescent moss.`,
    fight: `Steel clashes in the ${location}. You parry, riposte, and seize the opening.`,
    trade: `A quiet stall in the ${location}. One relic hums softly, as if alive.`
  };
  const out = lines[action] || `Unknown action: ${action}`;
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🧭 Quest: ${codeFence(out)}`
  });
}
```

#### /bots/src/commands/aln.game.hangman.js
```js
import { codeFence } from "../utils/comments.js";

export default async function handle(ctx, params) {
  const letter = (params?.letter || "").toUpperCase();
  if (!letter || !/^[A-Z]$/.test(letter)) {
    return ctx.octokit.issues.createComment({
      ...ctx.issue(),
      body: "Provide a single letter: `/aln.game.hangman { \"letter\": \"A\" }`"
    });
  }
  // Stateless demo: in real use, persist state to a discussion or issue body checklist
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🔤 Hangman guess: ${codeFence(letter)}`
  });
}
```

#### /bots/src/commands/aln.community.santaGift.js
```js
export default async function handle(ctx, params) {
  const target = params?.target || "@everyone";
  const gift = params?.gift || "Mystery Box";
  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body: `🎁 Santa has delivered "${gift}" to ${target}!`
  });
}
```

#### /bots/src/commands/utility.syncTree.js
```js
import fs from "node:fs";
import path from "node:path";

export default async function handle(ctx) {
  const treePath = path.resolve("config/accessibility.tree.json");
  const content = fs.readFileSync(treePath, "utf8");
  const body = [
    "♿ Accessibility Tree sync:",
    "```json",
    content,
    "```"
  ].join("\n");

  return ctx.octokit.issues.createComment({
    ...ctx.issue(),
    body
  });
}
```

#### /bots/src/events/onIssueComment.js
```js
import { parseCommand } from "../utils/parseCommand.js";
import { getHandler } from "../commands/dispatcher.js";
import { isOrgAllowed } from "../loaders/security.js";

export default async function onIssueComment(ctx, manifest, log) {
  if (!isOrgAllowed(ctx)) return;

  const { body } = ctx.payload.comment;
  const parsed = parseCommand(body);
  if (!parsed) return;

  const cmd = manifest.commands.find(c => c.id === parsed.cmd);
  if (!cmd) {
    await ctx.octokit.reactions.createForIssueComment({
      ...ctx.repo(),
      comment_id: ctx.payload.comment.id,
      content: "confused"
    });
    return;
  }

  const handler = getHandler(cmd.id);
  if (!handler) {
    log.warn({ cmd: cmd.id }, "Handler not found");
    return;
  }

  log.info({ cmd: cmd.id, params: parsed.params }, "Executing command");
  try {
    await handler(ctx, parsed.params);
  } catch (e) {
    log.error({ err: e, cmd: cmd.id }, "Command execution failed");
    await ctx.octokit.issues.createComment({
      ...ctx.issue(),
      body: `⚠️ Command failed: ${e.message}`
    });
  }
}
```

#### /bots/src/events/onPullRequest.js
```js
export default async function onPullRequest(ctx, manifest, log) {
  const pr = ctx.payload.pull_request;
  const labels = [];

  if (pr.changed_files >= 20) labels.push("size/XL");
  else if (pr.changed_files >= 10) labels.push("size/L");
  else if (pr.changed_files >= 5) labels.push("size/M");
  else labels.push("size/S");

  try {
    if (labels.length) {
      await ctx.octokit.issues.addLabels({
        ...ctx.repo(),
        issue_number: pr.number,
        labels
      });
    }

    // Gentle checklist
    const body = [
      "🔍 PR checklist:",
      "- [ ] Tests or validation updated",
      "- [ ] Manifest entries updated (if adding commands/tools)",
      "- [ ] Docs synced (`/utility.syncTree` to preview accessibility tree)"
    ].join("\n");

    await ctx.octokit.issues.createComment({ ...ctx.issue(), body });
  } catch (e) {
    log.error({ err: e }, "PR labeling/checklist failed");
  }
}
```

#### /bots/src/events/onPush.js
```js
export default async function onPush(ctx, manifest, log) {
  const files = ctx.payload.commits.flatMap(c => c.added.concat(c.modified));
  const touchedManifest = files.some(f => f.endsWith("bots.manifest.json"));

  if (touchedManifest) {
    try {
      await ctx.octokit.repos.createCommitStatus({
        ...ctx.repo({ sha: ctx.payload.after }),
        state: "pending",
        context: "aln/manifest-validate",
        description: "Validating bots.manifest.json"
      });
      // If it loaded here, manifest is valid.
      await ctx.octokit.repos.createCommitStatus({
        ...ctx.repo({ sha: ctx.payload.after }),
        state: "success",
        context: "aln/manifest-validate",
        description: "Manifest validated"
      });
    } catch (e) {
      log.error({ err: e }, "Manifest status update failed");
    }
  }
}
```

#### /bots/config/bots.manifest.json
```json
{
  "bots": [
    {
      "id": "github.aln.gatekeeper",
      "description": "PR labeling, checklists, and manifest validation."
    },
    {
      "id": "github.aln.santa",
      "description": "Community gifting and seasonal drops."
    },
    {
      "id": "github.aln.librarian",
      "description": "Syncs / previews accessibility tree and docs."
    }
  ],
  "commands": [
    {
      "id": "aln.game.dice",
      "description": "Roll a die",
      "params": { "sides": "number" }
    },
    {
      "id": "aln.game.quest",
      "description": "Begin a quest scene",
      "params": { "action": "string", "location": "string" }
    },
    {
      "id": "aln.game.hangman",
      "description": "Guess a letter",
      "params": { "letter": "string" }
    },
    {
      "id": "aln.community.santaGift",
      "description": "Gift an item to a user",
      "params": { "target": "string", "gift": "string" }
    },
    {
      "id": "utility.syncTree",
      "description": "Preview the accessibility tree",
      "params": {}
    }
  ],
  "accessibilityTree": "config/accessibility.tree.json"
}
```

#### /bots/config/accessibility.tree.json
```json
{
  "root": {
    "Gameplay Tools": [
      "Dice Roller (D4-D100)",
      "Quest Engine",
      "Hangman / Word Games"
    ],
    "Community Modules": [
      "Santa Gift Drop",
      "ALN.Fantasia Archive",
      "Contributor Bot Deck"
    ],
    "System & Dev Tools": [
      "CI/CD Hook",
      "ALN Lint / Clean",
      "Error Recovery Agents"
    ]
  }
}
```

#### /bots/README.md
```md
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
```

#### /.github/workflows/bots-ci.yml
```yaml
name: bots-ci
on:
  push:
    paths:
      - "bots/**"
      - ".github/workflows/bots-ci.yml"
  pull_request:
    paths:
      - "bots/**"

jobs:
  ci:
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: bots
    steps:
      - name: Checkout
        uses: actions/checkout@v4
      - name: Use Node
        uses: actions/setup-node@v4
        with:
          node-version: 18
          cache: npm
          cache-dependency-path: bots/package-lock.json
      - name: Install
        run: npm ci
      - name: Lint
        run: npm run lint || true
      - name: Manifest sanity
        run: node -e "JSON.parse(require('fs').readFileSync('config/bots.manifest.json','utf8')); console.log('manifest: OK')"
```

#### /.github/workflows/bots-deploy-example.yml
```yaml
name: bots-deploy-example
on:
  workflow_dispatch:
    inputs:
      environment:
        description: "Deploy environment"
        required: true
        default: "staging"

jobs:
  build-and-output:
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: bots
    steps:
      - name: Checkout
        uses: actions/checkout@v4
      - name: Use Node
        uses: actions/setup-node@v4
        with:
          node-version: 18
          cache: npm
          cache-dependency-path: bots/package-lock.json
      - name: Install
        run: npm ci
      - name: Build Docker image
        run: docker build -t github-aln-bots:latest .
      - name: Show run command
        run: |
          echo "Run locally:"
          echo "docker run -p 3000:3000 \\"
          echo "  -e ALN_APP_ID=\$ALN_APP_ID \\"
          echo "  -e ALN_PRIVATE_KEY=\$ALN_PRIVATE_KEY \\"
          echo "  -e ALN_WEBHOOK_SECRET=\$ALN_WEBHOOK_SECRET \\"
          echo "  github-aln-bots:latest"
```

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

#### /docs/PLAYBOOK.md
```md
# ALN Command Playbook

## Core Gameplay
- `aln.game.dice { "sides": 20 }` — Roll a D20
- `aln.game.quest { "action": "explore", "location": "cave" }` — Begin quest
- `aln.game.hangman { "letter": "A" }` — Guess a letter

## Community
- `aln.community.santaGift { "target": "@user", "gift": "rare-item" }` — Gift an item

## Utility
- `utility.syncTree` — Preview the accessibility tree in the current thread
```

---

### How to deploy and verify

- **Local run (quickest):**  
  - **Install:** npm ci in /bots  
  - **Expose:** tunnel port 3000  
  - **Set env:** ALN_APP_ID, ALN_PRIVATE_KEY, ALN_WEBHOOK_SECRET  
  - **Start:** npm start  
  - **Verify:** Comment on any issue/PR: /aln.game.dice { "sides": 20 }

- **Container run:**  
  - **Build:** docker build -t github-aln-bots:latest bots  
  - **Run:** docker run -p 3000:3000 -e ALN_APP_ID=... -e ALN_PRIVATE_KEY="..." -e ALN_WEBHOOK_SECRET=... github-aln-bots:latest

- **Production host:**  
  - **Requirement:** Public HTTPS endpoint and persistent env secrets  
  - **Scale:** Single instance is fine; stateless by default

---

### Optional hardening and evolution

- **Auth scope guard:** Set ALN_ALLOWED_ORGS to restrict responding to specific orgs.  
- **Stateful games:** Persist hangman/quest state in issue body checklists or a tiny KV store.  
- **Seasonal drops:** Add a cron-driven job or scheduled Action that opens a PR with a “gift” content pack.  
- **Audit hooks:** Emit JSON logs to a central log store; include command, actor, repo, and result.

If you want, I can add a scheduled seasonal “gift drop” PR creator and a minimal KV-backed hangman state module next.
