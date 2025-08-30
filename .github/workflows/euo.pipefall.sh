#!/usr/bin/env bash
set -euo pipefail

CFG=".bit/linking.repo.bit.yml"
ENDPOINT="${AGENTIC_ENDPOINT:-}"
TOKEN="${AGENTIC_TOKEN:-}"
if [[ -z "$ENDPOINT" || -z "$TOKEN" ]]; then
  echo "[agentic] Skipping (no credentials)"
  exit 0
fi

EVENT="$1"                # e.g., preflight.ok, preflight.fail, resync.ok
PAYLOAD_FILE="${2:-}"     # optional JSON file

echo "[agentic] Send $EVENT to $ENDPOINT"
curl -sS -X POST "$ENDPOINT" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  --data @"${PAYLOAD_FILE:-/dev/stdin}" <<< "{\"event\":\"$EVENT\",\"repo\":\"$GITHUB_REPOSITORY\",\"run_id\":\"$GITHUB_RUN_ID\"}"
