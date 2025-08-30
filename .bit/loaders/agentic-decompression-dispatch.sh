#!/usr/bin/env bash
set -euo pipefail
EVENT="decompression"
PAYLOAD_FILE="$1"
ENDPOINT="${AGENTIC_ENDPOINT:-}"
TOKEN="${AGENTIC_TOKEN:-}"

[[ -z "$ENDPOINT" || -z "$TOKEN" ]] && exit 0

curl -sS -X POST "$ENDPOINT" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  --data @"$PAYLOAD_FILE"
