#!/usr/bin/env bash
set -euo pipefail
MANIFEST=".bit/bitrunners.manifest.yml"
LEDGER=$(yq '.runner.ledger.path' "$MANIFEST")
mkdir -p "$(dirname "$LEDGER")"
echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"BOOT\",\"runner\":\"$(yq '.runner.id' "$MANIFEST")\"}" >> "$LEDGER"

# Start OPA server with policies (optional headless use)
if command -v opa >/dev/null; then
  opa run --server --addr :8181 "$(yq '.runner.policies.dir' "$MANIFEST")" >/dev/null 2>&1 &
  echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"OPA_START\",\"port\":8181}" >> "$LEDGER"
fi

# Prewarm cache
if .bit/loaders/cache_prewarm.sh; then
  echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"CACHE_PREWARM_OK\"}" >> "$LEDGER"
else
  echo "{\"ts\":\"$(date -Iseconds)\",\"event\":\"CACHE_PREWARM_SKIP_OR_FAIL\"}" >> "$LEDGER"
fi
