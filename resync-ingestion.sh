#!/usr/bin/env bash
set -euo pipefail

echo "🔄 Ingestion resync $(date -u +%FT%TZ)"

LEDGER_FILE="registries/command-ledger.alnlog"
MANIFEST_FILE="manifests/master-enforcement.aln"
INGEST_DIR="data/ingestion"

# Signature check
if ! verify-ed25519 "$MANIFEST_FILE" ".keys/ed25519.public"; then
  echo "❌ Policy manifest signature invalid"
  exit 1
fi

mkdir -p "$INGEST_DIR"

# Stream ingest -> Data.Bank to avoid RAM growth
aln-analyze --stream | while IFS= read -r line || [[ -n "$line" ]]; do
  echo "$line" > "$INGEST_DIR/chunk.aln"
  curl -fsS -X POST \
    -H "Authorization: Bearer ${DATABANK_TOKEN:-}" \
    -H "Content-Type: application/octet-stream" \
    --data-binary @"$INGEST_DIR/chunk.aln" \
    "${DATABANK_URL:-http://so.bit}/ingest/violation"
  : > "$INGEST_DIR/chunk.aln"
  sudo sh -c 'sync; echo 3 > /proc/sys/vm/drop_caches' || true
done

HASH=$(sha256sum "$MANIFEST_FILE" | awk '{print $1}')
printf '{"ts":"%s","bot":"resync-ingestion","repo":"%s","payload":"%s"}\n' \
  "$(date -u +%FT%TZ)" "${GITHUB_REPOSITORY:-local}" "$HASH" >> "$LEDGER_FILE"

echo "✅ Ingestion resync complete."
