#!/usr/bin/env bash
set -euo pipefail

POLICY=".bit/decompression.policy.yml"
LOGFILE=$(yq '.compliance.log_to_ledger' "$POLICY")
mkdir -p "$(dirname "$LOGFILE")"

SRC="$1"
DEST="$2"

# Validate format
EXT="${SRC##*.}"
if ! yq '.allowed_formats[]' "$POLICY" | grep -qx "$EXT"; then
  echo "❌ Format .$EXT not allowed by policy"
  exit 1
fi

# Validate destination
if ! yq '.destinations[]' "$POLICY" | grep -qx "$DEST"; then
  echo "❌ Destination $DEST not allowed by policy"
  exit 1
fi

# Optional checksum verification
if [[ "$(yq '.compliance.checksum_required' "$POLICY")" == "true" ]]; then
  if [[ -f "$SRC.sha256" ]]; then
    sha256sum -c "$SRC.sha256"
  else
    echo "❌ Missing checksum file for $SRC"
    exit 1
  fi
fi

# Decompress safely
mkdir -p "$DEST"
case "$EXT" in
  zip) unzip -q "$SRC" -d "$DEST" ;;
  gz) gunzip -c "$SRC" > "${DEST}/$(basename "$SRC" .gz)" ;;
  bz2) bunzip2 -c "$SRC" > "${DEST}/$(basename "$SRC" .bz2)" ;;
  xz) unxz -c "$SRC" > "${DEST}/$(basename "$SRC" .xz)" ;;
  tar.gz) tar -xzf "$SRC" -C "$DEST" ;;
  tar.bz2) tar -xjf "$SRC" -C "$DEST" ;;
  tar.xz) tar -xJf "$SRC" -C "$DEST" ;;
  zstd) zstd -d "$SRC" -o "${DEST}/$(basename "$SRC" .zst)" ;;
esac

# Log to ledger
TS=$(date -Iseconds)
SHA=$(sha256sum "$SRC" | awk '{print $1}')
echo "{\"ts\":\"$TS\",\"src\":\"$SRC\",\"dest\":\"$DEST\",\"sha256\":\"$SHA\"}" >> "$LOGFILE"

echo "✅ Decompression complete and logged."
