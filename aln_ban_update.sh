#!/usr/bin/env bash
set -euo pipefail

# === Config ===
POLICY_JSON="${POLICY_JSON:-/etc/aln/policy.json}"
POLICY_SIG="${POLICY_SIG:-/etc/aln/policy.json.sig}"
BAN_DB="${BAN_DB:-/var/lib/aln/banlist.db}"
AUDIT_LOG="${AUDIT_LOG:-/var/log/aln/ban_audit.log}"
LOCKFILE="${LOCKFILE:-/var/lock/aln_ban_update.lock}"
TMP_DIR="${TMP_DIR:-/tmp/aln_ban_update}"
LEDGER_ENDPOINT="${LEDGER_ENDPOINT:-}"
LEDGER_TOKEN="${LEDGER_TOKEN:-}"
OPERATOR_ACK_FILE="${OPERATOR_ACK_FILE:-/etc/aln/operator_ack.txt}"

# === Requirements check ===
require() { command -v "$1" >/dev/null 2>&1 || { echo "Missing dependency: $1" >&2; exit 1; }; }
require jq
require gpg
require shasum || true # macOS provides shasum; Linux can use sha256sum
if ! command -v shasum >/dev/null 2>&1 && ! command -v sha256sum >/dev/null 2>&1; then
  echo "Missing checksum tool: shasum or sha256sum" >&2; exit 1
fi

mkdir -p "$(dirname "$BAN_DB")" "$(dirname "$AUDIT_LOG")" "$TMP_DIR"

# === Lock for idempotency/atomicity ===
exec 9>"$LOCKFILE"
flock -n 9 || { echo "Another update is in progress"; exit 1; }

# === Verify policy signature ===
echo "[INFO] Verifying policy signature..."
gpg --verify "$POLICY_SIG" "$POLICY_JSON" >/dev/null 2>&1 || {
  echo "[ERROR] Policy signature verification failed"; exit 2;
}

# === Optional operator acknowledgment ===
ACK_REQ=$(jq -r '.operator_ack_required // false' "$POLICY_JSON")
if [[ "$ACK_REQ" == "true" ]]; then
  if [[ ! -s "$OPERATOR_ACK_FILE" ]]; then
    echo "[ERROR] Operator acknowledgment required but not found at $OPERATOR_ACK_FILE"
    exit 3
  fi
fi

# === Parse policy ===
POLICY_ID=$(jq -r '.policy_id' "$POLICY_JSON")
EFFECTIVE=$(jq -r '.effective' "$POLICY_JSON")
JURIS=$(jq -cr '.jurisdiction')
LEGAL_BASIS=$(jq -cr '.legal_basis')

# === Build new ban DB content from policy ===
BAN_TMP="$TMP_DIR/banlist.new"
: > "$BAN_TMP"

{
  echo "# ALN Ban Database (generated)"
  echo "# Policy: $POLICY_ID"
  echo "# Effective: $EFFECTIVE"
  echo "# Jurisdiction: $JURIS"
  echo "# Legal-Basis: $LEGAL_BASIS"
  echo "# Generated-At: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  echo
  echo "[devices]"
  jq -r '.bans.devices[]? | (.name + " | reason=" + (.reason // "unspecified"))' "$POLICY_JSON"

  echo
  echo "[rf_patterns]"
  jq -r '.bans.rf_patterns[]? | (.pattern + " | reason=" + (.reason // "unspecified"))' "$POLICY_JSON"

  echo
  echo "[neuro_signals]"
  jq -r '.bans.neuro_signals[]? | (.category + " | reason=" + (.reason // "unspecified"))' "$POLICY_JSON"

  echo
  echo "[exceptions]"
  jq -r '.exceptions[]? | ("match=" + ( .match|tostring ) + " | allowed=" + (.allowed|tostring) + " | scope=" + (.scope // "unspecified") + " | justification=" + (.justification // "unspecified"))' "$POLICY_JSON"
} >> "$BAN_TMP"

# === Enforce explicit allow semantics by pruning any "allowed" residues ===
# (This maintains deny-by-default unless explicitly allowed with justification)
SANITIZED="$TMP_DIR/banlist.sanitized"
grep -v -i ' allowed=true' "$BAN_TMP" > "$SANITIZED" || true

# === Atomic replace with backup ===
if [[ -f "$BAN_DB" ]]; then
  cp -f "$BAN_DB" "$BAN_DB.bak.$(date -u +%Y%m%dT%H%M%SZ)"
fi
mv -f "$SANITIZED" "$BAN_DB"

# === Compute file hashes for audit chain ===
HASH_CMD=""
if command -v shasum >/dev/null 2>&1; then HASH_CMD="shasum -a 256"; else HASH_CMD="sha256sum"; fi
BAN_HASH=$($HASH_CMD "$BAN_DB" | awk '{print $1}')
POLICY_HASH=$($HASH_CMD "$POLICY_JSON" | awk '{print $1}')
SIG_HASH=$($HASH_CMD "$POLICY_SIG" | awk '{print $1}')

# Previous audit entry hash for chaining
PREV_HASH=$(tail -n 1 "$AUDIT_LOG" 2>/dev/null | awk -F ' prev=' '{print $2}' | awk '{print $1}')
[ -z "$PREV_HASH" ] && PREV_HASH="GENESIS"

AUDIT_ENTRY="ts=$(date -u +%Y-%m-%dT%H:%M:%SZ) policy=$POLICY_ID ban_db_sha256=$BAN_HASH policy_sha256=$POLICY_HASH sig_sha256=$SIG_HASH prev=$PREV_HASH"
echo "$AUDIT_ENTRY" >> "$AUDIT_LOG"

# === Optional ledger push ===
if [[ -n "$LEDGER_ENDPOINT" && -n "$LEDGER_TOKEN" ]]; then
  echo "[INFO] Pushing update to governance ledger..."
  # Post a minimal, privacy-safe record containing hashes and metadata only
  curl -sS -X POST \
    -H "Authorization: Bearer $LEDGER_TOKEN" \
    -H "Content-Type: application/json" \
    -d "$(jq -n --arg policy "$POLICY_ID" \
                 --arg ban_hash "$BAN_HASH" \
                 --arg policy_hash "$POLICY_HASH" \
                 --arg sig_hash "$SIG_HASH" \
                 --arg prev "$PREV_HASH" \
                 --arg ts "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
                 '{policy_id:$policy, ban_db_sha256:$ban_hash, policy_sha256:$policy_hash, sig_sha256:$sig_hash, prev:$prev, timestamp:$ts}')" \
    "$LEDGER_ENDPOINT" >/dev/null
fi

echo "[OK] Ban database updated under policy $POLICY_ID"
exit 0
