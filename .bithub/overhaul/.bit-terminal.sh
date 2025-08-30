#!/usr/bin/env bash
set -euo pipefail

CFG=".bit-terminal.json"
LOG_DIR=".bit-terminal_logs"
mkdir -p "$LOG_DIR"

if [[ ! -f "$CFG" ]]; then
  cat > "$CFG" <<'JSON'
{
  "version": "1",
  "env": {
    "FORCE_COLOR": "1"
  },
  "preflight": [
    "echo 'Preflight: tools check'",
    "docker --version || true",
    "gh --version || true",
    "yq --version || true",
    "jq --version || true"
  ],
  "commands": [
    { "name": "lint", "run": "echo 'lint ok'" },
    { "name": "unit", "run": "echo 'unit tests ok'" },
    { "name": "pkg",  "run": "echo 'packaging ok'" }
  ]
}
JSON
fi

echo "[.bit-terminal] Using $CFG"
export $(jq -r '.env | to_entries | map("\(.key)=\(.value)") | .[]' "$CFG")

run_block() {
  local label="$1"; shift
  local arr_json="$1"; shift
  local idx=0
  echo "$arr_json" | jq -c '.[]' | while read -r item; do
    idx=$((idx+1))
    local name; name="$(jq -r '.name // ("step-" + ($idx|tostring))' --argjson idx "$idx" <<<"$item" 2>/dev/null || echo "step-$idx")"
    local cmd;  cmd="$(jq -r '.run' <<<"$item")"
    echo "[$label][$idx] $name → $cmd"
    bash -lc "$cmd" | tee "$LOG_DIR/$label-$idx-$name.log"
  done
}

# Run preflight then commands
PRE="$(jq -c '.preflight // [] | to_entries | map({name: ("pre-" + (.key|tostring)), run: .value})' "$CFG")"
run_block "preflight" "$PRE"
CMDS="$(jq -c '.commands // []' "$CFG")"
run_block "commands" "$CMDS"

echo "[.bit-terminal] Completed. Logs in $LOG_DIR/"
