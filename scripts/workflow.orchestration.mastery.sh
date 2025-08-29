#!/bin/bash
# workflow.orchestration.mastery.sh
# Supercharged Git/ALN workflow automation—floor to ceiling and "screams.of.victory" style
set -e
FLOOR_LOG=".git/floor.log"
CEILING_LOG=".git/ceiling.log"
LOUD_BAT="hillarious.bat"
PROJECT_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
function echo_victory() {
  echo -e "\033[35m*** 'screams.of.victory' ***\033[0m"
}
function floor_to_ceiling() {
  git log --oneline | tee "$FLOOR_LOG"
  tail -n 13 "$FLOOR_LOG" | awk '{print "🎈    " $0 "    🎉"}' > "$CEILING_LOG"
  echo "Git history launched from $FLOOR_LOG to $CEILING_LOG"
}
function workflow_hillarious_bat() {
  echo "@echo off" > "$LOUD_BAT"
  echo "echo Ha! Windows bat but Linux workflow: Commit parade of good times." >> "$LOUD_BAT"
  echo "pause" >> "$LOUD_BAT"
}
function git_fix_vampire() {
  git config --global user.email "$(git config user.email 2>/dev/null || echo "not-set@nowhere.com")"
  git config --global commit.gpgsign true
  echo "Sleeping in coffin until dark... (waiting for GPG to bite again)"
}
function aln_build_and_deploy() {
  if [ -f "$PROJECT_ROOT/scripts/build.aln" ]; then
    echo "Running ALN build automation..."
    aln run "$PROJECT_ROOT/scripts/build.aln"
  else
    echo "No build.aln script found; skipping ALN build."
  fi
}
echo "== workflow.orchestration.mastery.sh activated =="
echo_victory
floor_to_ceiling
workflow_hillarious_bat
git_fix_vampire
aln_build_and_deploy
echo
echo "All glory to the GIT tower of *good times*: Workflow parade executed!"
echo_victory
echo "Run $LOUD_BAT in Windows for bonus laughs."
echo "Good dev times surely florished (and logs are in .git/)! 🦇"
chmod +x scripts/workflow.orchestration.mastery.sh
./scripts/workflow.orchestration.mastery.sh
