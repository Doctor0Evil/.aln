#!/bin/sh
# commit-msg: Enforces positivity!
msg=$(cat "$1")
if echo "$msg" | grep -iE 'fix|bug|fail|error|broken'; then
  echo "😢 Sad words detected! Please use positive language in commit messages."
  exit 1
fi
exit 0
