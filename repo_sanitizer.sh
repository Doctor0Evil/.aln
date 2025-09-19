#!/bin/bash
# repo_sanitizer.sh — Alliance Workflow Sorter
set -e

ROOT=$(pwd)

# 1. Create clean structure
mkdir -p clean/{core,workflows,compliance,scripts,assets,sandbox}

# 2. Detect & route files
find "$ROOT" -type f | while read f; do
  case "$f" in
    *.aln|*.ini|*.json) cp "$f" clean/core/ ;;
    *.yml|*.lisp) cp "$f" clean/workflows/ ;;
    *.rego|*compliance*|*comply*) cp "$f" clean/compliance/ ;;
    *.ps1|*.sh|*.bat|*.py) cp "$f" clean/scripts/ ;;
    *.svg|*.png|*.meta) cp "$f" clean/assets/ ;;
    *slopbucket*|*lolcode*|*banter*) cp "$f" clean/sandbox/ ;;
  esac
done

# 3. Deduplicate
fdupes -rdN clean/  # requires fdupes

# 4. Generate manifest
find clean -type f | sort | sha256sum > clean/repo-manifest.aln
