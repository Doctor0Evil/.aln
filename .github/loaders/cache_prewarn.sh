#!/usr/bin/env bash
set -euo pipefail
MANIFEST=".bit/bitrunners.manifest.yml"
SECONDARY_ENABLED=$(yq '.runner.cache.secondary.enabled' "$MANIFEST")
[[ "$SECONDARY_ENABLED" != "true" ]] && exit 0

ROOT=".bithub/cache"
mkdir -p "$ROOT"
# Restore language tarballs if present (ns driver)
for t in node.tgz python.tgz dotnet.tgz java.tgz; do
  [[ -f "$ROOT/$t" ]] || continue
  case "$t" in
    node.tgz)   mkdir -p ~/.npm node_modules; tar -xzf "$ROOT/$t" -C . ;;
    python.tgz) mkdir -p ~/.cache/pip;        tar -xzf "$ROOT/$t" -C . ;;
    dotnet.tgz) mkdir -p ~/.nuget/packages;   tar -xzf "$ROOT/$t" -C . ;;
    java.tgz)   mkdir -p ~/.m2/repository;    tar -xzf "$ROOT/$t" -C . ;;
  esac
done
