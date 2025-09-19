#!/usr/bin/env python3
# aln_to_json.py — convert ci-package.aln -> manifest.json (minimal but robust)
import json, re, sys, pathlib

def parse_aln(file_path):
    txt = open(file_path).read()
    # naive but robust extraction for expected blocks
    # extract package name/version
    m = re.search(r'manifest\s+([A-Za-z0-9_\-]+)\s+v([0-9\.]+)\s*{', txt)
    package = m.group(1) if m else "unknown"
    version = m.group(2) if m else "0.0"
    # extract targets block content
    targ_block = re.search(r'targets\s*:\s*\[([^\]]+)\]', txt, re.S)
    targets = []
    if targ_block:
        items = re.findall(r'\{([^\}]+)\}', targ_block.group(1), re.S)
        for it in items:
            nm = re.search(r'name\s*:\s*"([^"]+)"', it)
            tri = re.search(r'triple\s*:\s*"([^"]+)"', it)
            feats = re.findall(r'"([^"]+)"', re.search(r'features\s*:\s*\[([^\]]*)\]', it).group(1)) if re.search(r'features\s*:\s*\[', it) else []
            targets.append({"name": nm.group(1) if nm else None, "triple": tri.group(1) if tri else None, "features": feats})
    return {"package": package, "version": version, "targets": targets}

if __name__ == "__main__":
    p = sys.argv[1] if len(sys.argv)>1 else "ci-package.aln"
    manifest = parse_aln(p)
    print(json.dumps(manifest))
