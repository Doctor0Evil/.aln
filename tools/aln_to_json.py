#!/usr/bin/env python3
"""
aln_to_json.py – Convert .aln manifest into structured JSON (tasks aware)
"""

import re, json, sys
from pathlib import Path

def parse_aln(text: str) -> dict:
    manifest = {
        "tasks": [],
        "signature_verification": "placeholder"
    }

    # match task_automation(...) calls
    task_pattern = re.compile(
        r'task_automation\(\s*"(?P<desc>[^"]+)"\s*,\s*(?P<args>[^)]*)\)',
        re.MULTILINE
    )

    for match in task_pattern.finditer(text):
        desc = match.group("desc")
        args = match.group("args")
        params = []

        # parse arguments like schedule="24h", action="...", filters=...
        for token in re.split(r",\s*", args):
            if not token: 
                continue
            if "=" in token:
                key, val = token.split("=", 1)
                params.append(f"{key.strip()}={val.strip()}")
            else:
                params.append(token.strip())

        manifest["tasks"].append({
            "description": desc,
            "params": params
        })

    return manifest


def main():
    if len(sys.argv) < 2:
        print("Usage: aln_to_json.py <file.aln>")
        sys.exit(1)

    input_path = Path(sys.argv[1])
    text = input_path.read_text()

    manifest = parse_aln(text)
    out_path = input_path.with_suffix(".manifest_task.json")

    out_path.write_text(json.dumps(manifest, indent=2))
    print(f"[+] Wrote manifest: {out_path}")


if __name__ == "__main__":
    main()
