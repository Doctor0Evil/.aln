#!/usr/bin/env python3
"""
Bit.Hub-Compliant: get_pending_workflows.py

Purpose:
    Fetch pending GitHub Actions workflows for a given repository,
    with full compliance to Bit.Hub's security, audit, and governance policies.

Key Compliance Features:
    - No hard-coded secrets; uses GITHUB_TOKEN from env.
    - Repo URL and owner/name pulled from .bit/manifest.json.
    - Structured JSON logging to .bithub/reports/.
    - API schema validation before processing.
    - Retry/backoff for transient errors.
    - Exit codes for CI/CD gating.
"""

import json
import os
import sys
import time
from pathlib import Path
from typing import Any, Dict, List
import requests

# === CONFIG ===
MANIFEST_PATH = Path(".bit/manifest.json")
REPORTS_DIR = Path(".bithub/reports")
SCHEMA_PATH = Path(".bit/schemas/pending_workflows.schema.json")
API_URL_TEMPLATE = "https://api.github.com/repos/{owner}/{repo}/actions/runs"

# === LOAD MANIFEST ===
if not MANIFEST_PATH.exists():
    sys.stderr.write(f"[ERROR] Manifest not found at {MANIFEST_PATH}\n")
    sys.exit(2)

with MANIFEST_PATH.open() as f:
    manifest = json.load(f)

OWNER = manifest.get("owner")
REPO = manifest.get("repo")
if not OWNER or not REPO:
    sys.stderr.write("[ERROR] Manifest missing 'owner' or 'repo'\n")
    sys.exit(2)

# === AUTH ===
TOKEN = os.getenv("GITHUB_TOKEN")
if not TOKEN:
    sys.stderr.write("[ERROR] GITHUB_TOKEN not set in environment\n")
    sys.exit(2)

# === SCHEMA VALIDATION ===
try:
    import jsonschema
except ImportError:
    sys.stderr.write("[WARN] jsonschema not installed; skipping schema validation\n")
    jsonschema = None

if SCHEMA_PATH.exists() and jsonschema:
    with SCHEMA_PATH.open() as f:
        schema = json.load(f)
else:
    schema = None

# === HTTP SESSION ===
session = requests.Session()
session.headers.update({
    "Authorization": f"Bearer {TOKEN}",
    "Accept": "application/vnd.github+json",
    "User-Agent": "BitHub-WorkflowAuditor/1.0"
})

# === RETRY CONFIG ===
MAX_RETRIES = 3
BACKOFF_SECONDS = 5

def fetch_pending_runs() -> List[Dict[str, Any]]:
    """Fetch pending workflow runs from GitHub API."""
    url = API_URL_TEMPLATE.format(owner=OWNER, repo=REPO)
    params = {"status": "in_progress"}  # 'queued' can be added if needed
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            resp = session.get(url, params=params, timeout=15)
            if resp.status_code == 200:
                data = resp.json()
                if schema and jsonschema:
                    jsonschema.validate(instance=data, schema=schema)
                return [
                    run for run in data.get("workflow_runs", [])
                    if run.get("status") in ("queued", "in_progress")
                ]
            else:
                sys.stderr.write(f"[WARN] Attempt {attempt}: HTTP {resp.status_code} — {resp.text}\n")
        except (requests.RequestException, jsonschema.ValidationError) as e:
            sys.stderr.write(f"[ERROR] Attempt {attempt} failed: {e}\n")
        time.sleep(BACKOFF_SECONDS * attempt)
    return []

def write_report(pending_runs: List[Dict[str, Any]]) -> None:
    """Write structured report to .bithub/reports/."""
    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    report_path = REPORTS_DIR / "pending_workflows.json"
    report = {
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "repo": f"{OWNER}/{REPO}",
        "pending_count": len(pending_runs),
        "pending_runs": [
            {
                "id": run.get("id"),
                "name": run.get("name"),
                "event": run.get("event"),
                "status": run.get("status"),
                "created_at": run.get("created_at"),
                "html_url": run.get("html_url")
            }
            for run in pending_runs
        ]
    }
    with report_path.open("w") as f:
        json.dump(report, f, indent=2)
    print(f"[INFO] Report written to {report_path}")

def main() -> None:
    sys.stdout.write(f"[INFO] Fetching pending workflows for {OWNER}/{REPO}\n")
    pending = fetch_pending_runs()
    write_report(pending)
    if pending:
        sys.stdout.write(f"[INFO] Found {len(pending)} pending workflows\n")
        sys.exit(1)  # Non-zero to signal pending work if gating
    else:
        sys.stdout.write("[INFO] No pending workflows found\n")
        sys.exit(0)

if __name__ == "__main__":
    main()
