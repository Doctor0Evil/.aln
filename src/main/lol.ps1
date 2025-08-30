#!/usr/bin/env pwsh
<#
.SYNOPSIS
  ALNFantasia Booster Script – orchestrates Bit.Hub compliance, ingestion, bot spawn, and fun modules.

.DESCRIPTION
  Reads master.policy.aln.yml for rules, runs preflight fixes, corrects workflows, resyncs data,
  triggers NPC/bot events, and optionally runs humour modules with allowed profanity.
#>

$ErrorActionPreference = 'Stop'
$ledger = ".bithub/ledger/booster.log"
New-Item -ItemType Directory -Force -Path (Split-Path $ledger) | Out-Null

function Log-Event($event, $detail) {
    $ts = (Get-Date).ToString("o")
    Add-Content -Path $ledger -Value (@{ts=$ts;event=$event;detail=$detail} | ConvertTo-Json -Compress)
    Write-Host "[$event] $detail"
}

# 1. Load master policy
if (-not (Test-Path ".bit-actions/master.policy.aln.yml")) {
    throw "Master policy file missing."
}
$policy = yq -o=json '. ' ".bit-actions/master.policy.aln.yml" | ConvertFrom-Json

# 2. Preflight namespace & extension normalization
Log-Event "PREFLIGHT" "Checking .bit/.bitrunners/.gitrunners structure"
# … reuse your fixed preflight logic here, using $policy.required and $policy.forbid …

# 3. Workflow correction/autopatch
Log-Event "WORKFLOW_FIX" "Scanning workflows for compliance"
# … iterate .github/workflows, apply safelist/replace_map from $policy.actions …

# 4. Data ingestion resync
Log-Event "DATA_RESYNC" "Triggering ingestion pipeline"
# … call your ingestion scripts or APIs …

# 5. NPC/Bot spawn hooks
Log-Event "BOT_SPAWN" "Launching NPC/bot events"
# … call aln-bot-summon or equivalent, if installed …

# 6. Optional humour module
if ($policy.modules.whitelist -contains "aln://modules/fun.lol") {
    $allowed = $policy.actions.allowed_profanity -join ", "
    Log-Event "FUN_LOL" "Running fun.lol.exe with allowed profanity: $allowed"
    Write-Host "(>*_*)> 🎉 <(*_*<)  Fucking.legendary! Erupting with massive bot army."
}

Log-Event "COMPLETE" "Booster run finished"
git update-index --chmod=+x lol.ps1
chmod +x lol.ps1
