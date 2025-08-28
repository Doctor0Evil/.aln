# scripts/humor-bot-ai.ps1
# Purpose: Cross-platform preflight for humor override files, paths, and basic hygiene.
# Fails fast with actionable messages for CI.

[CmdletBinding()]
param(
  [string]$RepoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..')).Path
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Write-Note([string]$msg){ Write-Host "[humor-bot] $msg" }

try {
  Write-Note "OS=$([System.Runtime.InteropServices.RuntimeInformation]::OSDescription)"
  Write-Note "RepoRoot=$RepoRoot"

  $expected = @(
    "src/ai/advanced-reasoning-core/logic-exe.lisp",
    "src/ai/advanced-reasoning-core/humor-classifier.lisp",
    "src/ai/advanced-reasoning-core/humor_injection_ai_override.lisp",
    "config/humor-modules.manifest.lisp"
  )

  foreach ($rel in $expected) {
    $path = Join-Path $RepoRoot $rel
    if (-not (Test-Path -LiteralPath $path)) {
      throw "Missing required file: $rel"
    }
    Write-Note "Found $rel"
  }

  # Basic directory availability
  foreach ($relDir in @("logs","scripts","src/ai/advanced-reasoning-core","config")) {
    $dir = Join-Path $RepoRoot $relDir
    if (-not (Test-Path -LiteralPath $dir)) {
      throw "Missing required directory: $relDir"
    }
    Write-Note "Dir OK: $relDir"
  }

  # Non-destructive line-ending check (warn-only)
  $lfFiles = Get-ChildItem -LiteralPath (Join-Path $RepoRoot "src") -Recurse -Include *.lisp -File
  foreach ($f in $lfFiles) {
    $bytes = [System.IO.File]::ReadAllBytes($f.FullName)
    $crlf = 0; $lf = 0
    for ($i=0; $i -lt $bytes.Length; $i++) {
      if ($bytes[$i] -eq 10) { $lf++ }
      if ($i -gt 0 -and $bytes[$i-1] -eq 13 -and $bytes[$i] -eq 10) { $crlf++ }
    }
    if ($crlf -gt 0) {
      Write-Note "Warning: CRLF detected in $($f.FullName) (CRLF lines ≈ $crlf). Prefer LF in repo."
    }
  }

  Write-Note "Preflight checks passed."
  exit 0
}
catch {
  Write-Error "[humor-bot] $_"
  exit 1
}
