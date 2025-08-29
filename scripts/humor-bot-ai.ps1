# Purpose: Cross-platform preflight for humor override files, paths, and hygiene.
# Fails fast with actionable CI messages. Warns on CRLF usage.

[CmdletBinding()]
param(
  [string]$RepoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '..')).Path
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Write-Note([string]$msg) {
  Write-Host "[humor-bot] $msg"
}

function Assert-Exists($path, $type) {
  if (-not (Test-Path -LiteralPath $path)) {
    throw "Missing required $type: $path"
  }
  Write-Note "$type OK: $path"
}

try {
  Write-Note "OS = $([System.Runtime.InteropServices.RuntimeInformation]::OSDescription)"
  Write-Note "RepoRoot = $RepoRoot"

  $requiredFiles = @(
    "src/ai/advanced-reasoning-core/logic-exe.lisp",
    "src/ai/advanced-reasoning-core/humor-classifier.lisp",
    "src/ai/advanced-reasoning-core/humor_injection_ai_override.lisp",
    "config/humor-modules.manifest.lisp"
  )

  $requiredDirs = @(
    "logs",
    "scripts",
    "src/ai/advanced-reasoning-core",
    "config"
  )

  foreach ($rel in $requiredFiles) {
    Assert-Exists (Join-Path $RepoRoot $rel) "File"
  }

  foreach ($relDir in $requiredDirs) {
    Assert-Exists (Join-Path $RepoRoot $relDir) "Directory"
  }

  # Warn on CRLF line endings (non-destructive)
  $lfFiles = Get-ChildItem -LiteralPath (Join-Path $RepoRoot "src") -Recurse -Include *.lisp -File
  foreach ($f in $lfFiles) {
    $bytes = [System.IO.File]::ReadAllBytes($f.FullName)
    $crlf = 0
    for ($i = 1; $i -lt $bytes.Length; $i++) {
      if ($bytes[$i - 1] -eq 13 -and $bytes[$i] -eq 10) { $crlf++ }
    }
    if ($crlf -gt 0) {
      Write-Note "Warning: CRLF detected in $($f.FullName) (~$crlf lines). Prefer LF."
    }
  }

  Write-Note "✅ Preflight checks passed."
  exit 0
}
catch {
  Write-Error "[humor-bot] $_"
  exit 1
}
