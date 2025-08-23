#!/usr/bin/env pwsh
<#
.SYNOPSIS
  Corrects ALN files in a null‑safe, CI‑friendly way.

.DESCRIPTION
  Recursively scans the repository for `.aln` files,
  applies corrections, and writes results back in place.
  Will fail the run if no `.aln` files are found.
#>

# Stop on first error
$ErrorActionPreference = 'Stop'

# Get the repo root (script's parent directory 2 levels up)
$RepoRoot = Resolve-Path (Join-Path $PSScriptRoot '..') | Select-Object -ExpandProperty Path

Write-Host "🔍 Searching for .aln files under: $RepoRoot"

# Find all .aln files
$alnFiles = Get-ChildItem -Path $RepoRoot -Recurse -Filter '*.aln' -File -ErrorAction SilentlyContinue

if (-not $alnFiles -or $alnFiles.Count -eq 0) {
    Write-Error "❌ No .aln files found. Check your repo structure or runner checkout path."
    exit 1
}

foreach ($file in $alnFiles) {
    try {
        Write-Host "✏️ Processing: $($file.FullName)"

        # Read the entire file content
        $content = Get-Content -LiteralPath $file.FullName -Raw

        # 🛠️ Perform your correction logic here
        # Example: Trim trailing whitespace from each line
        $corrected = $content -split "`r?`n" | ForEach-Object { $_.TrimEnd() } | Out-String

        # Write corrected content back to the file
        Set-Content -LiteralPath $file.FullName -Value $corrected -NoNewline
    }
    catch {
        Write-Error "⚠️ Failed processing $($file.FullName): $_"
        exit 1
    }
}

Write-Host "✅ Completed correction on $($alnFiles.Count) file(s)."
