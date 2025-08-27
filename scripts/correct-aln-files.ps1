<#
.SYNOPSIS
  Corrects ALN files with robust OS detection, error handling, and audit-friendly logging.

.DESCRIPTION
  This script runs safely in GitHub Actions (Windows, macOS, Linux) without clobbering built-in constants.
  It detects the OS, runs correction logic, and logs each step for audit purposes.

.NOTES
  Author: XboxTeeJay & Copilot
  Requires: PowerShell 7+
#>

# --- Safety & Logging Configuration ---
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$scriptName   = $MyInvocation.MyCommand.Name
$scriptRoot   = Split-Path -Parent $MyInvocation.MyCommand.Definition
$logFile      = Join-Path $scriptRoot "audit-log.txt"

# Timestamped logging helper
function Write-Log {
    param (
        [string]$Message,
        [string]$Level = "INFO"
    )
    $timestamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    $entry = "[$timestamp] [$Level] $Message"
    Write-Host $entry
    Add-Content -Path $logFile -Value $entry
}

Write-Log "=== [$scriptName] Starting ==="

# --- OS Detection (safe) ---
$isWindowsOS = $IsWindows
$isLinuxOS   = $IsLinux
$isMacOS     = $IsMacOS

if ($isWindowsOS) {
    Write-Log "Detected OS: Windows"
} elseif ($isLinuxOS) {
    Write-Log "Detected OS: Linux"
} elseif ($isMacOS) {
    Write-Log "Detected OS: macOS"
} else {
    Write-Log "Unknown OS detected" "WARN"
}

# --- Main Correction Logic ---
try {
    # Example: normalize line endings to LF
    Write-Log "Normalizing line endings to LF..."
    Get-ChildItem -Path $scriptRoot -Recurse -Include *.aln |
        ForEach-Object {
            $original = Get-Content $_.FullName -Raw
            $normalized = $original -replace "`r`n", "`n"
            if ($normalized -ne $original) {
                Set-Content -Path $_.FullName -Value $normalized -NoNewline
                Write-Log "Normalized line endings in: $($_.FullName)"
            }
        }

    # Example: remove trailing whitespace
    Write-Log "Removing trailing whitespace..."
    Get-ChildItem -Path $scriptRoot -Recurse -Include *.aln |
        ForEach-Object {
            $lines = Get-Content $_.FullName
            $cleaned = $lines | ForEach-Object { $_ -replace "\s+$", "" }
            if ($cleaned -ne $lines) {
                Set-Content -Path $_.FullName -Value $cleaned
                Write-Log "Trimmed trailing whitespace in: $($_.FullName)"
            }
        }

    Write-Log "All corrections completed successfully."
}
catch {
    Write-Log "Unhandled error: $($_.Exception.Message)" "ERROR"
    throw
}

Write-Log "=== [$scriptName] Finished ==="
