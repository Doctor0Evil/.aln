<#
.SYNOPSIS
  Executes all ALN correction scripts listed in a manifest, with OS‑aware logic and audit logging.

.DESCRIPTION
  Reads a manifest file (`correction-manifest.json`) to determine which scripts to run and in what order.
  Runs each safely in its own scope, logs start/stop and any failures, and preserves per‑script logs for audit.

.NOTES
  Author: XboxTeeJay & Copilot
  Requires: PowerShell 7+
#>

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$scriptRoot    = Split-Path -Parent $MyInvocation.MyCommand.Definition
$manifestPath  = Join-Path $scriptRoot 'correction-manifest.json'
$sessionLog    = Join-Path $scriptRoot 'audit-session.log'

# Logging helper
function Write-Log {
    param(
        [string]$Message,
        [string]$Level = "INFO"
    )
    $timestamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    $line = "[$timestamp] [$Level] $Message"
    Write-Host $line
    Add-Content -Path $sessionLog -Value $line
}

Write-Log "=== Correction run started ==="

# --- OS Detection ---
$isWindowsOS = $IsWindows
$isLinuxOS   = $IsLinux
$isMacOS     = $IsMacOS

if ($isWindowsOS) { Write-Log "Detected OS: Windows" }
elseif ($isLinuxOS) { Write-Log "Detected OS: Linux" }
elseif ($isMacOS) { Write-Log "Detected OS: macOS" }
else { Write-Log "Unknown OS" "WARN" }

# --- Load manifest ---
if (-not (Test-Path $manifestPath)) {
    throw "Manifest file not found at: $manifestPath"
}

try {
    $manifest = Get-Content $manifestPath -Raw | ConvertFrom-Json
} catch {
    throw "Failed to parse manifest JSON: $($_.Exception.Message)"
}

if (-not $manifest.scripts -or $manifest.scripts.Count -eq 0) {
    Write-Log "No scripts listed in manifest." "WARN"
    exit 0
}

$failures = @()

# --- Execute each script ---
foreach ($scriptEntry in $manifest.scripts) {
    $scriptFile = Join-Path $scriptRoot $scriptEntry.name
    if (-not (Test-Path $scriptFile)) {
        Write-Log "Script not found: $($scriptEntry.name)" "ERROR"
        $failures += "$($scriptEntry.name): missing"
        continue
    }

    $scriptLog = Join-Path $scriptRoot ("log-" + [IO.Path]::GetFileNameWithoutExtension($scriptEntry.name) + ".txt")
    Write-Log ">>> Starting $($scriptEntry.name)"

    try {
        & $scriptFile *>&1 | Tee-Object -FilePath $scriptLog
        $exitCode = $LASTEXITCODE
        if ($exitCode -ne 0) {
            Write-Log "Script failed with exit code $exitCode" "ERROR"
            $failures += "$($scriptEntry.name): exit code $exitCode"
        } else {
            Write-Log "Script completed successfully."
        }
    } catch {
        Write-Log "Unhandled exception in $($scriptEntry.name): $($_.Exception.Message)" "ERROR"
        $failures += "$($scriptEntry.name): exception"
    }

    Write-Log "<<< Finished $($scriptEntry.name)"
}

# --- Summary ---
if ($failures.Count -gt 0) {
    Write-Log "One or more scripts failed:`n$($failures -join "`n")" "ERROR"
    throw "Corrections completed with errors."
} else {
    Write-Log "All scripts completed successfully."
}

Write-Log "=== Correction run finished ==="
