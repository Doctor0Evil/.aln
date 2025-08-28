<#
.SYNOPSIS
  Executes all ALN correction scripts listed in a manifest, with OS‑aware logic and audit logging.

.DESCRIPTION
  Reads a manifest file (`correction-manifest.json`) to determine which scripts to run and in what order.
  Runs each script in its own scope, logs start/stop and failures, and preserves per‑script logs for audit.
  Works across Windows, Linux, and macOS (PowerShell 7+).

.NOTES
  Author: XboxTeeJay & Copilot
  Maintained by: Jacob Farmer
  Requires: PowerShell 7+
#>

# --- Safe Defaults ---
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

# --- Path Setup ---
$scriptRoot   = $PSScriptRoot   # Safe, consistent base for sub‑scripts
$manifestPath = Join-Path $scriptRoot 'correction-manifest.json'
$sessionLog   = Join-Path $scriptRoot 'audit-session.log'

# --- Logging Helper ---
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

# --- Begin Run ---
Write-Log "=== Correction run started ==="

# --- OS Detection (parser‑safe) ---
$osName = if ($IsWindows) { "Windows" }
elseif ($IsLinux)        { "Linux"   }
elseif ($IsMacOS)        { "macOS"   }
else                     { "Unknown" }

Write-Log "Detected OS: $osName"

# --- Load Manifest ---
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

# --- Execute Each Script ---
$failures = @()
foreach ($scriptEntry in $manifest.scripts) {
    $scriptName = $scriptEntry.name
    $scriptFile = Join-Path $scriptRoot $scriptName
    $scriptLog  = Join-Path $scriptRoot ("log-" + [IO.Path]::GetFileNameWithoutExtension($scriptName) + ".txt")

    if (-not (Test-Path $scriptFile)) {
        Write-Log "Script not found: ${scriptName}" "ERROR"
        $failures += "${scriptName}: missing"
        continue
    }

    Write-Log ">>> Starting ${scriptName}"
    try {
        # Run the script and tee stdout/stderr into a per‑script log
        & $scriptFile *>&1 | Tee-Object -FilePath $scriptLog

        $exitCode = $LASTEXITCODE
        if ($exitCode -ne 0) {
            Write-Log "Script failed with exit code $exitCode" "ERROR"
            $failures += "${scriptName}: exit code $exitCode"
        }
        else {
            Write-Log "Script completed successfully."
        }
    }
    catch {
        Write-Log "Unhandled exception in ${scriptName}: $($_.Exception.Message)" "ERROR"
        $failures += "${scriptName}: exception"
    }
    Write-Log "<<< Finished ${scriptName}"
}

# --- Summary ---
if ($failures.Count -gt 0) {
    Write-Log "One or more scripts failed:`n$($failures -join "`n")" "ERROR"
    throw "Corrections completed with errors."
}
else {
    Write-Log "All scripts completed successfully."
}

Write-Log "=== Correction run finished ==="
