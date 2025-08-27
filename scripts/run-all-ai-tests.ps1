<#
.SYNOPSIS
  Runs AI/NPC pipeline scripts as defined in ai-manifest.json with audit logging.

.DESCRIPTION
  Each script is run in isolation with its output captured to a per-script log.
  A master audit-session.log captures the run order, timestamps, and failures.

.NOTES
  Requires PowerShell 7+
#>

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$scriptRoot   = Split-Path -Parent $MyInvocation.MyCommand.Definition
$manifestPath = Join-Path $scriptRoot 'ai-manifest.json'
$sessionLog   = Join-Path $scriptRoot 'audit-session.log'

function Write-Log {
    param([string]$Message,[string]$Level="INFO")
    $ts = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    $entry = "[$ts] [$Level] $Message"
    Write-Host $entry
    Add-Content -Path $sessionLog -Value $entry
}

Write-Log "=== AI/NPC test run started ==="

if (-not (Test-Path $manifestPath)) {
    throw "Manifest not found: $manifestPath"
}

try {
    $manifest = Get-Content $manifestPath -Raw | ConvertFrom-Json
} catch {
    throw "Failed to parse manifest: $($_.Exception.Message)"
}

$failures = @()
foreach ($item in $manifest.scripts) {
    $scriptFile = Join-Path $scriptRoot $item.name
    if (-not (Test-Path $scriptFile)) {
        Write-Log "Missing script: $($item.name)" "ERROR"
        $failures += "$($item.name): missing"
        continue
    }

    $logFile = Join-Path $scriptRoot ("log-" + [IO.Path]::GetFileNameWithoutExtension($item.name) + ".txt")
    Write-Log ">>> Starting $($item.name)"
    try {
        & $scriptFile *>&1 | Tee-Object -FilePath $logFile
        if ($LASTEXITCODE -ne 0) {
            Write-Log "$($item.name) failed with exit code $LASTEXITCODE" "ERROR"
            $failures += "$($item.name): exit code $LASTEXITCODE"
        } else {
            Write-Log "$($item.name) completed successfully."
        }
    } catch {
        Write-Log "Exception in $($item.name): $($_.Exception.Message)" "ERROR"
        $failures += "$($item.name): exception"
    }
    Write-Log "<<< Finished $($item.name)"
}

if ($failures.Count -gt 0) {
    Write-Log "Failures:`n$($failures -join "`n")" "ERROR"
    throw "One or more AI/NPC tests failed."
}

Write-Log "=== AI/NPC test run finished ==="
