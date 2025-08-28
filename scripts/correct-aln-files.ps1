# ===============================
# correct-aln-files.ps1 (ALN Standard)
# ===============================
# Safely corrects ALN files:
#   • Normalizes line endings (CRLF on Windows, LF elsewhere).
#   • Removes trailing whitespace.
#   • Logs all actions to audit-log.txt with rotation.
# ===============================
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

# --- Script Variables ---
$scriptName = $MyInvocation.MyCommand.Name
$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Definition
$logFile    = Join-Path $scriptRoot "audit-log.txt"

# --- Ensure audit-log.txt exists ---
if (-not (Test-Path $logFile)) {
    New-Item -ItemType File -Path $logFile -Force | Out-Null
}

# --- Log Rotation (max 10 MB) ---
if ((Test-Path $logFile) -and ((Get-Item $logFile).Length -gt 10MB)) {
    $backupName = "$logFile.$(Get-Date -Format 'yyyyMMddHHmmss').bak"
    Rename-Item $logFile $backupName -Force
    New-Item -ItemType File -Path $logFile -Force | Out-Null
}

# --- Logging Helper ---
function Write-Log {
    param(
        [string]$Message,
        [string]$Level = "INFO"
    )
    $timestamp = (Get-Date).ToString("yyyy-MM-dd HH:mm:ss")
    $entry = "[$timestamp] [$Level] $Message"
    Write-Host $entry
    Add-Content -Path $logFile -Value $entry
}

Write-Log "=== [$scriptName] Starting ==="

# --- OS Detection ---
$isWindowsOS = $IsWindows
$isLinuxOS   = $IsLinux
$isMacOS     = $IsMacOS
$lineEnding = if ($isWindowsOS) { "`r`n" } else { "`n" }

if ($isWindowsOS)      { Write-Log "Detected OS: Windows" }
elseif ($isLinuxOS)    { Write-Log "Detected OS: Linux" }
elseif ($isMacOS)      { Write-Log "Detected OS: macOS" }
else                   { Write-Log "Unknown OS detected" "WARN" }

# --- Main Correction Logic ---
try {
    $files = Get-ChildItem -Path $scriptRoot -Recurse -Include *.aln -File
    foreach ($file in $files) {
        $original = Get-Content $file.FullName -Raw
        $normalized = $original -replace "(`r`n|`n|`r)", $lineEnding
        $normalizedNoWS = ($normalized -split "`n") | ForEach-Object {
            $_ -replace "\s+$", ""
        }
        $final = $normalizedNoWS -join $lineEnding
        if ($final -ne $original) {
            Set-Content -Path $file.FullName -Value $final -Encoding UTF8
            Write-Log "Corrected: $($file.FullName)"
        }
    }
    Write-Log "All corrections completed successfully."
}
catch {
    Write-Log "Unhandled error: $($_.Exception.Message)" "ERROR"
    throw
}

Write-Log "=== [$scriptName] Finished ==="
