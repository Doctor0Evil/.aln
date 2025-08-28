# ===============================
# correct-aln-files.ps1 (ALN Standard, safe)
# ===============================
# Safely corrects ALN files:
#   • Normalizes line endings (CRLF on Windows, LF elsewhere).
#   • Removes trailing whitespace.
#   • Logs all actions to a unique audit-log per job/process.
# ===============================
$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

# --- Script Variables ---
$scriptName = $MyInvocation.MyCommand.Name
$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Definition

# Unique log file (by process) prevents race conditions in CI runners
$logFile = Join-Path $scriptRoot ("audit-log-" + $PID + ".txt")

# --- Ensure log exists before use ---
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

# --- OS Detection (system vars are read-only, just test) ---
$lineEnding = if ($IsWindows) { "`r`n" } else { "`n" }
if ($IsWindows)      { Write-Log "Detected OS: Windows" }
elseif ($IsLinux)    { Write-Log "Detected OS: Linux" }
elseif ($IsMacOS)    { Write-Log "Detected OS: macOS" }
else                 { Write-Log "Unknown OS detected" "WARN" }

# --- Main Correction Logic ---
try {
    $files = Get-ChildItem -Path $scriptRoot -Recurse -Include *.aln -File
    foreach ($file in $files) {
        if ($null -eq $file -or -not $file.PSObject.Properties.Match('FullName')) {
            Write-Log "Skipped an item without .FullName property" "WARN"
            continue
        }
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
# scripts/correct-aln-files.ps1
# ALN master corrections orchestrator for repo https://github.com/Doctor0Evil/ALN_Programming_Language.git

param([string]$TargetPath="aln-files/", [string]$LispModule="correct.files.lisp")

Write-Host "=== [ALN] Corrections Orchestrator Start ==="
Write-Host "Target path: $TargetPath"
Write-Host "Lisp module: $LispModule"

# Validate input path
if (!(Test-Path $TargetPath)) {
    Write-Error "Target ALN files folder missing: $TargetPath"
}

# Call Lisp to correct each ALN file
$alnFiles = Get-ChildItem -Path $TargetPath -Filter "*.aln.lisp" -File
foreach ($file in $alnFiles) {
    Write-Host "Correcting $($file.Name) via Lisp engine..."
    # This assumes a lisp binary 'aln-lisp' in PATH; adapt as needed
    $cmd = "aln-lisp --load $LispModule --eval '(correct.aln.syntax \"$($file.FullName)\")'"
    Write-Host "[DEBUG] Running: $cmd"
    $out = & bash -c "$cmd" 2>&1
    Write-Host "[LOG] Output: $out"
    Out-File -FilePath "scripts/correction-$($file.BaseName).log" -InputObject $out
}

Write-Host "=== [ALN] Corrections complete. ==="
exit 0
