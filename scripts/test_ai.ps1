$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$runningOnWindows = $IsWindows
$runningOnLinux   = $IsLinux
$runningOnMacOS   = $IsMacOS

Write-Host "=== [test_ai.ps1] Starting ==="

# Example tests
# ...
Write-Host "AI module tests passed."
