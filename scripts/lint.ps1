$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$runningOnWindows = $IsWindows
$runningOnLinux   = $IsLinux
$runningOnMacOS   = $IsMacOS

Write-Host "=== [lint.ps1] Starting ==="

# Example style check
Get-ChildItem -Recurse -Include *.aln | ForEach-Object {
    # Insert lint logic here
    Write-Host "Linted $($_.FullName)"
}
