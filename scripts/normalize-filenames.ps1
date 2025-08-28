$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$runningOnWindows = $IsWindows
$runningOnLinux   = $IsLinux
$runningOnMacOS   = $IsMacOS

Write-Host "=== [normalize-filenames.ps1] Starting ==="

# Example logic: normalise filenames to lowercase
Get-ChildItem -Recurse -File | ForEach-Object {
    $lower = $_.Name.ToLowerInvariant()
    if ($_.Name -ne $lower) {
        Rename-Item $_.FullName $lower
        Write-Host "Renamed $($_.Name) -> $lower"
    }
}
