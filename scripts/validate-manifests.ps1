$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

$runningOnWindows = $IsWindows
$runningOnLinux   = $IsLinux
$runningOnMacOS   = $IsMacOS

Write-Host "=== [validate-manifests.ps1] Starting ==="

# Example: validate all manifest JSON files
Get-ChildItem -Recurse -Filter '*.json' -File | ForEach-Object {
    try {
        $null = Get-Content $_.FullName -Raw | ConvertFrom-Json
        Write-Host "Valid JSON: $($_.FullName)"
    } catch {
        Write-Warning "Invalid JSON in: $($_.FullName) - $($_.Exception.Message)"
        exit 1
    }
}
