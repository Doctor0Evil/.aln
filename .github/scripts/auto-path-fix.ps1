# .github/scripts/auto-path-fix.ps1
$workflowDir = ".github"
$workflowsDir = "$workflowDir/workflows"
# Find workflow YAML files with backslash in the path
Get-ChildItem -Path $workflowDir -Recurse -Filter "*.yml" | Where-Object {
    $_.FullName -match '\\'
} | ForEach-Object {
    $newPath = $_.FullName -replace '\\', '/'
    $newDir = Split-Path $newPath -Parent
    if (-not (Test-Path $newDir)) {
        New-Item -ItemType Directory -Path $newDir -Force | Out-Null
    }
    Move-Item $_.FullName $newPath -Force
    Write-Host "[AUTO-FIX] Moved $($_.FullName) -> $newPath"
}
