Write-Host "=== [combat-sim.ps1] Starting ==="

$global:LASTEXITCODE = 0

# Resolve path to run-combat-sim.ps1 relative to this script's folder
$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Definition
$target     = Join-Path $scriptRoot 'run-combat-sim.ps1'

if (-not (Test-Path $target)) {
    throw "Target script not found: $target"
}

# Run the combat simulation
& $target

if ($LASTEXITCODE -ne 0) {
    throw "Combat simulation failed with exit code $LASTEXITCODE"
}

Write-Host "Combat sim complete."
