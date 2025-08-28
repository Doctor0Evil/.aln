# test/test-harness.ps1

$ErrorActionPreference = 'Stop'
Write-Host "=== [test-harness.ps1] Starting ALN ingestion test ==="

# Create sandbox
$testRoot = Join-Path $PWD 'test-aln-ingestion'
if (Test-Path $testRoot) {
    Remove-Item -Recurse -Force $testRoot
}
New-Item -ItemType Directory -Path $testRoot | Out-Null

# Simulated .aln files
Set-Content -Path (Join-Path $testRoot 'valid.aln') -Value @"
spec: Character
name: Arkon
class: Warrior
"@

Set-Content -Path (Join-Path $testRoot 'invalid.aln') -Value @"
name: Vex
class: Rogue
"@

Set-Content -Path (Join-Path $testRoot 'empty.aln') -Value ""

Write-Host "Created test files:"
Get-ChildItem -Path $testRoot

# Run validation script
$validationScript = Join-Path $PWD 'scripts\correct-aln-files.ps1'
if (-not (Test-Path $validationScript)) {
    throw "Validation script not found: $validationScript"
}

Push-Location $testRoot
try {
    & $validationScript
    $exitCode = $LASTEXITCODE
    Write-Host "Validation script exited with code: $exitCode"
}
catch {
    Write-Error "Validation failed: $_"
}
finally {
    Pop-Location
}

Write-Host "=== [test-harness.ps1] Completed ==="
