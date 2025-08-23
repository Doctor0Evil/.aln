# Ensure scripts/ and modules/ directories exist
$scriptDir = "scripts"
$moduleDir = "modules"

if (-not (Test-Path $scriptDir)) { New-Item -Path $scriptDir -ItemType Directory | Out-Null }
if (-not (Test-Path $moduleDir)) { New-Item -Path $moduleDir -ItemType Directory | Out-Null }

# Main utility script
$mainScript = Join-Path $scriptDir "init.ps1"
@"
Write-Host 'ALN PowerShell Init Script Running'
# Insert your workflow/CI/CD bootstrapping logic here.
"@ | Out-File -Encoding utf8 $mainScript -Force

# Example module
$moduleFile = Join-Path $moduleDir "ALNCore.psm1"
@"
function Initialize-ALNCore {
    Write-Host 'ALNCore module initialization completed.'
}
Export-ModuleMember -Function Initialize-ALNCore
"@ | Out-File -Encoding utf8 $moduleFile -Force

# Example task script
$taskScript = Join-Path $scriptDir "do-task.ps1"
@"
Write-Output 'This is a sample PowerShell task file.'
"@ | Out-File -Encoding utf8 $taskScript -Force

Write-Host "✅ All standard ALN PowerShell files populated:"
Get-ChildItem -Recurse -Include *.ps1,*.psm1 -Path $scriptDir, $moduleDir | Select-Object -ExpandProperty FullName
