Write-Host "[ALN BOOTSTRAP] Initializing ALN pseudo-env…"
if (-Not (Test-Path "$env:USERPROFILE\.alnconfig")) {
    New-Item -Path "$env:USERPROFILE\.alnconfig" -ItemType File -Force | Out-Null
    Write-Host "[CONFIG] Created new ALN config at $env:USERPROFILE\.alnconfig"
}
Set-Content -Path "$env:USERPROFILE\.alnconfig" -Value "ALN_ENV=active;SESSION=non-admin;START=$(Get-Date)"
Write-Host "[ALN CONFIG] Session marked active."
Write-Host "[ALN BOOTSTRAP] Launching ALN Terminal—pretend prompt now enabled."
Start-Process powershell -ArgumentList "-NoLogo", "-NoExit", "-Command", "echo Welcome to the ALN pseudo-terminal!"
