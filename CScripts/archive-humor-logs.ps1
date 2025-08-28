# Archive daily eruption logs for audit and replay
$date = Get-Date -Format yyyyMMdd
Copy-Item "logs/ai-humor-eruption.log" "logs/ai-humor-eruption-$date.log"
Write-Host "Log archived as ai-humor-eruption-$date.log"
