# Randomly erupt a live humor event from db (spontaneous)

$humorDbPath = "data/database.birth.ai.humor.sqlite"
$jokeIdx = sqlite3 $humorDbPath "SELECT COUNT(*) FROM humor_jokes;"
$rand = Get-Random -Minimum 1 -Maximum ($jokeIdx + 1)
$jokeText = sqlite3 $humorDbPath "SELECT joke FROM humor_jokes WHERE id=$rand;"
Write-Host "Humor Eruption: $jokeText" -ForegroundColor Yellow
Add-Content "logs/ai-humor-eruption.log" "$(Get-Date -Format o): $jokeText"
