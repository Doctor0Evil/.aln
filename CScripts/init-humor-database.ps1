# Create humor.ai DB, seed initial records

$humorDbPath = "data/database.birth.ai.humor.sqlite"
if (!(Test-Path "data")) { New-Item -ItemType Directory -Path "data" }
if (Test-Path $humorDbPath) { Remove-Item $humorDbPath }
sqlite3 $humorDbPath "CREATE TABLE humor_jokes(id INTEGER PRIMARY KEY, joke TEXT, style TEXT, origin TEXT, created DATETIME DEFAULT CURRENT_TIMESTAMP);"
sqlite3 $humorDbPath "INSERT INTO humor_jokes(joke,style,origin) VALUES ('Why was the computer cold? Because it left its Windows open.','pun','machine.init');"
sqlite3 $humorDbPath "INSERT INTO humor_jokes(joke,style,origin) VALUES ('AI walks into a bar. Bartender: Can you pass the Turing Test? AI: Only if you serve strong logic.','meta','machine.init');"
Write-Host "AI humor database created and seeded: $humorDbPath"
