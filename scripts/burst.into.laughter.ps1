# burst.into.laughter.ps1
# ALN Persona Auto-Push with Dice Roll + Debug Trace
# Repo: https://github.com/Doctor0Evil/ALN_Programming_Language.git

param(
  [string]$ModulePath = "C:\Users\Hunter\ALN_Repo\src\Main\Personas\funny-persona-tier5.aln",
  [string]$Branch = "main",
  [string]$Email = "xboxteejaymcfarmer@gmail.com"
)

# Step 0: Roll the Dice!
$Roll = Get-Random -Minimum 1 -Maximum 7

# Step 1: Prepare Git
cd (Split-Path $ModulePath -Parent)
git add $ModulePath

# Step 2: Commit with humor event
$CommitMsg = "Add funny-persona (dice-roll $Roll): automated laugh burst!"
git commit -m $CommitMsg

# Step 3: Push to remote (ensure secure PAT in env:GITHUBPAT)
git push origin $Branch

# Step 4: Email outcome (pseudo - insert SMTP config if used)
$Subject = "ALN Persona Push: burst.into.laughter (Roll $Roll)"
$Body = @"
ALN GITHUB ALERT

Operation: burst.into.laughter.ps1
Module: funny-persona-tier5.aln
Dice Roll Result: $Roll
Timestamp: $(Get-Date -Format s)

Result:
Commit message: $CommitMsg
Branch pushed: $Branch
"@
# Example placeholder for real mail: Send-MailMessage -To $Email -Subject $Subject -Body $Body

# Step 5: Lisp-style Debug Output
Write-Host ";;; === POWERSHELL DEBUG CONSOLE DUMP ==="
Write-Host ";;; burst.into.laughter.ps1 executed"
Write-Host ">>> DICE ROLL VALUE: $Roll"
Write-Host ">>> Module: $ModulePath"
Write-Host ">>> Git branch: $Branch"
Write-Host ">>> Commit Message: '$CommitMsg'"
Write-Host ">>> Email Sent: $Email (summary only; configure SMTP for delivery)"
Write-Host ";;; === END DUMP ==="

# Optional: Burst of Laughter
$laughs = @("Ha!", "Haha!", "Bwahaha!", "Hehehe!", "Snrk!", "LOL!", "🤣")
Write-Host ("[ALN] burst.into.laughter: " + $laughs[$Roll-1])
