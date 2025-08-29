# powershell.profanity.ps1
# Censors profanity in input for logic safety in humor bots (customizable for comedy mode)

param (
    [string]$Text,
    [string]$BlacklistFile = "profanity-blacklist.txt"
)

# Load blacklist words (one per line, skip blanks/comments)
$profanities = Get-Content $BlacklistFile | Where-Object { $_.Trim() -and -not $_.StartsWith("#") }

# Function: censor profane words (replace with asterisks, keep first/last letters for humor)
function Censor-Profanity {
    param ([string]$inputText, [array]$badWords)
    $censored = $inputText
    foreach ($word in $badWords) {
        $pattern = "\b$($word)\b"
        $replacement = $word.Length -gt 2 ? "$($word.Substring(0,1))" + ('*' * ($word.Length-2)) + "$($word.Substring($word.Length-1,1))" : '*' * $word.Length
        $censored = [regex]::Replace($censored, $pattern, $replacement, "IgnoreCase")
    }
    return $censored
}

# Run filter
$SafeText = Censor-Profanity -inputText $Text -badWords $profanities

# Output (for workflow loop or humor logic core)
Write-Output $SafeText
