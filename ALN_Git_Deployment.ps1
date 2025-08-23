param(
    [string]$TargetDir = "$env:USERPROFILE\ALN_Programming_Language",
    [string]$RepoUrl = "https://github.com/Doctor0Evil/ALN_Programming_Language.git",
    [string]$Branch = "main",
    [int]$MaxRetries = 3,
    [int]$RetryDelaySeconds = 5,
    [string]$CommitMessage = "Automated deployment: ALN core enhancements & compliance updates [$((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))]",
    [string]$GitUserName = "Hunter",
    [string]$GitUserEmail = "hunter@example.com",
    [string]$GitHubToken = $env:GITHUB_TOKEN
)

$ErrorActionPreference = 'Stop'

try {
    # Start logging session
    $transcriptPath = Join-Path $env:TEMP "ALN_git_deployment_$(Get-Date -Format yyyyMMddHHmmss).log"
    if (Get-Command Stop-Transcript -ErrorAction SilentlyContinue) { Stop-Transcript -ErrorAction SilentlyContinue }
    Start-Transcript -Path $transcriptPath -Append

    # Create target directory if missing
    if (-not (Test-Path $TargetDir)) {
        New-Item -Path $TargetDir -ItemType Directory -Force | Out-Null
    }
    Set-Location -Path $TargetDir

    # Check GitHub token presence
    if ([string]::IsNullOrEmpty($GitHubToken)) {
        throw "GitHubToken is missing. Set environment variable GITHUB_TOKEN or pass as parameter."
    }

    # Compose authenticated repo URL safely with variable delimiters
    $authRepoUrl = $RepoUrl -replace '^https://', "https://${GitUserName}:${GitHubToken}@"

    # Configure Git user if not set
    try {
        $currentUserName = (git config user.name) -replace "`r`n", ""
        if ([string]::IsNullOrEmpty($currentUserName)) {
            git config --global user.name $GitUserName
        }
        $currentUserEmail = (git config user.email) -replace "`r`n", ""
        if ([string]::IsNullOrEmpty($currentUserEmail)) {
            git config --global user.email $GitUserEmail
        }
    } catch {
        # Ignore any errors in configuration
    }

    # Function to clone or clean repo
    function Ensure-GitRepository ($Path) {
        if (-not (Test-Path (Join-Path $Path ".git"))) {
            if ((Get-ChildItem -Path $Path -Force | Measure-Object).Count -gt 0) {
                Write-Host "Cleaning the folder '$Path' for fresh clone..." -ForegroundColor Yellow
                Get-ChildItem -Path $Path -Force -Recurse | Remove-Item -Force -Recurse -ErrorAction SilentlyContinue
            }
            Write-Host "Cloning repository from $RepoUrl ..." -ForegroundColor Green
            git clone --branch $Branch $authRepoUrl $Path | Out-Null
            if ($LASTEXITCODE -ne 0) {
                throw "Git clone failed. Check URL, token, and permissions."
            }
        }
        else {
            Write-Host "Existing repository found at $Path. Syncing..." -ForegroundColor Cyan
        }
    }

    # Pull latest changes with rebase
    function Pull-WithRebase {
        try {
            Write-Host "Pulling latest changes with rebase..." -ForegroundColor Blue
            git fetch origin | Out-Null
            git pull --rebase origin $Branch | Out-Null
            Write-Host "Pull and rebase successful." -ForegroundColor Green
        } catch {
            Write-Warning "Pull with rebase failed. Continuing anyway.`nError: $_"
        }
    }

    # Commit staged changes if any
    function Commit-AllChanges ($Message) {
        Write-Host "Adding changes to staging..." -ForegroundColor Blue
        git add -A | Out-Null

        git diff --cached --quiet
        if ($LASTEXITCODE -eq 0) {
            Write-Host "No changes to commit." -ForegroundColor Gray
            return $false
        }
        Write-Host "Committing changes: $Message" -ForegroundColor Green
        git commit -m $Message | Out-Null
        if ($LASTEXITCODE -ne 0) {
            Write-Warning "Git commit failed but continuing..."
            return $false
        }
        Write-Host "Commit created." -ForegroundColor Green
        return $true
    }

    # Push with retry logic
    function Push-WithRetry ($BranchName, $MaxAttempts, $DelaySeconds) {
        $attempt = 0
        while ($attempt -lt $MaxAttempts) {
            try {
                Write-Host "Pushing to origin/$BranchName (Attempt $($attempt+1) of $MaxAttempts)..." -ForegroundColor Blue
                git push origin $BranchName | Out-Null
                if ($LASTEXITCODE -eq 0) {
                    Write-Host "Push succeeded." -ForegroundColor Green
                    return $true
                }
                throw "Git push failed with exit code $LASTEXITCODE"
            } catch {
                $attempt++
                if ($attempt -ge $MaxAttempts) {
                    throw "Push failed after $MaxAttempts attempts. Last error: $_"
                }
                Write-Warning "Push error: $_. Retrying after $DelaySeconds seconds..."
                Start-Sleep -Seconds $DelaySeconds
            }
        }
    }

    # Main execution
    Write-Host "Starting automated deployment..." -ForegroundColor Magenta
    Write-Host "Target directory: $TargetDir"
    Write-Host "Repository URL: $RepoUrl"
    Write-Host "Branch: $Branch"

    Ensure-GitRepository -Path $TargetDir
    Pull-WithRebase

    $commitDone = Commit-AllChanges -Message $CommitMessage

    if ($commitDone) {
        Push-WithRetry -BranchName $Branch -MaxAttempts $MaxRetries -DelaySeconds $RetryDelaySeconds
    }
    else {
        Write-Host "Nothing to push. Skipping push step." -ForegroundColor Gray
    }

    Write-Host "`n✅ Deployment completed successfully." -ForegroundColor Green
    Write-Host "Logs saved at: $transcriptPath"
}
catch {
    Write-Error "[FATAL] Script failed: $_"
}
finally {
    if (Get-Command Stop-Transcript -ErrorAction SilentlyContinue) {
        Stop-Transcript -ErrorAction SilentlyContinue
    }
}
