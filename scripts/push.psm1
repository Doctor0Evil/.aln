param(
    [Parameter(Mandatory=$true)]
    [string]$PAT = "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"
)
#--- BEGIN DEFAULT FULL SOLUTION TEMPLATE, ALL VARS/LOGGING/ERRORS INCLUDED ---#
function Get-Timestamp { Get-Date -Format "yyyy-MM-dd HH:mm:ss" }
function Log-Message { param([string]$Message) $ts=Get-Timestamp; Write-Host "$ts - $Message"; Add-Content -Path "$env:USERPROFILE\aln-devops.log" -Value "$ts - $Message" }

$repoUrl              = "https://github.com/Doctor0Evil/ALN_Programming_Language.git"
$localPath            = "$env:USERPROFILE\ALN_Repo"
$branch               = "main"
$ghVersion            = "2.58.0"
$cliInstallDir        = "$env:USERPROFILE\cli-gh"
$zipName = "gh_${ghVersion}_windows_amd64.zip"
$zipPath              = "$env:TEMP\$zipName"
$logFile              = "$env:USERPROFILE\aln-devops.log"
$workflowDir          = "$localPath\.github\workflows"

# Preflight cleanup and logging
if (!(Test-Path $logFile)) { New-Item $logFile -Type File -Force | Out-Null }
Log-Message "Starting error-proof ALN DevOps script execution - full context"

# Clean invalid Scoop installations
$scoopDir = "$env:USERPROFILE\scoop"
if (Test-Path $scoopDir) { Log-Message "Cleaning up legacy scoop dir..."; Remove-Item $scoopDir -Recurse -Force -ErrorAction SilentlyContinue }
Log-Message "Legacy Scoop removed if present."

# Install Scoop if needed
if (!(Get-Command scoop -ErrorAction SilentlyContinue)) {
    Log-Message "Installing Scoop for isolated user (no admin, retry-safe)..."
    $installTry = 0; $maxRetry = 3
    do {
        try {
            Set-ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
            Invoke-Expression (Invoke-RestMethod get.scoop.sh)
            Log-Message "Scoop installed successfully."
            $installTry = $maxRetry + 1
        } catch {
            $installTry++
            Log-Message "Scoop install failed (attempt $installTry): $($_.Exception.Message)"
            Start-Sleep -Seconds 8
            if ($installTry -ge $maxRetry) { throw "Scoop install failed after $maxRetry attempts." }
        }
    } while ($installTry -le $maxRetry)
} else { Log-Message "Scoop detected and OK." }

# Install portable GitHub CLI if needed
if (!(Get-Command gh -ErrorAction SilentlyContinue)) {
    Log-Message "Downloading/installing GitHub CLI $ghVersion no-admin version..."
    if (!(Test-Path $cliInstallDir)) { New-Item -ItemType Directory $cliInstallDir | Out-Null }
    $url = "https://github.com/cli/cli/releases/download/v$ghVersion/gh_${ghVersion}_windows_amd64.zip"
    Invoke-WebRequest -Uri $url -OutFile $zipPath -UseBasicParsing -ErrorAction Stop
    Expand-Archive -LiteralPath $zipPath -DestinationPath $cliInstallDir -Force
    Remove-Item $zipPath -Force
    $cliBin = Get-ChildItem -Path $cliInstallDir -Recurse -Filter gh.exe | Select-Object -First 1
    $cliBinFolder = Split-Path $cliBin.FullName
    $env:PATH = "$cliBinFolder;$env:PATH"
    # Persist user PATH for next session
    $userPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    if ($userPath -notlike "*$cliBinFolder*") {
        [Environment]::SetEnvironmentVariable("PATH", "$cliBinFolder;$userPath", "User")
    }
    Log-Message "GitHub CLI installed at $cliBinFolder and PATH updated."
} else { Log-Message "GitHub CLI detected and on PATH." }

# Set PAT as user-scope env variable (ALNGITHUBPAT) and in-session GITHUB_TOKEN
[Environment]::SetEnvironmentVariable("ALNGITHUBPAT", $PAT, "User")
$env:GITHUB_TOKEN = $PAT
Log-Message "PAT set in environment variable for scripts."

# Authenticate GH CLI with PAT
try {
    Log-Message "Authenticating gh with PAT..."
    echo $PAT | gh auth login --with-token
} catch {
    Log-Message "Authentication with gh failed: $($_.Exception.Message)"; throw
}
Log-Message "GH CLI authenticated."

# Ensure git is installed (via Scoop if missing)
if (!(Get-Command git.exe -ErrorAction SilentlyContinue)) {
    Log-Message "Git not detected, installing via Scoop (non-admin)..."
    scoop install git
    if (!(Get-Command git.exe -ErrorAction SilentlyContinue)) { throw "Git install failed." }
    Log-Message "Git installed."
} else { Log-Message "Git detected OK." }

# Clone or sync repo with authentication
if (!(Test-Path $localPath)) {
    Log-Message "Cloning ALN repo $repoUrl to $localPath"
    git clone --depth 1 "https://Doctor0Evil:${PAT}@github.com/Doctor0Evil/ALN_Programming_Language.git" $localPath
    if ($LASTEXITCODE -ne 0) { throw "Git clone failed. Check PAT validity and network." }
    Log-Message "Repo cloned."
} else {
    Set-Location $localPath
    Log-Message "Pulling with remote authenticated via PAT..."
    git remote set-url origin "https://Doctor0Evil:${PAT}@github.com/Doctor0Evil/ALN_Programming_Language.git"
    git fetch origin
    git checkout $branch
    git pull --rebase origin $branch
    if ($LASTEXITCODE -ne 0) { throw "Authenticated git pull failed. Check PAT perms." }
    Log-Message "Repo up-to-date."
}

# Prepare .github/workflows directory and fix any workflow YAML permission or trigger issues
if (!(Test-Path $workflowDir)) { New-Item -Path $workflowDir -ItemType Directory -Force | Out-Null }

# Create/repair workflow file for robust permissions and correct action versions
$workflowYml = @'
name: ALN Build & Push
on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]
  workflow_dispatch:

permissions:
  contents: write
  packages: write
  deployments: write

jobs:
  build-push:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3
      - name: Login to GHCR
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.ALNPAT }}
      - name: Build and Push Docker Image
        uses: docker/build-push-action@v6
        with:
          context: .
          file: ./Dockerfile
          push: true
          tags: ghcr.io/${{ github.repository_owner }}/aln-core-engine:latest,ghcr.io/${{ github.repository_owner }}/aln-core-engine:${{ github.sha }}
          build-args: |
            ALNVERSION=${{ github.sha }}
            BUILDDATE=${{ github.event.created_at }}
      - name: Upload artifact (optional)
        uses: actions/upload-artifact@v3
        with:
          name: aln-core-engine-${{ github.sha }}
          path: ./.dist
'@
Set-Content -Path "$workflowDir\build-and-push.yml" -Value $workflowYml -Encoding utf8
Log-Message "Workflow build-and-push.yml deployed fixed permissions."

# Set ALN PAT as repo secret using GH CLI
try {
    gh secret set ALNPAT --repo Doctor0Evil/ALN_Programming_Language --body "$PAT" --visibility all
    Log-Message "Repo secret ALNPAT set/updated."
} catch {
    Log-Message "Could not set repo secret ALNPAT: $_"
}

# Trigger workflow run to verify setup (non-blocking error)
try {
    gh workflow run build-and-push.yml --ref $branch --repo Doctor0Evil/ALN_Programming_Language
    Log-Message "GitHub Actions workflow triggered on $branch."
} catch {
    Log-Message "Warning: Could not trigger workflow via gh." 
}

# Try login Docker if available (not required, skip if fail)
try {
    if (Get-Command docker -ErrorAction SilentlyContinue) {
        Log-Message "Performing Docker login to GHCR..."
        $DockerUser = "Doctor0Evil"
        $proc = Start-Process -FilePath "docker" -ArgumentList "login ghcr.io -u $DockerUser --password-stdin" -PassThru -NoNewWindow -RedirectStandardInput Pipe
        $proc.StandardInput.WriteLine($PAT)
        $proc.StandardInput.Close()
        $proc.WaitForExit()
        Log-Message "Docker login to ghcr.io attempted. ExitCode: $($proc.ExitCode)"
    }
} catch { Log-Message "Docker login skipped: $($_.Exception.Message)" }

# Final repo status and .gitignore protection
if (!(Test-Path "$localPath\.gitignore")) {
    Set-Content -Path "$localPath\.gitignore" -Value ".env`n.log" -Encoding utf8
    Log-Message "Added .gitignore to protect .env and log files."
}
if (!(Test-Path "$localPath\.env")) {
    Set-Content -Path "$localPath\.env" -Value "GITHUBPAT=$PAT" -Encoding utf8
    Log-Message "Created .env with GITHUBPAT for local scripts (DO NOT COMMIT)."
}

# Log script completion and notify by email (optional)
Log-Message "ALN DevOps automation - Initialization completed successfully. Review $logFile for all actions."
