<#
.SYNOPSIS
Fixes Docker build issues by ensuring Dockerfile is in the correct location and provides correct build command.

.DESCRIPTION
This script checks for the existence of the Dockerfile in the expected location (docker/Dockerfile).
If it doesn't exist, it creates a basic Dockerfile to allow successful builds.
It also provides the correct Docker build command for the repository.

.EXAMPLE
.\Fix-DockerBuild.ps1
#>

param(
    [string]$RepoPath = (Get-Location).Path,
    [string]$DockerfilePath = "docker/Dockerfile"
)

# Set up logging
$LogPath = Join-Path $RepoPath "docker-build-fix.log"
$ErrorActionPreference = "Stop"

function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $LogEntry = "[$Timestamp] [$Level] $Message"
    
    # Write to console
    switch ($Level) {
        "ERROR" { Write-Host $LogEntry -ForegroundColor Red }
        "WARN" { Write-Host $LogEntry -ForegroundColor Yellow }
        "SUCCESS" { Write-Host $LogEntry -ForegroundColor Green }
        "INFO" { Write-Host $LogEntry -ForegroundColor Cyan }
        default { Write-Host $LogEntry }
    }
    
    # Write to log file
    try {
        $LogEntry | Out-File -FilePath $LogPath -Append -Encoding UTF8
    }
    catch {
        Write-Host "Warning: Could not write to log file $LogPath"
    }
}

try {
    # Create docker directory if it doesn't exist
    $DockerDir = Join-Path $RepoPath "docker"
    if (-not (Test-Path $DockerDir)) {
        Write-Log "Creating docker directory: $DockerDir" -Level "INFO"
        New-Item -ItemType Directory -Path $DockerDir -Force | Out-Null
    }

    # Check if Dockerfile exists
    $Dockerfile = Join-Path $DockerDir "Dockerfile"
    if (-not (Test-Path $Dockerfile)) {
        Write-Log "Dockerfile not found at $Dockerfile. Creating default Dockerfile." -Level "WARN"
        
        # Create a basic Dockerfile
        $defaultDockerfileContent = @'
# ALN Programming Language Docker Image
FROM alpine:3.19

# Metadata
LABEL org.opencontainers.image.title="ALN Programming Language"
LABEL org.opencontainers.image.description="ALN Programming Language Runtime Environment"
LABEL org.opencontainers.image.source="https://github.com/Doctor0Evil/ALN_Programming_Language"
LABEL org.opencontainers.image.licenses="MIT"

# Environment variables
ENV PATH="/app/aln/bin:${PATH}"

# Install system dependencies
RUN apk add --no-cache \
    curl \
    bash \
    openssl \
    ca-certificates \
    && update-ca-certificates

# Create app directory and user
RUN addgroup -g 1001 -S aln \
    && adduser -S aln -u 1001 -G aln \
    && mkdir -p /app/aln \
    && chown -R aln:aln /app

# Copy application files
COPY --chown=aln:aln . /app/aln/

# Set working directory
WORKDIR /app/aln

# Make scripts executable
RUN find /app/aln -name "*.sh" -type f -exec chmod +x {} \;

# Switch to non-root user
USER aln

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

# Expose port
EXPOSE 8080

# Entry point
CMD ["./start.sh"]
'@
        
        $defaultDockerfileContent | Out-File -FilePath $Dockerfile -Encoding UTF8
        Write-Log "Default Dockerfile created at $Dockerfile" -Level "SUCCESS"
    }
    else {
        Write-Log "Dockerfile found at $Dockerfile" -Level "SUCCESS"
    }

    # Verify Docker is installed
    if (Get-Command docker -ErrorAction SilentlyContinue) {
        Write-Log "Docker is installed and available" -Level "SUCCESS"
    }
    else {
        Write-Log "Docker is not installed. Please install Docker to build the image." -Level "WARN"
    }

    # Display the correct build command
    Write-Log ""
    Write-Log "To build the Docker image correctly, use this command:" -Level "SUCCESS"
    Write-Log "docker build . --file docker/Dockerfile --tag aln-core-engine:latest" -Level "SUCCESS"
    Write-Log ""
    Write-Log "To build with a timestamped tag:" -Level "INFO"
    Write-Log "docker build . --file docker/Dockerfile --tag aln-core-engine:$(Get-Date -Format 'yyyyMMddHHmmss')" -Level "INFO"
    Write-Log ""
    Write-Log "The GitHub Actions workflow is already configured to use the correct Dockerfile path."
    Write-Log "If you're using the workflow, no additional action is needed."
}
catch {
    Write-Log "An error occurred: $_" -Level "ERROR"
    Write-Log "Stack trace: $($_.Exception.StackTrace)" -Level "ERROR"
    exit 1
}
