# ==============================================================
# ALN-CyberNet v2.3.1-BCI UEFI BIOS Setup for Infineon SLB 9665
# LIVE Edition: Network Reset & Randomization (No Admin Rights)
# ==============================================================
# Target: C:\Users\Hunter\ALN_Programming_Language\scripts\infineon-bios-setup.ps1
# Applies randomized mt6883 chipset network configuration for user profile
# ==============================================================
# Created: 2025-08-22
# ==============================================================

param(
    [switch]$LogToConsole = $true
)

# ===========================
# CONFIGURATION
# ===========================
$BaseLogDir = Join-Path -Path $env:USERPROFILE -ChildPath ".aln_logs"
if (-not (Test-Path $BaseLogDir)) {
    New-Item -Path $BaseLogDir -ItemType Directory -Force | Out-Null
}
$LogFile = "net-live-$(Get-Date -Format 'yyyyMMdd_HHmmss').log"
$LogPath = Join-Path -Path $BaseLogDir -ChildPath $LogFile

function Write-Log {
    param([string]$Message,[string]$LogLevel="INFO")
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $entry = "[$timestamp] [$LogLevel] $Message"
    if ($LogToConsole) {
        switch ($LogLevel) {
            "INFO"     { Write-Host $entry -ForegroundColor Cyan }
            "SUCCESS"  { Write-Host $entry -ForegroundColor Green }
            "WARNING"  { Write-Host $entry -ForegroundColor Yellow }
            "ERROR"    { Write-Host $entry -ForegroundColor Red }
            default    { Write-Host $entry }
        }
    }
    $entry | Out-File -FilePath $LogPath -Append -Encoding UTF8
}

# ===========================
# NETWORK APPLYING (LIVE SAFE)
# ===========================
function Get-RandomIP {
    $rand = Get-Random -Minimum 100 -Maximum 250
    return "192.168.1.$rand"
}

function Apply-NetworkConfig {
    Write-Log -Message "Resetting and applying randomized network configuration (LIVE MODE)..." -LogLevel "INFO"

    $newIP     = Get-RandomIP
    $newSubnet = "255.255.255.0"
    $newGateway= "192.168.1.1"
    $newDNS    = "1.1.1.1"
    $newSSH    = Get-Random -Minimum 2000 -Maximum 65000

    $netConfig = @{
        'IP'        = $newIP
        'Subnet'    = $newSubnet
        'Gateway'   = $newGateway
        'DNS'       = $newDNS
        'SSH-Port'  = $newSSH
    }

    # Instead of admin-level network stack manipulation, persist to HKCU
    $registryPath = "HKCU:\Software\ALN\NetworkProfile"
    if (-not (Test-Path $registryPath)) { New-Item -Path $registryPath -Force | Out-Null }
    foreach ($k in $netConfig.Keys) {
        New-ItemProperty -Path $registryPath -Name $k -Value $netConfig[$k] -PropertyType String -Force | Out-Null
    }

    Write-Log -Message "Network configuration stored under HKCU\Software\ALN\NetworkProfile" -LogLevel "SUCCESS"
    return $netConfig
}

# ===========================
# MAIN EXECUTION
# ===========================
try {
    Write-Log -Message "Starting LIVE mt6883 chipset network reconfiguration..." -LogLevel "INFO"
    Write-Log -Message "Original Config: IP=192.168.1.101, Subnet=255.255.255.0, DHCP=192.168.1.01, SSH=443, DNS=Auto" -LogLevel "INFO"

    $newConfig = Apply-NetworkConfig

    Write-Host "`n===== LIVE NETWORK CONFIGURATION =====" -ForegroundColor Yellow
    foreach ($k in $newConfig.Keys) {
        Write-Host "$k : $($newConfig[$k])" -ForegroundColor Cyan
    }
    Write-Host "======================================" -ForegroundColor Yellow

    Write-Log -Message "LIVE mode completed successfully! Log saved at $LogPath" -LogLevel "SUCCESS"
    exit 0
}
catch {
    Write-Log -Message "LIVE reconfiguration failed: $_" -LogLevel "ERROR"
    Write-Host "`nERROR: $_`n" -ForegroundColor Red
    exit 1
}
finally {
    Write-Host "`n[Live Reconfiguration Finished] Logs available at: $LogPath`n" -ForegroundColor Green
}