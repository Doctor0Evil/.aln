# ALN_CI_Remediation.psm1
$Global:LogFile = "$PSScriptRoot\ALN_CI_Remediation.log"
$Global:ComplianceStamp = (Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ")

function Write-Log {
    param([string]$Message,[string]$Level = "INFO")
    $entry = "[$(Get-Date -Format 'u')] [$Level] $Message"
    Add-Content -Path $Global:LogFile -Value $entry
    Write-Host $entry
}

function Exit-WithCode { param([int]$Code) ; Write-Log "Exiting with code $Code" ; exit $Code }

function Test-BinaryHash {
    param([string]$Path,[string]$ExpectedHash)
    Write-Log "Validating binary hash for $Path"
    if (-Not (Test-Path $Path)) { Write-Log "File not found: $Path" "ERROR" ; Exit-WithCode 1 }
    $actual = (Get-FileHash $Path -Algorithm SHA256).Hash
    if ($actual -ne $ExpectedHash) { Write-Log "Hash mismatch for $Path" "ERROR" ; Exit-WithCode 2 }
    Write-Log "Binary hash verified for $Path"
}

function Update-DockerImageTags {
    param([string]$DockerfilePath)
    Write-Log "Standardizing Docker image tags in $DockerfilePath"
    (Get-Content $DockerfilePath) -replace 'FROM (.*):.*', 'FROM `$1:latest' | Set-Content $DockerfilePath
}

function Apply-RegoPolicies {
    param([string]$PolicyPath,[string[]]$TargetEnvs)
    foreach ($env in $TargetEnvs) {
        Write-Log "Applying Rego policy to $env"
        Copy-Item $PolicyPath -Destination "$env\policies" -Force
        $result = & opa eval --data "$env\policies" --input test.json 'data.compliance.allow'
        if ($LASTEXITCODE -ne 0 -or $result -notmatch "true") { Write-Log "Policy validation failed for $env" "ERROR" ; Exit-WithCode 3 }
    }
}

function Test-ALNSchema {
    param([string]$SchemaFile,[string]$SampleProject)
    Write-Log "Validating ALN schema $SchemaFile"
    & aln validate --schema $SchemaFile --project $SampleProject
    if ($LASTEXITCODE -ne 0) { Write-Log "Schema validation failed" "ERROR" ; Exit-WithCode 4 }
}

function Test-QuantumCryptoSync {
    param([string]$DeployedLibPath,[string]$BuildLibPath)
    Write-Log "Checking quantum crypto library sync"
    $hashDeployed = (Get-FileHash $DeployedLibPath -Algorithm SHA256).Hash
    $hashBuild = (Get-FileHash $BuildLibPath -Algorithm SHA256).Hash
    if ($hashDeployed -ne $hashBuild) { Write-Log "Quantum crypto library mismatch" "ERROR" ; Exit-WithCode 5 }
}

function Add-ComplianceStamp {
    param([string]$TargetFile)
    Write-Log "Stamping compliance metadata into $TargetFile"
    Add-Content -Path $TargetFile -Value "`n# ComplianceStamp: $Global:ComplianceStamp"
}

Export-ModuleMember -Function * -Alias *
