# scripts/correct-aln-files.ps1

$ErrorActionPreference = 'Stop'
Write-Host "=== [correct-aln-files.ps1] Starting ==="

try {
    # Detect platform
    $isWindows = $IsWindows
    $isLinux = $IsLinux
    $isMacOS = $IsMacOS

    $platform = if ($isWindows) { "Windows" }
                elseif ($isLinux) { "Linux" }
                elseif ($isMacOS) { "macOS" }
                else { "Unknown" }

    Write-Host "Platform detected: $platform"

    # Target directory
    $targetDir = Join-Path $PWD 'aln'
    if (-not (Test-Path $targetDir)) {
        Write-Warning "Target directory not found: $targetDir"
        exit 1
    }

    $files = Get-ChildItem -Path $targetDir -Filter '*.aln' -File
    if (-not $files) {
        Write-Warning "No .aln files found in $targetDir"
        exit 2
    }

    foreach ($file in $files) {
        Write-Host "Validating: $($file.Name)"
        $content = Get-Content $file.FullName -Raw

        if ($content -notmatch 'spec:') {
            Write-Warning "$($file.Name) missing 'spec:' header"
            exit 3
        }

        if (-not $isWindows) {
            $normalized = $content -replace "`r`n", "`n"
            Set-Content -Path $file.FullName -Value $normalized
            Write-Host "$($file.Name) normalized for Unix line endings"
        }
    }

    Write-Host "=== [correct-aln-files.ps1] Completed successfully ==="
    exit 0
}
catch {
    Write-Error "Unhandled error: $_"
    exit 99
}
