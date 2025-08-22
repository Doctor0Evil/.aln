# ALN File Correction Script - Non-admin, safe, and consistent
# Runs in GitHub Actions without elevated permissions

# Define correction patterns (customize as needed)
$correctionPatterns = @{
    # Add file extensions and correction patterns here
    "README.md" = {
        $content = Get-Content $_ -Raw
        if (-not $content.Contains("ALN Universal Language Framework")) {
            $content = "## ALN Universal Language Framework v9.0.8`n`n" + $content
            Set-Content $_ -Value $content
            Write-Host "Corrected: $($_.Name)"
        }
    }
    
    "ALN_Programming_Language/README.md" = {
        $content = Get-Content $_ -Raw
        if (-not $content.Contains("# ALN Universal Language Framework")) {
            $content = "# ALN Universal Language Framework v9.0.8`n`n" + $content
            Set-Content $_ -Value $content
            Write-Host "Corrected: $($_.Name)"
        }
    }
    
    "*.aln" = {
        $content = Get-Content $_ -Raw
        if (-not $content.Contains("@DOCUMENTATION")) {
            $newContent = "@DOCUMENTATION {`n@TITLE ""ALN Universal Language Framework""`n@VERSION ""9.0.8""`n@SUMMARY ""Adaptive, AI-native, self-evolving programming language and runtime for universal system and application development.""`n}`n`n" + $content
            Set-Content $_ -Value $newContent
            Write-Host "Corrected: $($_.Name)"
        }
    }
    
    "*.cs" = {
        $content = Get-Content $_ -Raw
        if (-not $content.Contains("ALN_Programming_Language")) {
            $newContent = "/*`n * ALN Universal Language Framework`n * Copyright (c) 2025 ALN Universal Language Framework`n */`n`n" + $content
            Set-Content $_ -Value $newContent
            Write-Host "Corrected: $($_.Name)"
        }
    }
    
    "*.ps1" = {
        $content = Get-Content $_ -Raw
        if (-not $content.Contains("ALN Universal Language Framework")) {
            $newContent = "# ALN Universal Language Framework - File Correction Script`n# Copyright (c) 2025 ALN Universal Language Framework`n`n" + $content
            Set-Content $_ -Value $newContent
            Write-Host "Corrected: $($_.Name)"
        }
    }
    
    "*.md" = {
        $content = Get-Content $_ -Raw
        if (-not $content.Contains("ALN Universal Language Framework")) {
            $newContent = "## ALN Universal Language Framework v9.0.8`n`n" + $content
            Set-Content $_ -Value $newContent
            Write-Host "Corrected: $($_.Name)"
        }
    }
}

# Find all files that need correction
$filesToCorrect = Get-ChildItem -Recurse -Include *.aln, *.cs, *.ps1, *.md -Exclude .git, .github, node_modules, __pycache__ -File

foreach ($file in $filesToCorrect) {
    # Check if this file type matches any correction pattern
    $matchedPattern = $null
    foreach ($pattern in $correctionPatterns.Keys) {
        if ($file.Name -like $pattern) {
            $matchedPattern = $pattern
            break
        }
    }
    
    if ($matchedPattern) {
        & $correctionPatterns[$matchedPattern] $file
    }
}

Write-Host "ALN file correction complete. Total files corrected: $($filesToCorrect.Count)"
