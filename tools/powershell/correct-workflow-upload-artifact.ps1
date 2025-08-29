# ==============================================================
# PowerShell Script: Correct Deprecated Github Action Versions
# For use with CI/CD runners (incl. Ubuntu 24.04 LTS)
# Purpose: Locate and auto-correct deprecated `actions/upload-artifact@v3`
#          in all workflow YAMLs, replacing with the latest v4 or above.
# ==============================================================

# Set the directory to search for workflow files
$workflowDir = ".github/workflows"

# Find all YAML workflow files
$workflowFiles = Get-ChildItem -Path $workflowDir -Recurse -Include *.yml,*.yaml

foreach ($file in $workflowFiles) {
    Write-Host "Scanning $($file.FullName)..."
    # Read file content
    $content = Get-Content $file.FullName -Raw

    # Check for deprecated actions/upload-artifact@v3 usage
    if ($content -match "actions/upload-artifact@v3") {
        # Replace v3 with v4 (or latest stable)
        $newContent = $content -replace "actions/upload-artifact@v3", "actions/upload-artifact@v4"
        Set-Content -Path $file.FullName -Value $newContent -NoNewline
        Write-Host "✅ Corrected: updated actions/upload-artifact to v4 in $($file.Name)"
    } else {
        Write-Host "No deprecated 'upload-artifact@v3' found in $($file.Name)"
    }
}

Write-Host "`n[COMPLETE] All workflow YAMLs scanned and corrected as needed."
Write-Host "Tip: Commit and push the fixed workflows to resolve CI errors."
