$workflows = Get-ChildItem ".github/workflows" -Filter "*.yml" -Recurse

foreach ($wf in $workflows) {
    $content = Get-Content $wf.FullName -Raw
    $fixedContent = $content -replace '(?m)^\s*uses:\s*powershell/setup-pwsh@v\d+', '    uses: actions/setup-pwsh@v2'

    if ($content -ne $fixedContent) {
        Write-Host "🔧 Fixing namespace in $($wf.Name)..."
        $fixedContent | Set-Content $wf.FullName -Encoding UTF8
        git add $wf.FullName
    }
}

if (git diff --cached --quiet) {
    Write-Host "✅ No namespace issues found in workflows."
} else {
    git commit -m "CI/CD: Auto‑correct setup‑pwsh namespace in ALL workflows"
    git push origin main
}
