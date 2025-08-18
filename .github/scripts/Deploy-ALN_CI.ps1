# --- Namespace Auto-Heal (Local) ---
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

if (-not (git diff --cached --quiet)) {
    git commit -m "CI/CD: Auto‑correct setup‑pwsh namespace (local preflight)"
    git push origin main
} else {
    Write-Host "✅ No namespace issues found."
}

# --- Continue bootstrap ---
Write-Host "🚀 Running ALN CI/CD bootstrap..."
# your existing bootstrap steps here
