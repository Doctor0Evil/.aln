# Blockchain-like secure checkpointing for ALN volatile files

$volatileFiles = Get-ChildItem "..\core","..\ai","..\game" -Include *.lisp,*.aln,*.txt -Recurse |
    Where-Object { (Get-Content $_.FullName) -match "random|hotpatch|madness|event-trigger|chaos" }

foreach ($file in $volatileFiles) {
    $content = Get-Content $file.FullName
    $hash = [System.BitConverter]::ToString((New-Object Security.Cryptography.SHA256Managed).ComputeHash([System.Text.Encoding]::UTF8.GetBytes($content))) -replace '-', ''
    $timestamp = Get-Date -Format o
    $block = @{
        File = $file.FullName
        Hash = $hash
        Timestamp = $timestamp
    }
    ConvertTo-Json $block | Add-Content "logs/blockchain-stabilization.log"
    Write-Host "Blockchain checkpoint: $($file.Name) @ $timestamp with hash $hash" -ForegroundColor Green
}

Write-Host "All volatile files checkpointed to blockchain log."
