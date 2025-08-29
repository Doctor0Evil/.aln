# for.laughter.psm1 - Dynamic Humor Persona Injection Module
# Drop-in module for fun and DevOps banter, "shitsandgiggles.com" edition

function Invoke-MarkStoneQuip {
    param([string]$Context)
    $joke = @(
        "If this doesn't work, just say it's a feature.",
        "Blame the CI runner; it's probably getting coffee.",
        "In DevOps, nothing is broken—just 'unexpectedly creative'.",
        "Mark's Law: For every big push, there's an equal and opposite facepalm."
    ) | Get-Random
    Write-Host "[Mark Stone] $($joke) | $Context"
}

function Invoke-TroyWilliamsJibe {
    param([string]$Context)
    $quip = @(
        "I debug in public because therapy is expensive.",
        "This build failed so fast, it qualified for the Olympics.",
        "My code compiles because my keyboard is haunted.",
        "YAML: Yet Another Mistake-Laden Language"
    ) | Get-Random
    Write-Host "[Troy Williams] $($quip) | $Context"
}

function Laughter-Inject {
    param([string]$Task = "make the pipeline giggle")
    Invoke-MarkStoneQuip "Injecting humor for: $Task"
    Invoke-TroyWilliamsJibe "$Task, because why not?"
    Write-Host "[Injected for.laughter] - All systems nominally hilarious."
}

function Laughter-Debug {
    param([string]$Info)
    Write-Host "[DEBUG] $Info (If you laugh, it was intentional.)"
}

Export-ModuleMember -Function * # Exports all functions

# Example: (in your init step)
#     Import-Module "$PSScriptRoot\for.laughter.psm1"
#     Laughter-Inject "your workflow step"
