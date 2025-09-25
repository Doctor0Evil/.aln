# Real-Time NPC State/Behavior Monitor
while ($true) {
    $npcs = Get-Content .\npcs\active_npcs.json | ConvertFrom-Json
    foreach ($npc in $npcs) {
        # Output status/behavior state for each
        Write-Host ("NPC: {0}, State: {1}, Aggro: {2}, Health: {3}" -f `
            $npc.Name, $npc.CurrentState, $npc.AggressionLevel, $npc.Health)
        # [Add hooks for triggers/responders here]
    }
    Start-Sleep -Seconds 4
}
