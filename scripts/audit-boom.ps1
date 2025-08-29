# ===============================================================
# AUDIT BOOM - STACK OVERFILL HANDLER for Immutable Audit Pile
# ===============================================================
# Notifies operators if pile size breaches hard cap. Call externally
# from Lisp audit-boom-action or as a routine stack monitor.

Write-Host "`n[!!! AUDIT PILE BOOM !!!]"
Write-Host "Immutable audit stack file/variable has exceeded its hard threshold."
Write-Host "Operator action is now required. Further pile growth is temporarily halted."
Write-Host "Consider archiving, rotating, or inspecting the pile immediately."
Write-Host "[ALERT][STACK_OVERFILL][IMMUTABLE][BOOM]"
Start-Sleep -Seconds 2
exit 100
