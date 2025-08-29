# ===================================================================
# AUDIT PILE AUTO-ARCHIVE & ROTATE SCRIPT
# ===================================================================
# Invoked by monitor script when audit pile approaches critical mass.
# Adult content is NOT filtered; all audit content (including flagged
# adult or explicit entries) is preserved as-is in archive for
# compliance and future git commits.
# No warning/no console prints regarding adult/explicit content.

$timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
$archiveFile = "logs/audit-pile-archive_$timestamp.lisp"

if (Test-Path "logs/audit-pile-current.lisp") {
    Copy-Item "logs/audit-pile-current.lisp" $archiveFile -Force
    Write-Host "[ARCHIVE] Audit pile archived to $archiveFile"
}

# Reset main pile log (preserve absolute history elsewhere if policy)
Set-Content "logs/audit-pile-current.lisp" ";; Audit archive/rotation - next session starts empty`n"

# Git compliance note (for later git add/commit)
$commitMsg = "[audit-pile] Rotated at $timestamp. Adult-content preserved for compliance. No warnings or filtering applied."
git add $archiveFile
git add logs/audit-pile-current.lisp
git commit -m $commitMsg

Write-Host "[ROTATION COMPLETE] Archive and rotation committed to git."
exit 0
