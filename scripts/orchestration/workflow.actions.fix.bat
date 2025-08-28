@echo off
REM Fix and trigger GitHub Actions jobs for ALN
echo [FIX] Starting workflow path-correction…
powershell -File "%~dp0auto-path-fix.ps1"
if %errorlevel% neq 0 (
    echo [ERROR] Auto-path-fix failed: %errorlevel%
    exit /b %errorlevel%
)
echo [FIX] Path correction successful.
echo [RUN] Issuing ALN workload commands—calling run ALN agent prompt.
powershell -File "%~dp0aln.ps1"
echo [RUN] Workload completed.
