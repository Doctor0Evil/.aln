@echo off
setlocal enabledelayedexpansion
echo [ALN BOOTSTRAP] Beginning ALN onboarding for non-admin Windows session.
powershell -File "%~dp0aln.ps1"
if %errorlevel% neq 0 (
    echo [ERROR] Powershell bootstrap failed: %errorlevel%
    exit /b %errorlevel%
)
echo [ALN BOOTSTRAP] ALN setup sequence complete.
endlocal
