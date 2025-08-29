@echo off
REM ALN Workflow Actions Orchestrator—Direct Trigger Script
set workflowdir=%~dp0.github\workflows
for %%f in (%workflowdir%\*.yml) do (
    echo [ORCH] Checking workflow: %%f
    powershell -File "%~dp0auto-path-fix.ps1"
    if %errorlevel% neq 0 (
        echo [ERROR] Path-fix failed on %%f : %errorlevel%
    )
)
echo [ORCH] Invoking ALN agent run sequence and receiving commands...
powershell -File "%~dp0aln.ps1"
echo [ORCH] Workflow orchestration complete. All actions dispatched.
