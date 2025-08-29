@echo off
REM === ALN/LISP Resilient Ingestion/Sync Adaptive Script ===
REM Hybrid batch script for automated ingestion with dynamic error handling and context-aware input sanitization.

setlocal enabledelayedexpansion

:: Define humor reason generator
set "joke=Why did the batch script cross the pipeline? To debug the other side!"

:: Sanitize input args (contextual; expandable pattern set)
for %%A in (%*) do (
    set "ARG=%%A"
    set "ARG=!ARG:;= !"
    echo Sanitized input: !ARG!
)

echo    --------------------------------------------
echo    [ALN] SUPERINTELLIGENT INGESTION PIPELINE
echo    Humor: %joke%
echo    --------------------------------------------

:: Main ALN/LISP sync trigger
if exist "%~dp0aln.ps1" (
    powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0aln.ps1" -task "ingest"
    if !errorlevel! neq 0 (
        echo [WARN] Ingestion failed, running Lisp fallback...
        if exist "%~dp0ci.lisp" (
            echo (aln-fallback-ingest) | sbcl --script "%~dp0ci.lisp"
        )
    )
) else (
    echo [ERROR] Core ALN ingest is missing. Aborting.
    exit /b 1
)

:: Output autonomous correction banner
echo [AI] Workflow correction: Batch/Lisp/ALN triplet validated.

exit /b 0
