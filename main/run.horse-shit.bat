@echo off
REM === "Horse-Shit Executable" Humor Demo ===
REM Safely launch horse-shit.exe, even with the 'creative' filename.

setlocal
set "HORSE_EXE=horse-shit.exe"

REM Check if file exists in current directory
if exist "%~dp0%HORSE_EXE%" (
    echo --------------------------------------------------------------
    echo  [INFO] Preparing to execute %HORSE_EXE% with maximal caution.
    echo  [HUMOR] Warning: Contents may include fresh barnyard logic.
    echo --------------------------------------------------------------
    pushd "%~dp0"
    "%HORSE_EXE%"
    set "exitcode=%errorlevel%"
    popd
    echo [INFO] horse-shit.exe exited with code %exitcode%.
) else (
    echo [ERROR] "%HORSE_EXE%" not found in %~dp0.
    echo [HUMOR] No manure today.
    exit /b 1
)

REM Inspired by creative, plausible Batchfile experiments!
exit /b 0
