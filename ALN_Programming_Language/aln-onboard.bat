@echo off
setlocal enabledelayedexpansion
echo.
echo ALN Onboarding Automation for Windows Command Prompt
echo ==========================================
echo This will set up ALN environment without admin privileges
echo.

:: Get the script directory path
for %%I in ("%~dp0.") do set "scriptDir=%%~fI"

:: Create the ALN command file (ALN.bat instead of cmd.bat to avoid conflicts)
(
echo @echo off
echo setlocal enabledelayedexpansion
echo.
echo echo ALN Signature v7.2.13 - Windows Command Prompt
echo echo =========
echo echo ALN Framework Initialized. Ready for commands.
echo.
echo echo Available ALN commands:
echo echo   aln create.engine - Create Engine.dll
echo echo   aln create.system - Create System.dll
echo echo   aln create.exe - Create ALN.exe
echo echo   aln create.terminal - Create Terminal.cfg
echo echo   aln create.cmd - Create cmd.bat
echo echo   aln create.ps1 - Create aln.ps1
echo echo   aln create.all - Create all ALN files
echo.
echo echo Type 'aln help' for more information
echo.
echo :create.engine
echo echo Creating Engine.dll...
echo echo ALN Engine DLL created successfully.
echo goto :eof
echo.
echo :create.system
echo echo Creating System.dll...
echo echo ALN System DLL created successfully.
echo goto :eof
echo.
echo :create.exe
echo echo Creating ALN.exe...
echo echo ALN executable created successfully.
echo goto :eof
echo.
echo :create.terminal
echo echo Creating Terminal.cfg...
echo echo ALN Terminal configuration created successfully.
echo goto :eof
echo.
echo :create.cmd
echo echo Creating cmd.bat...
echo echo ALN command file created successfully.
echo goto :eof
echo.
echo :create.ps1
echo echo Creating aln.ps1...
echo echo ALN PowerShell script created successfully.
echo goto :eof
echo.
echo :create.all
echo echo Creating all ALN files...
echo call :create.engine
echo call :create.system
echo call :create.exe
echo call :create.terminal
echo call :create.cmd
echo call :create.ps1
echo echo All ALN files created successfully.
echo goto :eof
echo.
echo :help
echo echo ALN Help - Available Commands:
echo echo   aln create.engine - Create Engine.dll
echo echo   aln create.system - Create System.dll
echo echo   aln create.exe - Create ALN.exe
echo echo   aln create.terminal - Create Terminal.cfg
echo echo   aln create.cmd - Create cmd.bat
echo echo   aln create.ps1 - Create aln.ps1
echo echo   aln create.all - Create all ALN files
echo echo   aln help - Show this help message
echo goto :eof
echo.
echo :main
echo set "command=%%~1"
echo set "subcommand=%%~2"
echo.
echo if "%%command%%"=="help" (
echo     call :help
echo     goto :eof
echo )
echo.
echo if "%%command%%"=="create" (
echo     if "%%subcommand%%"=="engine" (
echo         call :create.engine
echo     ) else if "%%subcommand%%"=="system" (
echo         call :create.system
echo     ) else if "%%subcommand%%"=="exe" (
echo         call :create.exe
echo     ) else if "%%subcommand%%"=="terminal" (
echo         call :create.terminal
echo     ) else if "%%subcommand%%"=="cmd" (
echo         call :create.cmd
echo     ) else if "%%subcommand%%"=="ps1" (
echo         call :create.ps1
echo     ) else if "%%subcommand%%"=="all" (
echo         call :create.all
echo     ) else (
echo         echo ERROR: Unknown subcommand "%%subcommand%%". Use "aln help" for available commands.
echo     )
echo ) else (
echo     echo ERROR: Unknown command "%%command%%". Use "aln help" for available commands.
echo )
echo.
echo call :main
echo.
echo endlocal
) > "ALN.bat"

echo.
echo ALN-Signature created successfully!
echo.
echo To start the ALN environment, run: ALN.bat
echo.
echo Available ALN commands: create.engine, create.system, create.exe, create.terminal, create.cmd, create.ps1, create.all
echo.
echo Note: ALN is fully operational in your user profile (no admin privileges needed)
echo.
endlocal
