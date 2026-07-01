@echo off
setlocal
REM ===========================================================================
REM (Re)start the Emacs daemon. Run once per login -- if a daemon is already
REM running it is stopped (gracefully, then force-killed) and a fresh one is
REM started. Works with any Emacs install (PATH, scoop, Program Files, choco).
REM After this, open frames from a Start-Menu / taskbar shortcut running:
REM     emacsclientw.exe -c -n -a ""
REM ===========================================================================

REM --- Locate runemacs.exe --------------------------------------------------
set "RUNEMACS="
REM 1) On PATH (scoop shims, choco, winget, manual PATH installs)
for /f "delims=" %%I in ('where runemacs.exe 2^>nul') do if not defined RUNEMACS set "RUNEMACS=%%I"
REM 2) Common fixed install locations
if not defined RUNEMACS for %%P in (
  "%USERPROFILE%\scoop\apps\emacs\current\bin\runemacs.exe"
  "%LOCALAPPDATA%\Programs\Emacs\bin\runemacs.exe"
  "%ProgramFiles%\Emacs\bin\runemacs.exe"
  "%ProgramData%\chocolatey\bin\runemacs.exe"
) do if not defined RUNEMACS if exist "%%~P" set "RUNEMACS=%%~P"
REM 3) Program Files versioned dir (…\Emacs\emacs-30.2\bin\runemacs.exe)
if not defined RUNEMACS for /d %%D in ("%ProgramFiles%\Emacs\emacs-*") do if exist "%%D\bin\runemacs.exe" set "RUNEMACS=%%D\bin\runemacs.exe"

if not defined RUNEMACS (
  echo Could not find runemacs.exe. Add Emacs's bin folder to PATH, or edit this script.
  pause
  exit /b 1
)

REM --- Locate emacsclient.exe -----------------------------------------------
set "EMACSCLIENT="
for %%I in ("%RUNEMACS%") do if exist "%%~dpIemacsclient.exe" set "EMACSCLIENT=%%~dpIemacsclient.exe"
if not defined EMACSCLIENT for /f "delims=" %%J in ('where emacsclient.exe 2^>nul') do if not defined EMACSCLIENT set "EMACSCLIENT=%%J"

REM --- Stop any running daemon (graceful -> wait -> force) -------------------
REM A lingering old daemon is the classic footgun: emacsclient reattaches to
REM the STALE daemon (old config) unless we wait for it to actually die first.
if not defined EMACSCLIENT goto forcekill
echo Stopping any running Emacs daemon...
REM Bind confirm-kill-processes so a running subprocess can't block the exit.
"%EMACSCLIENT%" -e "(let ((confirm-kill-processes nil)) (kill-emacs))" >nul 2>&1

set /a KILLTRIES=0
:killwait
"%EMACSCLIENT%" -e "t" >nul 2>&1
if errorlevel 1 goto killed
set /a KILLTRIES+=1
if %KILLTRIES% geq 8 goto forcekill
<nul set /p "=."
ping -n 2 127.0.0.1 >nul
goto killwait

:forcekill
REM Graceful stop didn't take (or no emacsclient): force-kill ONLY the daemon
REM emacs.exe (matched by its --daemon command line), never standalone frames.
powershell -NoProfile -Command "Get-CimInstance Win32_Process -Filter ""Name='emacs.exe'"" | Where-Object { $_.CommandLine -match '--daemon' } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force }" >nul 2>&1
ping -n 2 127.0.0.1 >nul

:killed
echo Starting Emacs daemon: "%RUNEMACS%"
start "" "%RUNEMACS%" --daemon

REM --- Wait until the daemon has finished preloading every package -----------
REM The daemon keeps loading (elpaca activation + package preload) after launch;
REM poll `my/preload-done' so we report REAL readiness, not just "process started".
if not defined EMACSCLIENT (
  echo Daemon starting in the background.
  goto :done
)

echo Loading packages ^(first run after a wipe can take a few minutes^)...
set /a TRIES=0
set "SERVERUP="
:waitloop
set /a TRIES+=1
set "READY="
"%EMACSCLIENT%" -e "(and (boundp (quote my/preload-done)) my/preload-done)" > "%TEMP%\emacs-ready.tmp" 2>nul
if not errorlevel 1 set "SERVERUP=1"
set /p READY=<"%TEMP%\emacs-ready.tmp"
del "%TEMP%\emacs-ready.tmp" >nul 2>&1
if "%READY%"=="t" (
  echo.
  echo Emacs daemon ready -- all packages loaded. You can open frames now.
  ping -n 3 127.0.0.1 >nul
  goto :done
)
REM Server never answered within ~1 min => daemon crashed on init. Show + hold.
if not defined SERVERUP if %TRIES% gtr 60 (
  echo.
  echo ERROR: the daemon did not come up -- it likely errored during init.
  echo To see the actual error, run this in a terminal:
  echo     emacs --fg-daemon
  echo.
  pause
  goto :done
)
REM Server is up but preload hasn't finished after a long time.
if %TRIES% gtr 600 (
  echo.
  echo Still warming after a while; it will keep loading in the background.
  pause
  goto :done
)
<nul set /p "=."
ping -n 2 127.0.0.1 >nul
goto :waitloop

:done
endlocal
