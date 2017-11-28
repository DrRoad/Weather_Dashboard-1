@echo off
color A
title HostShiny

echo --------------------------------------------------
echo Host Weather Dashboard
echo --------------------------------------------------

REM -- SETTINGS

SET RSCRIPT="G:\13. Portfolio Management\04. Analysis\0. Short Term Desk\Reports\R\bin\x64\Rscript.exe"
SET R_RUNAPP="H:\Sources\Weather_Dashboard\run_shiny_app.R"
SET R_SHINY="H:\Sources\Weather_Dashboard\src"
REM -- HOST APPS

set /a port=%random% %% 1000-0 + 7000
echo %port%

start /WAIT /B "Weather Dashboard" %RSCRIPT% %R_RUNAPP% %port% %R_SHINY%
timeout 10
"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" http://127.0.0.1:%port%