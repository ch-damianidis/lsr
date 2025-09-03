@echo off
cd /d "%~dp0"
"C:\Program Files\R\R-4.4.2\bin\x64\Rscript.exe" -e "shiny::runApp('.', launch.browser = TRUE)"
pause
