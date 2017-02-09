:loop
del /s /q bin\application_drop.exe
gprbuild -p main.gpr
pause
cd bin
application_drop.exe
cd ..
pause
goto loop