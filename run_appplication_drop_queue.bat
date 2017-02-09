:loop
del /s /q bin\application_drop_queue.exe
gprbuild -p main.gpr
pause
cd bin
application_drop_queue.exe
cd ..
pause
goto loop