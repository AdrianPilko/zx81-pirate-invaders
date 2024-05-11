REM build all the assembly code "main" files in this directory

REM Its's assumed that the assembler pasmo.exe exists in e:\zx81\
REM change to wherever you have installed or the path to it

REM clean up before calling assembler 
del pivaders.p
del *.lst
del *.sym

call e:\zx81\pasmo.exe -v pivaders.asm pivaders.p

REM call will auto run emulator EightyOne if installed
REM comment in or out usin rem which one to run
call pivaders.p 


