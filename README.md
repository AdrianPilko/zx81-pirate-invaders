# ZX81 Pirate Invaders
A zx81 game which brings a maritime angle to an old classic

pivaders.asm is the main file.

There are two build scripts - both use the pasmo assembler. The decision to change was mainly due to it being better supported and better OS availablility (ie you can download a Linux or Windows version). Some of the syntax is slightly different but not greatly. https://pasmo.speccy.org/

MS Windows: 
 1) install pasmo.exe and make sure the build script file build_using_pasmo.bat has the correct path
 2) run build_using_pasmo.bat to build

Linux:
 1) if not already done so install pasmo using whatever method your distro allows
 2) run build_using_pasmo.sh to build 


The old tasm compatible build is not supported and those build files are in ./archive
The ./resources directory contains files for sprite data that are not actually used in the build just needed to recreate or create the data that is embedded in pivaders.asm. pasmo supports INCBIN so at some point may change to using that instead of copy/pasting the test versions of sprites.
