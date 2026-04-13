# How to compile Dactyloidae (as of version 13.2)

Step 0. INSTALL GIT FOR WINDOWS!!!!!

Step 1. If you are on Windows 8.1 or older, mount your installation media.

Step 2. Go Win+R and type 'optionalfeatures.exe' and check .NET 3.5

Step 3. Get the DirectX SDK (June 2010) from HERE: https://archive.org/details/dxsdk_jun10

Step 4. After that has installed, get MozillaBuild 3.2 from HERE: https://ftp.mozilla.org/pub/mozilla/libraries/win32/MozillaBuildSetup-3.2.exe

Step 5. After THAT has installed, you can commence onto installing the FWDK. First, get the .7z HERE: https://archive.org/details/funny-windows-development-kit

Step 6. Extract the FWDK to C:\Dev

Step 7. Move the start-shell-ewdk and start-shell-ewdk-x64 from this folder into c:\mozilla-build.

Step 8. Now that you have done all those prerequisites, open your start-shell-ewdk (64 if you want to build a 64 bit browser)

Step 9. Git clone this repo if you have not already

Step 10. CD into the directory of where your git clone is, and execute THESE commands:

``git config core.autocrlf false``

``git config core.eof lf``

``git rm --cached -r .``

``git reset --hard``

Step 11. Now that you have FINALLY done all those, you are now free to do a ``./mach build`` (isn't mozillabuild fun?)

Step 11.5. After the build, if you really feel fancy, you can test your build before packaging it with  ``./mach run``

Step 12. Run ``./mach installer ``  (64-bit only for now, sorry)
