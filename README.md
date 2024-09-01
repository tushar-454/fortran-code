# Fortran Code

## Setup Codeblocks for Fortran [Windows]

### Step - 1
Uninstall previous Codeblocks software

### Step - 2
Download Codeblocks with GNU Fortarn Compilor [Click Here](https://www.codeblocks.org/downloads/binaries/#imagesoswindows48pnglogo-microsoft-windows)
Then You can see `codeblocks-20.03mingw-setup.exe` text. Now Just click on `FossHUB` or `Sourceforge.net` then download will be start.
 If your device don't support 64bit you can click `codeblocks-20.03mingw-32bit-setup.exe`. After download completed you need install the software first.
 
 ### Step - 3 
 
 Install the codeblocks software by double click .exe file. Then Open the software. Now you need to configure the IDE.
 
 ### Step - 4
 
 Now, You need findout the installaion path from `Local disk C:`
 Example: C:\Program Files\CodeBlocks\MinGW
 Copy the path from your computer file explorer where your codeblocks installed.
 
 ### Step - 5
 
 Now come to codeblocks app, Open it and Go to `Settings` and go to `Compiler ...`
 Now you need select `GNU Fortran Compiler` from `Selected compiler` 
 Now you need go to `Toolchain executables` tab, then you can see a input box which label will be `Compiler's installation directory`
 Now in this box you need past the path which you copy before.
 Then you can see the below more input box which filled with `mingw3-gfortran.exe` just replace this text with `gfortran.exe`
 Then Click `OK`
 
 ### Step - 6
 Now create a file with `.f95` extension then open it in codeblocks.
 Then write your first fortran code and then build and run it.
 ```fortarn
 PRINT *, "Hello Fortran"
 STOP
 END
 ```
 
 You find build and run button in top tool section.
 
 Hurray! You can see pop up a terminal and see the output `Hello Fortarn`.
 
 Your codeblocks ide now setup for fortran.