@ECHO OFF
del *.exe
del *.mak

set UDIRX= %CD%
copy ..\..\Post-Processing\fortranRoutines\PART2VTU_SWE_windows_ifort.bat PART2VTU_SWE_windows_ifort.bat
copy ..\..\Post-Processing\matlabRoutines\*.m %UDIRX%

cd ..\..\execs\

move SPHYSICS_SWE_gen_2D.exe ..\execs.bak

cd ..\source\SPHYSICSgen2D
del *.exe

nmake -f SPHYSICS_SWE_gen_win_ifort.mak clean
nmake -f SPHYSICS_SWE_gen_win_ifort.mak

IF EXIST SPHYSICS_SWE_gen_2D.exe (
  ECHO.
  ECHO SPHYSICSGEN compilation Done=yes
  ECHO.
  copy SPHYSICS_SWE_gen_2D.exe ..\..\execs\SPHYSICS_SWE_gen_2D.exe

  cd %UDIRX%

  copy ..\..\execs\SPHYSICS_SWE_gen_2D.exe SPHYSICS_SWE_gen_2D.exe

  SPHYSICS_SWE_gen_2D.exe <Case_tsunami.txt > Case_tsunami.out

  copy SPHYSICS.mak ..\..\source\SPHYSICS_SWE_2D\SPHYSICS.mak

  cd ..\..\execs\

  del *.obj

  move SPHYSICS_SWE_2D.exe ..\execs.bak

  cd ..\source\SPHYSICS_SWE_2D
  del *.exe

  nmake -f SPHYSICS.mak clean
  nmake -f SPHYSICS.mak

  IF EXIST SPHYSICS_SWE_2D.exe (
    ECHO.
    ECHO SPHYSICScompilationDone = yes
    ECHO.
    copy SPHYSICS_SWE_2D.exe ..\..\execs\SPHYSICS_SWE_2D.exe

    cd %UDIRX%

    copy ..\..\execs\SPHYSICS_SWE_2D.exe SPHYSICS_SWE_2D.exe 

    SPHYSICS_SWE_2D.exe

  ) ELSE (
    ECHO.
    ECHO SPHYSICScompilation FAILED
    ECHO Check you have specified the correct compiler in Case file
    ECHO.

    cd %UDIRX%
  )
) ELSE (
  ECHO SPHYSICSGEN compilation FAILED
  cd %UDIRX%
)









