@ECHO OFF
del *.exe
del *.mak

set UDIRX= %CD%
copy ..\..\Post-Processing\fortranRoutines\PART2VTU_SWE_windows_ftn95.bat PART2VTU_SWE_windows_ftn95.bat
copy ..\..\Post-Processing\matlabRoutines\*.m %UDIRX%

cd ..\..\execs\

move SPHYSICS_SWE_gen_2D.exe ..\execs.bak

cd ..\source\SPHYSICS_SWE_gen_2D
del *.exe

mk32 -f SPHYSICS_SWE_gen_ftn95.mak clean
mk32 -f SPHYSICS_SWE_gen_ftn95.mak

IF EXIST SPHYSICS_SWE_gen_2D.exe (
  ECHO.
  ECHO SPHYSICSGEN compilation Done=yes
  ECHO.
  copy SPHYSICS_SWE_gen_2D.exe ..\..\execs\SPHYSICS_SWE_gen_2D.exe

  cd %UDIRX%

  copy ..\..\execs\SPHYSICS_SWE_gen_2D.exe SPHYSICS_SWE_gen_2D.exe

  SPHYSICS_SWE_gen_2D.exe <Case_thacker.txt 
  REM > Case_thacker.out
  REM !!To use debugging in ftn95, edit SPHYSICSgen_ftn95.mak, then use command below instead:
  REM sdbg SPHYSICS_SWE_gen_2D.exe <Case_thacker.txt

  IF EXIST SPHYSICS.mak (
    ECHO.
    ECHO SPHYSICS_SWE_gen_2D finished = yes
    ECHO.

    ) ELSE (
    ECHO.
    ECHO SPHYSICS_SWE_gen_2D run FAILED
    ECHO Check you have specified the correct compiler in Case file
    ECHO.

    cd %UDIRX%
  )
) ELSE (
  ECHO SPHYSICS_SWE_gen_2D compilation FAILED
  cd %UDIRX%
)









