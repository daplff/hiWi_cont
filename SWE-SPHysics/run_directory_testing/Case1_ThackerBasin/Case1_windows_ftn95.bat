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

  SPHYSICS_SWE_gen_2D.exe <Case_thacker.txt > Case_thacker.out
  REM -- To use debugging in ftn95, use following line instead (remove REM, and comment out above line with REM)
  REM sdbg SPHYSICSgen_2D.exe <Case_thacker.txt > Case_thacker.out
  REM -- NOTE: you must compile SPHYSICSgen_2D.f with the /CHECK compiling option in SPHYSICSgen_ftn95.mak

  copy SPHYSICS.mak ..\..\source\SPHYSICS_SWE_2D\SPHYSICS.mak

  cd ..\..\execs\

  del *.obj

  move SPHYSICS_SWE_2D.exe ..\execs.bak

  cd ..\source\SPHYSICS_SWE_2D
  del *.exe

  mk32 -f SPHYSICS.mak clean
  mk32 -f SPHYSICS.mak

  IF EXIST SPHYSICS_SWE_2D.exe (
    ECHO.
    ECHO SPHYSICScompilationDone = yes
    ECHO.
    copy SPHYSICS_SWE_2D.exe ..\..\execs\SPHYSICS_SWE_2D.exe

    cd %UDIRX%

    copy ..\..\execs\SPHYSICS_SWE_2D.exe SPHYSICS_SWE_2D.exe 

    SPHYSICS_SWE_2D.exe
    REM -- To use debugging in ftn95, use following line instead (remove #, and comment out above line with #)
    REM sdbg SPHYSICS_SWE_2D.exe
    REM -- NOTE: you must compile SPHYSICS_2D.exe with the /CHECK compiling option,  
    REM -- EDIT the 'tocompile_ftn95' subroutine of SPHYSICSgen_2D.f

  ) ELSE (
    ECHO.
    ECHO SPHYSICS_SWE_2D compilation FAILED
    ECHO Check you have specified the correct compiler in Case file
    ECHO.

    cd %UDIRX%
  )
) ELSE (
  ECHO SPHYSICS_SWE_GEN_2D compilation FAILED
  cd %UDIRX%
)









