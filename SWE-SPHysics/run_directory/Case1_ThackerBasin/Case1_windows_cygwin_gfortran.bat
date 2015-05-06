@ECHO OFF
del *.exe
del *.mak

set UDIRX= %CD%
copy ..\..\Post-Processing\fortranRoutines\PART2VTU_SWE_windows_cygwin_gfortran.bat PART2VTU_SWE_windows_cygwin_gfortran.bat
copy ..\..\Post-Processing\matlabRoutines\*.m %UDIRX%

cd ..\..\execs\

move SPHYSICS_SWE_gen_2D.exe ..\execs.bak

cd ..\source\SPHYSICS_SWE_gen_2D
del *.exe

make -f SPHYSICS_SWE_gen_gfortran.mak clean
make -f SPHYSICS_SWE_gen_gfortran.mak

IF EXIST ../../execs/SPHYSICS_SWE_gen_2D.exe (
  ECHO.
  ECHO SPHYSICSGEN compilation Done=yes
  ECHO.
  copy SPHYSICS_SWE_gen_2D.exe ..\..\execs\SPHYSICS_SWE_gen_2D.exe

  cd %UDIRX%

  copy ..\..\execs\SPHYSICS_SWE_gen_2D.exe SPHYSICS_SWE_gen_2D.exe

  SPHYSICS_SWE_gen_2D.exe <Case_thacker.txt > Case_thacker.out
  REM !!To use debugging in gfortran, edit SPHYSICSgen_gfortran.mak

  copy SPHYSICS.mak ..\..\source\SPHYSICS_SWE_2D\SPHYSICS.mak

  cd ..\..\execs\

  move SPHYSICS_SWE_2D.exe ..\execs.bak\

  cd ..\source\SPHYSICS_SWE_2D
  del *.exe

  make -f SPHYSICS.mak clean
  make -f SPHYSICS.mak

  IF EXIST ../../execs/SPHYSICS_SWE_2D.exe (
    ECHO.
    ECHO SPHYSICScompilationDone = yes
    ECHO.
    copy SPHYSICS_SWE_2D.exe ..\..\execs\SPHYSICS_SWE_2D.exe

    cd %UDIRX%

    copy ..\..\execs\SPHYSICS_SWE_2D.exe SPHYSICS_SWE_2D.exe 

    SPHYSICS_SWE_2D.exe
    REM -- To use debugging in gfortran, edit subroutine 'tocompile_gfortran' in SPHYSICSgen_2D.f

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









