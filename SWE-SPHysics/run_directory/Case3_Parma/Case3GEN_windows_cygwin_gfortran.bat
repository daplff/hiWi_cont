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

  SPHYSICS_SWE_gen_2D.exe <Case_parma_v01.txt 
  REM > Case_parma_v01.out
  REM !!To use debugging in gfortran, edit SPHYSICS_SWE_gen_2D_gfortran.mak

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









