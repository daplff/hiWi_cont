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

  SPHYSICS_SWE_gen_2D.exe <Case_tsunami.txt

  copy SPHYSICS.mak ..\..\source\SPHYSICS_SWE_2D\SPHYSICS.mak

) ELSE (
  ECHO SPHYSICSGEN compilation FAILED
  cd %UDIRX%
)









