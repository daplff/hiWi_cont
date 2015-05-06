set UDIRX= %CD%

cd ..\..\Post-Processing\fortranRoutines


nmake -f PART2VTU_SWE_win_ifort.mak


cd %UDIRX%


del Paraview*


md ParaviewFiles\VTU

 
copy ..\..\Post-Processing\fortranRoutines\PART2VTU_SWE_2D.exe PART2VTU_SWE_2D.exe

PART2VTU_SWE_2D.exe 


