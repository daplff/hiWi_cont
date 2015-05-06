UDIRX=`pwd`
cp ../../Post-Processing/matlabRoutines/* ./
cp ../../Post-Processing/fortranRoutines/PART2VTU_SWE_unix_ifort.bat ./

cd ../../source/SPHYSICS_SWE_gen_2D/
make -f SPHYSICS_SWE_gen_ifort.mak clean
make -f SPHYSICS_SWE_gen_ifort.mak
if [ $? -eq 0 ]; then
  GENcompilationDone="yes"
  echo 'GENcompilationDone = ' $GENcompilationDone
  cd $UDIRX
  ../../execs/SPHYSICS_SWE_gen_2D < Case_CADAM.txt 
  #> "Case_CADAM.out"
  if [ $? -eq 0 ]; then
    SPHYSICSgenDone="yes"
    echo 'SPHYSICSgenDone = ' $SPHYSICSgenDone
    cd $UDIRX
  else
    echo ' '
    echo 'SPHYSICS_SWE_gen_2D run failed'
    echo 'Make sure all parameters are correct in Case file'
  fi
else
  cd $UDIRX
  echo ' '
  echo 'SPHYSICS_SWE_gen_2D compilation failed'
fi
