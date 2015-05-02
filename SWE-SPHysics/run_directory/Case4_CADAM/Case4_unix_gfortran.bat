UDIRX=`pwd`
cd ../../source/SPHYSICS_SWE_gen_2D/
cp ../../Post-Processing/fortranRoutines/PART2VTU_SWE_unix_gfortran.bat ./

make -f SPHYSICS_SWE_gen_gfortran.mak clean
make -f SPHYSICS_SWE_gen_gfortran.mak
if [ $? -eq 0 ]; then
  SPHYSICSGENcompilationDone="yes"
  echo 'SPHYSICSGENcompilationDone = ' $SPHYSICSGENcompilationDone
  cd $UDIRX
  ../../execs/SPHYSICS_SWE_gen_2D < Case_CADAM.txt > "Case_CADAM.out"
  if [ $? -eq 0 ]; then
    cp SPHYSICS.mak ../../source/SPHYSICS_SWE_2D/
    cd ../../source/SPHYSICS_SWE_2D
    pwd
    make -f SPHYSICS.mak clean
    make -f SPHYSICS.mak 
    if [ $? -eq 0 ]; then
      SPHYSICScompilationDone="yes"
      echo 'SPHYSICScompilationDone = ' $SPHYSICScompilationDone
      rm SPHYSICS.mak
      cd $UDIRX
      pwd
      cp ../../execs/SPHYSICS_SWE_2D ./
      ./SPHYSICS_SWE_2D
    else
      cd $UDIRX
      echo ' '
      echo 'SPHYSICS__SWE_2D compilation failed'
      echo 'Make sure correct compiler is selected in Case file'
    fi
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
