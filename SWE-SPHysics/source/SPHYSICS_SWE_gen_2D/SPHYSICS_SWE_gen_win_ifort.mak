FC=ifort

OPTIONS=/nologo
COPTIONS=/O3

OBJFILES=SPHYSICS_SWE_gen_2D.obj

.f.obj:
	$(FC) $(OPTIONS) $(COPTIONS) /c $<

SPHYSICS_SWE_gen_2D.exe: $(OBJFILES)
	xilink /OUT:$@ $(OPTIONS) $(OBJFILES)

clean :
	del *.obj *.mod SPHYSICS_SWE_gen_2D.exe
