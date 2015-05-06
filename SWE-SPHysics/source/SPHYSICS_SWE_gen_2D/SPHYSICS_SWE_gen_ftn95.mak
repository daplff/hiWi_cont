## Uncomment/Comment the options as appropriate for debugging
# OPTIONS=/OPTIMISE
OPTIONS=/CHECK

OBJFILES=SPHYSICS_SWE_gen_2D.obj

SPHYSICS_SWE_gen_2D.exe: $(OBJFILES)

clean:
	del *.obj