FC=ifort -O3
srcdir = .
idir=../../execs
bakdir=../../execs.bak
objects=SPHYSICS_SWE_gen_2D.o \
#
SPHYSICSgen: $(objects)
	$(FC) -o SPHYSICS_SWE_gen_2D $(srcdir)/$(objects)
#
	if [ -d $(bakdir) ]; then \
	echo execs.bak Directory Exists; else \
	mkdir $(bakdir); \
	echo execs.bak Directory Created; \
	fi
#
	if [ -d $(idir) ]; then \
	echo execs Directory Exists; else \
	mkdir $(idir); \
	echo execs Directory Created; \
	fi

#
	if [ -f $(idir)/SPHYSICS_SWE_gen_2D ]; then \
	mv $(idir)/SPHYSICS_SWE_gen_2D $(bakdir)/; \
	echo Old SPHYSICS_SWE_gen_2D moved to execs.bak from execs; \
	fi
#
	mv SPHYSICS_SWE_gen_2D $(idir)
	echo New SPHYSICS_SWE_gen_2D moved to execs
#
clean :
	rm *.o
