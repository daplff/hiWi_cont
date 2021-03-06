FC=gfortran
OPTIONS= -O3
srcdir=.
idir=../../execs
bakdir=../../execs.bak

objects=  global_2D.o   \
	ini_divide_2D.o   \
	SPHYSICS_SWE_2D.o   \
	getdata_2D.o   \
	check_limits_2D.o   \
	poute_2D.o   \
	poute_grid_2D.o   \
	divide_2D.o   \
	divide_vir_2D.o   \
	variable_time_step_2D.o   \
	ac_2D.o   \
	ac_dw_var_2D.o   \
	celij_dw_2D.o   \
	self_dw_2D.o   \
	celij_dw_vir_2D.o   \
	celij_dw_ob_2D.o   \
	kernel_cubic_2D.o   \
	step_leap_frog_2D.o   \
	ac_alpha_2D.o   \
	celij_alpha_2D.o   \
	self_alpha_2D.o   \
	celij_alpha_vir_2D.o   \
	grid_h_var_2D.o   \
	celij_visc_2D.o   \
	self_visc_2D.o   \
	celij_ob_2D.o   \
	celij_vir_2D.o   \
	viscosity_LF_2D.o   \
	divide_b_2D.o   \
	bottom_2D.o   \
	celij_hb_2D.o   \
	source_slope_2D.o   \
	ac_b_2D.o   \
	celij_b_2D.o   \
	celij_b_c_2D.o   \
	limiter_minmod.o   \
	refinement_2D.o   \
	refinement_v_2D.o   \
	open_bc_2D.o   \
	interp_openbc_2D.o   \
	recount_2D.o   \
	celij_alpha_ob_2D.o   \
	divide_ob_2D.o   \
	ac_corr_2D.o   \
	celij_corr_2D.o   \
	self_corr_2D.o   \
	open_bc_pos_2D.o  

#
SPHYSICS_SWE_2D: $(objects)
	$(FC) $(OPTIONS) -o SPHYSICS_SWE_2D $(objects)
#
	if [ -d $(bakdir) ]; then \
	echo "execs.bak Directory Exists"; else \
	mkdir $(bakdir); \
	echo "execs.bak Directory Created"; \
	fi
#
	if [ -d $(idir) ]; then \
	echo "execs Directory Exists"; else \
	mkdir $(idir); \
	echo "execs Directory Created"; \
	fi
#
	-if [ -f $(idir)/SPHYSICS_SWE_2D ]; then \
	mv -f $(idir)/SPHYSICS_SWE_2D $(bakdir)/; \
	echo Old SPHYSICS_SWE_2D moved to execs.bak from execs; \
	fi
#
	mv SPHYSICS_SWE_2D $(idir)
	echo New SPHYSICS_SWE_2D moved to execs
#
clean:
	rm *.o
	rm *~
#
%.o: %.f
	$(FC) $(OPTIONS) -c -o $@ $<
