FC=gfortran
CC=g++
CFLAGS=-std=c++11 -g
FOPTIONS= -O3 -g
srcdir=.
idir=../execs
bakdir=../execs.bak
libraries=-lstdc++ -lnetcdf

dir_solver=SWE-SPHysics_solver
dir_new_solver=SWE-SPHysics_solver
objects_new=$(dir_new_solver)/SPH_init.o $(dir_new_solver)/SPH_loopstep.o $(dir_new_solver)/SPH_output.o $(dir_new_solver)/get_time.o $(dir_new_solver)/set_t_end.o  

dir_rootcpp=.
objects_cpp=$(dir_rootcpp)/testing_c++callf.cpp.o $(dir_rootcpp)/FortranIO/FortranIO.cpp.o $(dir_rootcpp)/SPHysics.cpp.o $(dir_rootcpp)/Datastructures/ParticlesStructure.cpp.o $(dir_rootcpp)/Datastructures/Parameters.cpp.o $(dir_rootcpp)/Output/ParticleOutputter.cpp.o

dir_rootfortran=.
objects_fortran=$(dir_rootfortran)/FortranIO/c_helper_module.o $(dir_rootfortran)/FortranIO/c_helper_module_from_SPH.o

objects_solver=$(dir_solver)/global_2D.o   \
	$(dir_solver)/ini_divide_2D.o   \
	$(dir_solver)/getdata_2D.o   \
	$(dir_solver)/check_limits_2D.o   \
	$(dir_solver)/poute_2D.o   \
	$(dir_solver)/poute_grid_2D.o   \
	$(dir_solver)/divide_2D.o   \
	$(dir_solver)/divide_vir_2D.o   \
	$(dir_solver)/variable_time_step_2D.o   \
	$(dir_solver)/ac_2D.o   \
	$(dir_solver)/ac_dw_var_2D.o   \
	$(dir_solver)/celij_dw_2D.o   \
	$(dir_solver)/self_dw_2D.o   \
	$(dir_solver)/celij_dw_vir_2D.o   \
	$(dir_solver)/celij_dw_ob_2D.o   \
	$(dir_solver)/kernel_cubic_2D.o   \
	$(dir_solver)/step_leap_frog_2D.o   \
	$(dir_solver)/ac_alpha_2D.o   \
	$(dir_solver)/celij_alpha_2D.o   \
	$(dir_solver)/self_alpha_2D.o   \
	$(dir_solver)/celij_alpha_vir_2D.o   \
	$(dir_solver)/grid_h_var_2D.o   \
	$(dir_solver)/celij_visc_2D.o   \
	$(dir_solver)/self_visc_2D.o   \
	$(dir_solver)/celij_ob_2D.o   \
	$(dir_solver)/celij_vir_2D.o   \
	$(dir_solver)/viscosity_LF_2D.o   \
	$(dir_solver)/divide_b_2D.o   \
	$(dir_solver)/bottom_2D.o   \
	$(dir_solver)/celij_hb_2D.o   \
	$(dir_solver)/source_slope_2D.o   \
	$(dir_solver)/ac_b_2D.o   \
	$(dir_solver)/celij_b_2D.o   \
	$(dir_solver)/celij_b_c_2D.o   \
	$(dir_solver)/limiter_minmod.o   \
	$(dir_solver)/refinement_2D.o   \
	$(dir_solver)/refinement_v_2D.o   \
	$(dir_solver)/open_bc_2D.o   \
	$(dir_solver)/interp_openbc_2D.o   \
	$(dir_solver)/recount_2D.o   \
	$(dir_solver)/celij_alpha_ob_2D.o   \
	$(dir_solver)/divide_ob_2D.o   \
	$(dir_solver)/ac_corr_2D.o   \
	$(dir_solver)/celij_corr_2D.o   \
	$(dir_solver)/self_corr_2D.o   \
	$(dir_solver)/open_bc_pos_2D.o  
#	$(dir_solver)/SPHYSICS_SWE_2D.o   \

%face:
	echo "$@face :)"
#
%.o: %.f
	$(FC) $(FOPTIONS) -c -o $@ $<
#
SPHYSICS_SWE_2D: $(objects_solver) $(objects_fortran) $(objects_new) $(objects_cpp) 
	$(FC) $(OPTIONS) -o SPHYSICS_SWE_2D $(objects_solver) $(objects_fortran) $(objects_new) $(objects_cpp) $(libraries)
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
	rm $(objects_new) $(objects_cpp) $(objects_solver) $(objects_fortran) */*~ *~ *.mod
#

#
%.cpp.o: %.cpp
	$(CC) $(CFLAGS) -c -o $@ $<
#
	

