#ifndef SIMULATIONPARAMETERS_H
#define SIMULATIONPARAMETERS_H

#include <boost/property_tree/ptree.hpp>

class SimulationParameters
{
public:
	SimulationParameters();
	~SimulationParameters();
	


	struct Physical {
	public:
		double rho0;		//density of fluid
		double viscos_val; 	//viscosity of fluid
		double vlx, vly; 	//domain extent in x, y
	};
	
	struct Output {
		double trec_ini;	//time of first output
		double out;			//output time interval
		double dx, dy;		//grid sizes for interpolated values on output grid
		int idebug;			//detailed output every timestep (1=y)
	};
	
	struct Simulation {
		struct Time {
			double dt;		//initial timestep
			double tmax;	//end time
			int ivar_dt;	//variable timesteps on/off (1=on)
		} time;
		struct InitialParticles {
			int np;			//initial no of fluid particles
			int npv;		//initial no of virtual particles
			int np_b;		//initial no of bed particles			
		} initialParticles;
		
		struct Refinement {
			double coef; 	//coefficient of init smoothing length to dx
			double hsm_b_max;//smoothlength max for bottom particles
			int n0;			// upper limit on number of particles per cell
			double ref_p, ref_h; //coefficients of moved pos and smoothlen for split particles
			double dw_min_ref;	//min elevation for allowing splitting
			double xmin_ref, ymin_ref; // offsets of refinement grid origin to xpos and ypos
			double dxx_ref, dyy_ref; // sizes of refinement grid
			double ncx_ref, ncy_ref;	//number of cells in refinement grid
			//TODO: implement refinement areas
		} refinement;
		
		//variables concerning the newton-raphson procedure to find accelerations
		//etc in gradient/density/pressures of particles
		struct Technical {
			int i_openbc; //number of open boundaries
			double dw_min_fric; //minimum depth for friction source term
			double distmin; //minimum distance between open boundary particles
			double tol; 	//newton-raphson tolerance in acceleration calculations
			int i_dw_iter;	//how often N-R iteration is made
			int i_max_iter;	//max num of iterations
			
			int iMUSCL;		//muscl reconstruction (1=on) approximate fluxes at cell edges with interpolation over cell, supposedly more accurate (see wikipedia)
			double CFL; 	//courant number, related to max dt/dx ratio for convergence of method (also see wikipedia)
			
			int i_restartrun; //restart an old run (1=yes)
		};		
	

		
	} simulation;
};

#endif // SIMULATIONPARAMETERS_H
