#ifndef SIMULATIONPARAMETERS_H
#define SIMULATIONPARAMETERS_H

class SimulationParameters
{
public:
	SimulationParameters();
	~SimulationParameters();



	struct Physical {
	public:
		double rho0;		//density of fluid
		double viscos_val; 	//viscosity of fluid
	} physical;
	
	struct Output {
		double trec_ini;	//time of first output
		double out;			//output time interval
		double dx, dy;		//grid sizes for interpolated values on output grid
		int idebug;			//detailed output every timestep (1=y)
	} output;
	
	struct Simulation {
		struct Time {
			double dt;		//initial timestep
			double tmax;	//end time
			int ivar_dt;	//variable timesteps on/off (1=on)
		} time;
		struct ParticleInfo {
			int np_b; 
			//TODO
		} particleInfo;
		struct Refinement {
			//TODO
		} refinement;
		
		//variables concerning the newton-raphson procedure to find accelerations
		//etc in gradient/density/pressures of particles
		struct Interpolations 
		{
			//TODO
		} interpolations;
		
		
	} simulation;
};

#endif // SIMULATIONPARAMETERS_H
