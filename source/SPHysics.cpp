#include "SPHysics.h"
#include "Datastructures/ParticlesStructure.h"
#include "FortranIO/FortranIO.h"
#include "Output/ParticleOutputter.h"

#define OUTPUT_FILE_NAME "OutputFileName.nc"

extern "C"
{
	void sph_init_();		  //  } fortran functions
	void sph_loopstep_();  // | using the "old"/unsafe way of
	void sph_output_();    // | calling fortran functions
}

//TODO:bind(c) in fortran

SPHysics::SPHysics()
{
}

SPHysics::~SPHysics()
{
}

void SPHysics::initialize(Parameters& parameters)
{
	fortranIO_ptr = std::make_shared<FortranIO>();
	//fortranIO_ptr->setSetXpos(true);

	sph_init_();

	particleOutputter_ptr = std::make_shared<ParticleOutputter>();
	//below now done in each timestep with new file
//	particleOutputter_ptr->initialise(15/*currently irrelevant number*/, OUTPUT_FILE_NAME);


}

void SPHysics::runTimestep(ParticlesStructure& particles)
{
	set_variables(particles);
	sph_loopstep_();
	get_variables(particles);
}
void SPHysics::simulatorOutput()
{
	sph_output_();
}
void SPHysics::set_variables(ParticlesStructure& particles) {
	fortranIO_ptr->setRuntimeVariables(particles);
}

void SPHysics::netcdfOutput(int no_particles, ParticlesStructure& particles, float time) {
	particleOutputter_ptr->writeToOutputSingleUse(no_particles,particles,time);
}

void SPHysics::get_variables(ParticlesStructure& particles) {
	fortranIO_ptr->getRuntimeVariables(particles);
}

void SPHysics::changeGetAllVars(bool input){
	fortranIO_ptr->changeGetAllVars(input);
}
void SPHysics::changeSetAllVars(bool input){
	fortranIO_ptr->changeSetAllVars(input);
}
