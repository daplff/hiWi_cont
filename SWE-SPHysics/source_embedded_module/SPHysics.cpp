#include "SPHysics.h"
#include "../../ParticlesStructure.h"
#include "FortranIO.h"
extern "C"
{
	void sph_init_();
	void sph_loopstep_();
}

SPHysics::SPHysics()
{
}

SPHysics::~SPHysics()
{
}

void SPHysics::initialize(Parameters& parameters)
{
	sph_init_();
}

void SPHysics::runTimestep(ParticlesStructure& particles)
{
	set_variables(particles);
	sph_loopstep_();
	get_variables(particles);
}

void SPHysics::set_variables(ParticlesStructure& particles) {
	fortranIO_ptr->setRuntimeVariables(particles);
}

void SPHysics::get_variables(ParticlesStructure& particles) {
	fortranIO_ptr->getRuntimeVariables(particles);
}

