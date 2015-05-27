#include "SPHysics.h"
extern "C"
{
	void sph_init_();
	void sph_loopstep_();
	void global
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
