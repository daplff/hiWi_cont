#ifndef SPHYSICS_H
#define SPHYSICS_H

#include <memory>
#include <boost/property_tree/json_parser.hpp>

//TODO: class Parameters!
class Parameters;
class ParticlesStructure;
class FortranIO;
class ParticleOutputter;

class SPHysics
{
public:
	void initialize(Parameters& parameters);
	void runTimestep(ParticlesStructure& particles);
	void simulatorOutput();
	void netcdfOutput(int no_particles, ParticlesStructure& particles, float time);
	SPHysics();
	~SPHysics();
	void set_variables(ParticlesStructure& particles);
	void get_variables(ParticlesStructure& particles);

private:
	std::shared_ptr<FortranIO> fortranIO_ptr;
	std::shared_ptr<ParticleOutputter> particleOutputter_ptr;
};

#endif // SPHYSICS_H
