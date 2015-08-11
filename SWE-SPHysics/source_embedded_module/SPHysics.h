#ifndef SPHYSICS_H
#define SPHYSICS_H

#include <memory>
#include <boost/property_tree/json_parser.hpp>

//TODO: class Parameters!
class Parameters;
class ParticlesStructure;
class FortranIO;

class SPHysics
{
public:
	void initialize(Parameters& parameters);
	void runTimestep(ParticlesStructure& particles);
	void simulatorOutput();
	SPHysics();
	~SPHysics();
	void set_variables(ParticlesStructure& particles);
	void get_variables(ParticlesStructure& particles);
private:
	std::shared_ptr<FortranIO> fortranIO_ptr;
};

#endif // SPHYSICS_H
