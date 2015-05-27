#ifndef SPHYSICS_H
#define SPHYSICS_H

//TODO: class Parameters!
class Parameters;
class ParticlesStructure;

class SPHysics
{
public:
	void initialize(Parameters& parameters);
	void runTimestep(ParticlesStructure& particles);
	SPHysics();
	~SPHysics();
	void set_variables(ParticlesStructure& particles);
	void get_variables(ParticlesStructure& particles);
};

#endif // SPHYSICS_H
