#ifndef FORTRANIO_H
#define FORTRANIO_H

class ParticlesStructure;

class FortranIO
{
public:
	FortranIO();
	~FortranIO();
	void setRuntimeVariables(ParticlesStructure& particles);
	void getRuntimeVariables(ParticlesStructure& particles);
	void getRuntimeParameters();

};

#endif // FORTRANIO_H
