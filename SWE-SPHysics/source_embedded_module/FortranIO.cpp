#include "FortranIO.h"
#include "../../ParticlesStructure.h"
#include "fortrangetset_prototypes.h"


FortranIO::FortranIO()
{
}

FortranIO::~FortranIO()
{
}

void FortranIO::setRuntimeVariables(ParticlesStructure& particles) {

	double * xpos_ptr = (particles.getXposArray()).data();
	int xpos_len = particles.getXposArray().size();

	C_SETARRAY_TO_FORTRAN(xp,xpos_ptr,xpos_len) ;
	C_SETARRAY_TO_FORTRAN(xp,xpos_ptr, xpos_len);


}

void FortranIO::getRuntimeVariables(ParticlesStructure& particles) {
	double * xpos_ptr = (particles.getXposArray()).data();
	int xpos_len = particles.getXposArray().size();


	C_GETARRAY_FROM_FORTRAN(xp, xpos_ptr, xpos_len);
}


