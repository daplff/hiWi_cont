#include "FortranIO.h"
#include "../../ParticlesStructure.h"
#include "fortrangetset_prototypes.h"
#include <iostream>

FortranIO::FortranIO()
{
}

FortranIO::~FortranIO()
{
}

void FortranIO::setRuntimeVariables(ParticlesStructure& particles) {

	double * xpos_ptr = (particles.getXposArray()).data();
	double * ypos_ptr = (particles.getYposArray()).data();
	int len = particles.getXposArray().size();
	if (particles.getYposArray().size() != len)
	{
		int tempYlen = particles.getYposArray().size();
		std::cerr<<
				"Warning! Xpos and Ypos vectors have different lengths! Setting editing length to min of xlen ("
				<< len << ") and ylen (" << tempYlen << ")!" <<std::endl;
		len = (len > tempYlen ? tempYlen : len);
	}

	C_SETARRAY_TO_FORTRAN(xp,xpos_ptr, len) ;
	C_SETARRAY_TO_FORTRAN(yp,ypos_ptr, len) ;


}

void FortranIO::getRuntimeVariables(ParticlesStructure& particles) {
	double * xpos_ptr = (particles.getXposArray()).data();
	double * ypos_ptr = (particles.getYposArray()).data();
	int len = particles.getXposArray().size();
	if (particles.getYposArray().size() != len)
	{
		int tempYlen = particles.getYposArray().size();
		std::cerr<<
				"Warning! Xpos and Ypos vectors have different lengths! Setting editing length to min of xlen ("
				<< len << ") and ylen (" << tempYlen << ")!" <<std::endl;
		len = (len > tempYlen ? tempYlen : len);
	}

	C_GETARRAY_FROM_FORTRAN(xp, xpos_ptr, len);
	C_GETARRAY_FROM_FORTRAN(yp, ypos_ptr, len);

}


