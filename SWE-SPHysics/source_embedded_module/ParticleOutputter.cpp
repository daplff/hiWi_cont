/*
 * ParticleOutputter.cpp
 *
 *  Created on: 10 Aug 2015
 *      Author: erik
 */

#include "ParticleOutputter.h"
#include <netcdf.h>
#include <cassert>
#include <iostream>

void ParticleOutputter::handleErrorChangeStatus(std::string message)
{
	fileIsOpened = false;
	std::cerr << message << std::endl;
}


ParticleOutputter::ParticleOutputter() :
	fileIsOpened(false),
	outputTimestepCounter(0)
{
	// TODO Auto-generated constructor stub

}

ParticleOutputter::~ParticleOutputter() {
	// TODO Auto-generated destructor stub
}

bool ParticleOutputter::getFileOpenStatus() {
}

bool ParticleOutputter::writeToOutput(std::vector<double> xpos, float time) {
	bool outStatus = true;

	if(nc_put_var1_float (ncidp,timeVarId, &outputTimestepCounter,&time)!= NC_NOERR)
		{handleOutputError("couldn't write time"); outStatus = false; }

	size_t currentStartIndices [] = {outputTimestepCounter, 0};

	size_t dimensionLengths [] = {1,xpos.size()};
	if (nc_put_vara_double(ncidp,xposVarId,currentStartIndices,dimensionLengths,xpos.data()) != NC_NOERR)
	{ handleOutputError("Couldn't write xpos"); outStatus = false;}

	return outStatus;
}

void ParticleOutputter::initialise(int no_particles, std::string fileName) {


	if(nc_create(fileName.c_str(), NC_NETCDF4, &ncidp) != NC_NOERR)
		handleErrorChangeStatus("Couldn't open file");


	if ((nc_def_dim(ncidp,"time",NC_UNLIMITED,&timeDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create time dim");

	if ((nc_def_dim(ncidp,"particleNo",NC_UNLIMITED,&partNoDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create particleNo dim");


	const int dimIdsArray [] = {timeDimId,partNoDimId};
	if (nc_def_var(ncidp,"xpos",NC_FLOAT,2,dimIdsArray, &xposVarId) != NC_NOERR)
			handleErrorChangeStatus("Couldn't create xpos variable");
	if (nc_def_var(ncidp,"time",NC_FLOAT,1,&timeDimId, &timeVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create time variable");

	//TODO: attributes (units etc.)


	// end definitions, because we're done here!
	nc_enddef(ncidp);

	fileIsOpened = true;

}

void ParticleOutputter::handleOutputError(std::string message) {
	//TODO
	handleErrorChangeStatus(message);
}



