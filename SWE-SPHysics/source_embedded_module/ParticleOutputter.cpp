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
#include "../../ParticlesStructure.h"

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
	if(	nc_close(ncFileId)!=NC_NOERR)
		handleErrorChangeStatus("couldn't close");
}

bool ParticleOutputter::getFileOpenStatus() {
	return fileIsOpened;
}

bool ParticleOutputter::writeToOutput(std::vector<double> xpos, float time) {
	bool outStatus = true;
	int opStatusTemp;

	opStatusTemp = nc_put_var1_float (ncFileId,timeVarId, &outputTimestepCounter,&time);
	if(opStatusTemp!= NC_NOERR)
		{handleOutputError("couldn't write time"); outStatus = false; }

	size_t currentStartIndices [] = {outputTimestepCounter, 0};

	size_t dimensionLengths [] = {1,xpos.size()};
	opStatusTemp = nc_put_vara_double(ncFileId,xposVarId,currentStartIndices,dimensionLengths,xpos.data());
	if (opStatusTemp != NC_NOERR)
	{ handleErrorOutputStatus("Couldn't write xpos",opStatusTemp); outStatus = false;}

	if(outStatus)
		++outputTimestepCounter;

	return outStatus;
}

void ParticleOutputter::initialise(int no_particles, std::string fileName) {


	if(nc_create(fileName.c_str(), NC_NETCDF4, &ncFileId) != NC_NOERR)
		handleErrorChangeStatus("Couldn't open file");


	if ((nc_def_dim(ncFileId,"time",NC_UNLIMITED,&timeDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create time dim");

	if ((nc_def_dim(ncFileId,"particleNo",NC_UNLIMITED,&partNoDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create particleNo dim");


	const int dimIdsArray [] = {timeDimId,partNoDimId};
	if (nc_def_var(ncFileId,"xpos",NC_FLOAT,2,dimIdsArray, &xposVarId) != NC_NOERR)
			handleErrorChangeStatus("Couldn't create xpos variable");
	if (nc_def_var(ncFileId,"time",NC_FLOAT,1,&timeDimId, &timeVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create time variable");

	//TODO: attributes (units etc.)


	// end definitions, because we're done here!
	nc_enddef(ncFileId);

	fileIsOpened = true;

}

void ParticleOutputter::handleOutputError(std::string message) {
	//TODO
	handleErrorChangeStatus(message);
}

bool ParticleOutputter::writeVecToOutput(int varId,
		std::vector<double> toOutput) {
	bool outStatus = true;
	int opStatusTemp;

	size_t currentStartIndices [] = {outputTimestepCounter, 0};

	size_t dimensionLengths [] = {1,toOutput.size()};
	opStatusTemp = nc_put_vara_double(ncFileId,xposVarId,currentStartIndices,dimensionLengths,toOutput.data());
	if (opStatusTemp != NC_NOERR)
	{ handleErrorOutputStatus("Couldn't write outvector",opStatusTemp); outStatus = false;}


	return outStatus;
}

bool ParticleOutputter::writeTimeToOutput(float time) {
	bool outStatus = true;
	int opStatusTemp;

	opStatusTemp = nc_put_var1_float (ncFileId,timeVarId, &outputTimestepCounter,&time);
	if(opStatusTemp!= NC_NOERR)
		{handleOutputError("couldn't write time"); outStatus = false; }

	return outStatus;
}

bool ParticleOutputter::writeToOutput(ParticlesStructure& particlesStructure, float time) {
	bool outStatus= true;

	if(!writeVecToOutput(xposVarId,particlesStructure.getXposArray()))
	{
		std::cerr<< "couldn't write xpos" ; outStatus = false;
	}
	if(!writeTimeToOutput(time))
	{
		/*std::cerr<< "couldn't write time" ; */outStatus = false;
	}

	if(outStatus)
		++outputTimestepCounter;

	return outStatus;
}

void ParticleOutputter::handleErrorOutputStatus(std::string message,
		int errorid) {

	std::cerr << message << " Error string: " << nc_strerror(errorid) << std::endl;
}




