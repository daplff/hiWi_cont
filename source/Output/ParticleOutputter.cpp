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
#include <stdio.h>

void ParticleOutputter::handleErrorChangeStatus(std::string message)
{
	fileIsOpened = false;
	std::cerr << message << std::endl;
}



ParticleOutputter::ParticleOutputter() :
	fileIsOpened(false),
	baseFileName("output_default"),
	outputTimestepCounter(0),
	outputFileCounter(0)
{
	// TODO Auto-generated constructor stub

}
ParticleOutputter::ParticleOutputter(std::string inputBaseFileName) :
	fileIsOpened(false),
	baseFileName(inputBaseFileName),
	outputTimestepCounter(0),
	outputFileCounter(0)
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

bool ParticleOutputter::initialise(int no_particles, std::string inputFileName) {

	if(fileIsOpened)
	{
		std::cerr<< "There is already an opened file! can't have initialising on top of that, sorry.." << std::endl;
		return fileIsOpened;
	}
	if(nc_create(inputFileName.c_str(), NC_NETCDF4, &ncFileId) != NC_NOERR)
		handleErrorChangeStatus("Couldn't open file");


	if ((nc_def_dim(ncFileId,"time",NC_UNLIMITED,&timeDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create time dim");

	if ((nc_def_dim(ncFileId,"particleNo",NC_UNLIMITED,&partNoDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create particleNo dim");


	const int dimIdsArray [] = {timeDimId,partNoDimId};
	if (nc_def_var(ncFileId,"xpos",NC_FLOAT,2,dimIdsArray, &xposVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create xpos variable");
	if (nc_def_var(ncFileId,"ypos",NC_FLOAT,2,dimIdsArray, &yposVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create ypos variable");
	if (nc_def_var(ncFileId,"time",NC_FLOAT,1,&timeDimId, &timeVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create time variable");

	//TODO: attributes (units etc.)


	// end definitions, because we're done here!
	nc_enddef(ncFileId);

	fileIsOpened = true;

	return fileIsOpened;
}

bool ParticleOutputter::initialiseSingleUse(int no_particles, std::string inputFileName) {

	if(fileIsOpened)
	{
		std::cerr<< "There is already an opened file! can't have initialising on top of that, sorry.." << std::endl;
		return fileIsOpened;
	}
	if(nc_create(inputFileName.c_str(), NC_NETCDF4, &ncFileId) != NC_NOERR)
		handleErrorChangeStatus("Couldn't open file");


	if ((nc_def_dim(ncFileId,"time",1,&timeDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create time dim");

	if ((nc_def_dim(ncFileId,"particleNo",no_particles,&partNoDimId) != NC_NOERR))
		handleErrorChangeStatus("Couldn't create particleNo dim");


	const int dimIdsArray [] = {timeDimId,partNoDimId};
	if (nc_def_var(ncFileId,"xpos",NC_FLOAT,2,dimIdsArray, &xposVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create xpos variable");
	if (nc_def_var(ncFileId,"ypos",NC_FLOAT,2,dimIdsArray, &yposVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create ypos variable");
	if (nc_def_var(ncFileId,"time",NC_FLOAT,1,&timeDimId, &timeVarId) != NC_NOERR)
				handleErrorChangeStatus("Couldn't create time variable");

	//TODO: attributes (units etc.)


	// end definitions, because we're done here!
	nc_enddef(ncFileId);

	fileIsOpened = true;

	return fileIsOpened;
}

void ParticleOutputter::handleOutputError(std::string message) {
	//TODO
	handleErrorChangeStatus(message);
}

bool ParticleOutputter::writeVecToOutput(int varId, int no_particles, size_t timestep,
		std::vector<double> toOutput) {
	bool outStatus = true;
	int opStatusTemp;

	size_t currentStartIndices [] = {outputTimestepCounter, 0};

	size_t dimensionLengths [] = {1,no_particles};
	opStatusTemp = nc_put_vara_double(ncFileId,varId,currentStartIndices,dimensionLengths,toOutput.data());
	if (opStatusTemp != NC_NOERR)
	{ handleErrorOutputStatus("Couldn't write outvector",opStatusTemp); outStatus = false;}


	return outStatus;
}

bool ParticleOutputter::writeTimeToOutput(size_t timestep, float time) {
	bool outStatus = true;
	int opStatusTemp;

	opStatusTemp = nc_put_var1_float (ncFileId,timeVarId, &timestep,&time);
	if(opStatusTemp!= NC_NOERR)
		{handleOutputError("couldn't write time"); outStatus = false; }

	return outStatus;
}

bool ParticleOutputter::writeToOutput(ParticlesStructure& particlesStructure, float time) {
	bool outStatus= true;


	if(!writeVecToOutput(xposVarId,particlesStructure.getXposArray().size(), outputTimestepCounter, particlesStructure.getXposArray()))
	{
		std::cerr<< "couldn't write xpos" ; outStatus = false;
	}
	if(!writeVecToOutput(yposVarId,particlesStructure.getYposArray().size(),outputTimestepCounter,particlesStructure.getYposArray()))
	{
		std::cerr<< "couldn't write ypos" ; outStatus = false;
	}
	if(!writeTimeToOutput(outputTimestepCounter, time))
	{
		/*std::cerr<< "couldn't write time" ; */outStatus = false;
	}

	if(outStatus)
		++outputTimestepCounter;

	return outStatus;
}
bool ParticleOutputter::writeToOutputSingleUse(int no_particles, ParticlesStructure& particlesStructure, float time) {

	char fileNumberString [4];
	sprintf(fileNumberString,"%04d",static_cast<int>(outputFileCounter));
	std::string fileName = (baseFileName + fileNumberString) + ".nc";
	if (fileIsOpened)
	{
		if(	nc_close(ncFileId)!=NC_NOERR)
			handleErrorChangeStatus("couldn't close");
		fileIsOpened = false;
	}

	bool outStatus= initialiseSingleUse(no_particles,fileName);

	if(!writeVecToOutput(xposVarId,no_particles, 0, particlesStructure.getXposArray()))
	{
		std::cerr<< "couldn't write xpos" ; outStatus = false;
	}
	if(!writeVecToOutput(yposVarId,no_particles, 0, particlesStructure.getYposArray()))
	{
		std::cerr<< "couldn't write ypos" ; outStatus = false;
	}
	if(!writeTimeToOutput(0, time))
	{
		/*std::cerr<< "couldn't write time" ; */outStatus = false;
	}

	if(outStatus)
		++outputFileCounter;

	return outStatus;
}

void ParticleOutputter::handleErrorOutputStatus(std::string message,
		int errorid) {

	std::cerr << message << " Error string: " << nc_strerror(errorid) << std::endl;
}




