/*
 * ParticleOutputter.h
 *
 *  Created on: 10 Aug 2015
 *      Author: erik
 */

#ifndef PARTICLEOUTPUTTER_H_
#define PARTICLEOUTPUTTER_H_
#include <iosfwd>
#include <vector>
#include <string>

class ParticlesStructure;

class ParticleOutputter {
public:
	ParticleOutputter();
	ParticleOutputter(std::string inputBaseFileName);
	virtual ~ParticleOutputter();
	bool getFileOpenStatus();
	bool writeToOutput(std::vector<double> xpos, float time);
	bool writeVecToOutput(int varId, int no_particles, size_t timestep, std::vector<double> toOutput);
	bool writeTimeToOutput(size_t timestep, float time);
	bool initialise(int no_particles, std::string fileName);
	bool initialiseSingleUse(int no_particles, std::string fileName);
	bool writeToOutput(ParticlesStructure& particlesStructure, float time);
	bool writeToOutputSingleUse(int no_particles, ParticlesStructure& particlesStructure, float time);

private:
	bool fileIsOpened;
	std::string baseFileName;

	int ncFileId, timeDimId,partNoDimId, xposVarId, yposVarId, timeVarId;
	size_t outputTimestepCounter; //for multitimestep file
	size_t outputFileCounter;	//for single timestep per file
	void handleErrorChangeStatus(std::string message);
	void handleOutputError(std::string message);
	void handleErrorOutputStatus(std::string message, int errorid);

};

#endif /* PARTICLEOUTPUTTER_H_ */
