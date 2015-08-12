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
	virtual ~ParticleOutputter();
	bool getFileOpenStatus();
	bool writeToOutput(std::vector<double> xpos, float time);
	bool writeVecToOutput(int varId, std::vector<double> toOutput);
	bool writeTimeToOutput(float time);
	bool initialise(int no_particles, std::string fileName);
	bool writeToOutput(ParticlesStructure& particlesStructure, float time);

private:
	bool fileIsOpened;
	std::string fileName;

	int ncFileId, timeDimId,partNoDimId, xposVarId, yposVarId, timeVarId;
	size_t outputTimestepCounter;
	void handleErrorChangeStatus(std::string message);
	void handleOutputError(std::string message);
	void handleErrorOutputStatus(std::string message, int errorid);

};

#endif /* PARTICLEOUTPUTTER_H_ */
