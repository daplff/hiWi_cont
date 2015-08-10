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

class ParticleOutputter {
public:
	ParticleOutputter();
	virtual ~ParticleOutputter();
	bool getFileOpenStatus();
	bool writeToOutput(std::vector<double> xpos, float time);
	void initialise(int no_particles, std::string fileName);

private:
	bool fileIsOpened;
	std::string fileName;

	int ncidp, timeDimId,partNoDimId, xposVarId, timeVarId;
	size_t outputTimestepCounter;
	void handleErrorChangeStatus(std::string message);
	void handleOutputError(std::string message);

};

#endif /* PARTICLEOUTPUTTER_H_ */
