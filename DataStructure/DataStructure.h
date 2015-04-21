#ifndef DATASTRUCTURE_H
#define DATASTRUCTURE_H



#include "SimulationParameters.h"
#include "DomainVariables.h"


class DataStructure
{
public:
	DataStructure();
	~DataStructure();
	
private:
	SimulationParameters parameters;
	DomainVariables variables;

};

#endif // DATASTRUCTURE_H

