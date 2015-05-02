#ifndef DOMAINVARIABLES_H
#define DOMAINVARIABLES_H

#include "ParticlesStructure.h"

class DomainVariables
{
public:
	DomainVariables();
	~DomainVariables();

		struct Bottom {
			ParticlesStructure particles;
		} bottom;
		
		struct Virtuals {
			ParticlesStructure particles;
		} virtuals;
		
		struct Fluid {
			int npar;
			ParticlesStructure particles;
		} fluid;
};

#endif // DOMAINVARIABLES_H
