#ifndef DOMAINVARIABLES_H
#define DOMAINVARIABLES_H

#include "ParticlesStructure.h"

class DomainVariables
{
public:
	DomainVariables();
	~DomainVariables();

		struct Bottom {
			int npar_b;
			ParticlesStructure particles;
		} bottom;
		
		struct Virtuals {
			ParticlesStructure particles;
		} virtuals;
		
		struct Fluid {
			int npar;
			ParticlesStructure particles;
		}
};

#endif // DOMAINVARIABLES_H
