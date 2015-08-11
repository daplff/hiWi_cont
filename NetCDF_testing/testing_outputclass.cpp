/*
 * testing_outputclass.cpp
 *
 *  Created on: 11 Aug 2015
 *      Author: erik
 */
#include <vector>
#include "../SWE-SPHysics/source_embedded_module/ParticleOutputter.h"
#include <iostream>

#define VECTOR_SIZE_ 100
#define FILE_NAME_	"testfile.nc"

int main()
{
	ParticleOutputter particleOutputter;
	std::vector<double> stuffToOutput;

	for (int i = 0; i<VECTOR_SIZE_; ++i)
	{
		stuffToOutput.push_back((i+i*i+i*i*(i-24))/(1+2.0*i*i*(i-15)));
		//std::cout<< i << ": " << stuffToOutput[i] << std::endl;
	}

	particleOutputter.initialise(VECTOR_SIZE_,FILE_NAME_);

	if(particleOutputter.getFileOpenStatus())
	{
		float time=1.2;
		while(time<10)
		{
			if(particleOutputter.writeToOutput(stuffToOutput,time))
			{
				for(int i = 0; i<stuffToOutput.size(); ++i)
					stuffToOutput[i] *=time;

			}
			else
				break;

			time *=time;
		}
	}

	return 0;
}
