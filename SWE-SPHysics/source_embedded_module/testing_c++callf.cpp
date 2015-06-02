/*
 * testing_c++callf.cpp
 *
 *  Created on: May 8, 2015
 *      Author: wannerbe
 */


#include "SPHysics.h"
#include <vector>
#include <iostream>
#include "../../ParticlesStructure.h"

void print_vector(std::vector<double> input, int step){

	for(int i=0;i<input.size(); i+= step)
	{
		std::cout<<input[i] << "\t";
	}
}


class Parameters{};



int main(int argc, char * argv [])
{

	Parameters parameters;
	ParticlesStructure particles;
	SPHysics sphysics;
	sphysics.initialize(parameters);
	sphysics.get_variables(particles);


	print_vector(particles.getXposArray(), 100);
	sphysics.runTimestep(particles);

	print_vector(particles.getXposArray(), 100);




	return 0;

}
