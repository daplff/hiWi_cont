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
#include "fortrangetset_prototypes.h"

extern "C"
{
FORTRANGETSET(int * ,np)

FORTRANGETSET(float *, time)
}


void print_vector(std::vector<double> input, int step)
{
	for(int i=0;i<input.size(); i+= step)
	{
		std::cout<<input[i] << "\t";
	}
}
void print_vectors(std::vector<double> xInput, std::vector<double> yInput, int step)
{
	for(int i=0;i<xInput.size(); i+= step)
	{
		std::cout<<"("  << xInput[i] <<","<<yInput[i]<< ")\t";
	}
}


class Parameters{};



int main(int argc, char * argv [])
{

	Parameters parameters;
	ParticlesStructure particles;
	SPHysics sphysics;
	sphysics.initialize(parameters);

	std::vector<double>& xPosref = particles.getXposArray();
	std::vector<double>& yPosref = particles.getYposArray();

	int num_particles;
	C_GET_FROM_FORTRAN(np,& num_particles);
	xPosref.resize(num_particles);
	yPosref.resize(num_particles);
	sphysics.get_variables(particles);

//	print_vector(xPosref,500);
	std::cout<<"starting while loop" << std::endl;


	double tempXpos = 0.0;
	float time= 0.0;
	while (tempXpos!=-2.0)
	{
		std::cout << "---------------running timestep ------------------------" << std::endl;

		sphysics.runTimestep(particles);
		std::cout<< "every 10 000th particles: \t";
		print_vectors(particles.getXposArray(),particles.getYposArray(), 10000);
		std::cout<< std::endl << "change xpos of first particle: (0 for unchanged, -2 for quit loop)" << std::endl << std::endl;
		std::cin>> tempXpos;
		C_GET_FROM_FORTRAN(time,& time);
		sphysics.netcdfOutput(particles,time);
		sphysics.simulatorOutput();
		if(tempXpos!=0.0)
		{
			xPosref[0]=tempXpos;
		}

	}


	return 0;

}
