/*
 * testing_c++callf.cpp
 *
 *  Created on: May 8, 2015
 *      Author: wannerbe
 */


#include "SPHysics.h"
#include <vector>
#include <iostream>
#include "Datastructures/ParticlesStructure.h"
#include "FortranIO/fortrangetset_prototypes.h"

extern "C"
{
FORTRANGETSET(int * ,np)
FORTRANGETSET(float *, time)
FORTRANGETSET(float* ,out)
FORTRANGETSET(float*, dt)
}


void print_vector(std::vector<float> input, int step)
{
	for(int i=0;i<input.size(); i+= step)
	{
		std::cout<<input[i] << "\t";
	}
}
void print_vectors(std::vector<float> xInput, std::vector<float> yInput, int step)
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

	std::cout<<"initialising" << std::endl;

	sphysics.initialize(parameters);


	std::cout<<"initialised SPHysics, initialising vars" << std::endl;
	std::vector<float>& xPosref = particles.getXposArray();
	std::vector<float>& yPosref = particles.getYposArray();



	std::cout<<"initialised some refs" << std::endl;
	int num_particles;
	C_GET_FROM_FORTRAN(np,& num_particles);
//	xPosref.resize(num_particles);
//	yPosref.resize(num_particles);
	particles.resizeArrays(num_particles);



	std::cout<<"initialising variables from fortran" << std::endl;
	sphysics.get_variables(particles);
	float makeOutputEvery;
	C_GET_FROM_FORTRAN(out,& makeOutputEvery);

//	print_vector(xPosref,500);
	std::cout<<"starting while loop" << std::endl;


	float makeOutput = makeOutputEvery + 0.1; //to get initial state outputted
	float dt;
	float tempXpos = 0.0;
	float time= 0.0;
	while (tempXpos!=-2.0)
	{

		std::cout<< "every 10 000th particles: \t";
		print_vectors(particles.getXposArray(),particles.getYposArray(), 10000);
		std::cout<< std::endl << "change xpos of first particle: (0 for unchanged, -2 for quit loop)" << std::endl << std::endl;
		std::cin>> tempXpos;

		//ugly thing to make output same time as in original

		C_GET_FROM_FORTRAN(time,& time);
		C_GET_FROM_FORTRAN(dt, &dt);
		if(makeOutput >= makeOutputEvery)
		{
			sphysics.netcdfOutput(num_particles,particles,time);
			makeOutput = 0.0;
		}
		makeOutput+=dt;
		sphysics.simulatorOutput();
		if(tempXpos!=0.0 && tempXpos != -2.0)
		{
			xPosref[0]=tempXpos;
		}
		std::cout << "---------------running timestep ------------------------" << std::endl;

		sphysics.runTimestep(particles);
		int num_particlesTemp;
		C_GET_FROM_FORTRAN(np,& num_particlesTemp);
		if(num_particlesTemp != num_particles)
		{
			num_particles = num_particlesTemp;
			particles.resizeArrays(num_particles);
		}


	}


	return 0;

}
