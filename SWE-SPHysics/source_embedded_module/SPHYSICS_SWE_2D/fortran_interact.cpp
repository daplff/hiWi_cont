/*
 * fortran_interact.cpp
 *
 *  Created on: May 4, 2015
 *      Author: wannerbe
 */
#include <iostream>
extern "C" {

void dosomethingwithfortran_(float * array, int * length);


}


void dosomethingwithfortran_(float * array, int * length)
{
const	int array_length = *length;

	for (int i = 0; i<array_length ; i+=100)
		std::cout<< i << ": " << array[i] << "\n";


}
