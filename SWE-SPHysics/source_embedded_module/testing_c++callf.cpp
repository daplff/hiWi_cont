/*
 * testing_c++callf.cpp
 *
 *  Created on: May 8, 2015
 *      Author: wannerbe
 */

#include<iostream>

extern "C" {
	void sph_init_();
	void sph_loopstep_();
	void get_time_(float * time);
	void set_t_end_(float * time);

}






int main(int argc, char * argv [])
{
	sph_init_();

	float time_fortran = 0.0;
	float time_end = 50.0;
	set_t_end_(&time_end);

	get_time_(&time_fortran);
	while(time_fortran < time_end)
	{
		sph_loopstep_();
		get_time_(&time_fortran);

		std::cout << "t = " << time_fortran << "\n";
	}

	std::cout << "done " <<std::endl;


	return 0;

}
