#include <iostream>

#include "source/Datastructures/Parameters.h"


int main(int argc, char **argv)
{
	Parameters tester;
	std::cout<<"testing testing\n" <<std::endl;


	tester.initialiseDefault();
	tester.printParams();
	
	return 0;
}
