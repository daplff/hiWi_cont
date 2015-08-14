
#include <iostream>

extern "C"
{
	int global2d_mp_npar;
	int global2d_mp_xp[];
}

int main(int argc, char **argv) {
	std::cout << global2d_mp_npar;
	std::cout << global2d_mp_xp;




	
}
