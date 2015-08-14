
#include <iostream>

extern "C"
{
//extern	int __global_2d_MOD_npar;
//	int global_2d_MOD_np;
//	void fortranfn_(int * changedvar);
	void C_HELPER_MODULE_set_simvars(double * xvector, int numParticles);
	void C_HELPER_MODULE_get_simvars(double * xvector, int numParticles);
	void C_HELPER_MODULE_dostuff();
}


//extern	"C" int __global_2d_MOD_npar;
//extern "C" 	int __global_2d_MOD_np;

int main(int argc, char **argv) {
	const int NUMDOUBLES = 5;
	double * myVec = new double[NUMDOUBLES];

	for (int i = 0; i<NUMDOUBLES; ++i)
	{
		myVec[i] = static_cast<double> (i);
	}

	C_HELPER_MODULE_set_simvars(myVec, NUMDOUBLES);
	C_HELPER_MODULE_set_simvars(myVec, NUMDOUBLES-1);
	C_HELPER_MODULE_dostuff();
	for (int i = 0; i<NUMDOUBLES; ++i)
	{
		std::cout<< myVec[i] << std::endl;
	}
	C_HELPER_MODULE_get_simvars(myVec,NUMDOUBLES);
	for (int i = 0; i<NUMDOUBLES; ++i)
	{
			std::cout<< myVec[i] << std::endl;
	}

//	int changedvar = 0;
//	std::cout << __global_2d_MOD_npar<<std::endl;
//	std::cout << global_2d_MOD_xp<<std::endl;
//	std::cout <<  __global_2d_MOD_np <<std::endl;
//	std::cout <<  global_2d_mp_np <<std::endl;
//	std::cout << changedvar <<std::endl;

//	fortranfn_(&changedvar);

//	std::cout << __global_2d_MOD_npar<<std::endl;
//		std::cout << global_2d_MOD_xp<<std::endl;
//		std::cout <<  __global_2d_MOD_np <<std::endl;
//		std::cout <<  global_2d_mp_np <<std::endl;
//	std::cout << changedvar <<std::endl;





	return 0;
}
