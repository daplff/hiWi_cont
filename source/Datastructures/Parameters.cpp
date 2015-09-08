/*
 * Parameters.cpp
 *
 *  Created on: 3 Sep 2015
 *      Author: erik
 */

#include "Parameters.h"

#include <cctype>
#include <iostream>
#include <fstream>
#include <sstream>

static const char indat_default []=
"     1000.000000\n\
        2.000000\n\
        0.001000\n\
        1.200000\n\
       10.000000\n\
       10.000000\n\
  7171\n\
 10609\n\
     0\n\
     0\n\
        0.000000\n\
    0.100000E-02\n\
     1\n\
       10.000000\n\
        0.400000\n\
        2.010000\n\
        0.500000\n\
        0.000000\n\
     0\n\
        0.118812\n\
     1\n\
     0\n\
     1\n\
     5\n\
    50\n\
        0.400000\n\
        0.000000\n\
        0.000000\n\
        0.000000\n\
        0.000000\n\
        0.120000\n\
        0.120000\n\
    84\n\
    84\n\
        1.000000\n\
        1.000000\n\
";

inline std::string getFirstWordOfNonemptyLine(std::istream& reader)
	{
		std::string temp("");
		while ((temp.empty() || (temp.length() == 1 && std::isspace(temp[0]))) && reader.rdstate() != reader.eofbit)
		{
		//	std::cout << counter++<<": "<< temp << std::endl;
			std::getline(reader,temp); 				//extract line
		}
		//std::cout<< "string: " << temp << std::endl;
//		std::cout << temp.substr(0,temp.find(' ')).size() << std::endl;
		int start = 0;
		while(std::isspace(temp[start]))
		{
		//	std::cout<< "start: " << start << std::endl;
			start++;
		}
		int end = start;
		while(!std::isspace(temp[end]) && end-start<temp.length())
		{
		//			std::cout<< "end: " << end << std::endl;
					end++;
		}

		//static int counter(0);
		//std::cout<< counter++ << ": returning " << temp.substr(start,end) << "! "<< std::endl;
		return temp.substr(start,end);	//return first word before space
	}

Parameters::Parameters()
	{
	// TODO Auto-generated constructor stub

}

Parameters::~Parameters() {
	// TODO Auto-generated destructor stub
}
void Parameters::initialiseFromFile(std::string fileName) {
	std::ifstream reader(fileName);

	initialiseFromStream(reader);

}

void Parameters::initialiseFromStream(std::istream& inputStream) {

	 rho0_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 viscos_val_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 dw_min_fric_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 coef_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 vlx_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 vly_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 np_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 np_b_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 npv_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 i_openbc_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 distmin_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 tol_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 ivar_dt_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 dt_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 CFL_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 tmax_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 out_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 trec_ini_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 i_restartRun_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 hsm_b_max_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 n0_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 idebug_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 iMUSCL_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 i_dw_iter_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 i_max_iter_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 ref_p_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 ref_h_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 dw_min_ref_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 xmin_ref_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 ymin_ref_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 dxx_ref_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 dyy_ref_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 ncx_ref_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 ncy_ref_in = std::stoi(getFirstWordOfNonemptyLine(inputStream));
	 dx_grd_in = std::stof(getFirstWordOfNonemptyLine(inputStream));
	 dy_grd_in = std::stof(getFirstWordOfNonemptyLine(inputStream));

}

void Parameters::initialiseFromString(const std::string& inputString) {
	std::stringstream inputStream(inputString);
	initialiseFromStream(inputStream);
}

void Parameters::initialiseDefault() {
	const std::string indat(indat_default);
	initialiseFromString(indat);
}

void Parameters::printParams() {
	std::cout<<std::endl<<rho0_in;
	std::cout<<std::endl<<viscos_val_in;
	std::cout<<std::endl<<dw_min_fric_in;
	std::cout<<std::endl<<coef_in;
	std::cout<<std::endl<<vlx_in;
	std::cout<<std::endl<<vly_in;
	std::cout<<std::endl<< np_in;
	std::cout<<std::endl<< np_b_in;
	std::cout<<std::endl<< npv_in;
	std::cout<<std::endl<< i_openbc_in;
	std::cout<<std::endl<<distmin_in;
	std::cout<<std::endl<<tol_in;
	std::cout<<std::endl<< ivar_dt_in;
	std::cout<<std::endl<<dt_in;
	std::cout<<std::endl<<CFL_in;
	std::cout<<std::endl<<tmax_in;
	std::cout<<std::endl<<out_in;
	std::cout<<std::endl<<trec_ini_in;
	std::cout<<std::endl<< i_restartRun_in;
	std::cout<<std::endl<<hsm_b_max_in;
	std::cout<<std::endl<< n0_in;
	std::cout<<std::endl<< idebug_in;
	std::cout<<std::endl<< iMUSCL_in;
	std::cout<<std::endl<< i_dw_iter_in;
	std::cout<<std::endl<< i_max_iter_in;
	std::cout<<std::endl<<ref_p_in;
	std::cout<<std::endl<<ref_h_in;
	std::cout<<std::endl<<dw_min_ref_in;
	std::cout<<std::endl<<xmin_ref_in;
	std::cout<<std::endl<<ymin_ref_in;
	std::cout<<std::endl<<dxx_ref_in;
	std::cout<<std::endl<<dyy_ref_in;
	std::cout<<std::endl<< ncx_ref_in;
	std::cout<<std::endl<< ncy_ref_in;
	std::cout<<std::endl<<dx_grd_in;
	std::cout<<std::endl<<dy_grd_in<<std::endl;
}





