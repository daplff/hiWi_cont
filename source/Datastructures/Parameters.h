/*
 * Parameters.h
 *
 *  Created on: 3 Sep 2015
 *      Author: erik
 */

#ifndef PARAMETERS_H_
#define PARAMETERS_H_
#include <string>
#include <iosfwd>

class Parameters {
public:
	Parameters();
	virtual ~Parameters();

	void initialiseFromFile(std::string fileName);
	void initialiseFromStream(std::istream& inputStream);
	void initialiseFromString(const std::string& inputString);
	void initialiseDefault();
	void printParams();

	friend class FortranIO;
private:
	float rho0_in;
	float viscos_val_in;
    float dw_min_fric_in;
	float coef_in;
    float vlx_in;
    float vly_in;
    int np_in;
    int np_b_in;
    int npv_in;
    int i_openbc_in;
    float distmin_in;
    float tol_in;
    int ivar_dt_in;
    float dt_in;
    float CFL_in;
    float tmax_in;
    float out_in;
    float trec_ini_in;
    int i_restartRun_in;
    float hsm_b_max_in;
    int n0_in;
    int idebug_in;
    int iMUSCL_in;
    int i_dw_iter_in;
    int i_max_iter_in;
//--------following parameters should be set to values that makes the simulator ignore their effects---------
    float ref_p_in;
    float ref_h_in;
	float dw_min_ref_in;
    float xmin_ref_in;
    float ymin_ref_in;
    float dxx_ref_in;
    float dyy_ref_in;
    int ncx_ref_in;
    int ncy_ref_in;
    float dx_grd_in;
    float dy_grd_in;
};

#endif /* PARAMETERS_H_ */
