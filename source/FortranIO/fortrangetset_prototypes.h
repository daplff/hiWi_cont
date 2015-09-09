/*
 * fortrangetset_prototypes.h
 *
 *  Created on: 27 May 2015
 *      Author: erik
 */

#ifndef FORTRANGETSET_PROTOTYPES_H_
#define FORTRANGETSET_PROTOTYPES_H_

#define CONCAT(A,B) A##B
#define GETTERNAME(varname) CONCAT(FORTRAN_GETTER_,varname)
#define SETTERNAME(varname) CONCAT(FORTRAN_SETTER_,varname)

#define FORTRANGETSET(vartype, varname) \
	void GETTERNAME(varname)(vartype input);\
	void SETTERNAME(varname)(vartype input);

#define FORTRANGETSETARRAY(vartype, varname) \
	void GETTERNAME(varname)(vartype input, int length);\
	void SETTERNAME(varname)(vartype input, int length);



extern "C" {
FORTRANGETSETARRAY(float*,yp)
FORTRANGETSETARRAY(float*,xp)
FORTRANGETSETARRAY(float*,vp)
FORTRANGETSETARRAY(float*,up)
FORTRANGETSETARRAY(float*,dw)
FORTRANGETSETARRAY(float*,areap)
FORTRANGETSETARRAY(float*,h_var)
FORTRANGETSETARRAY(short*,iflag)
FORTRANGETSET(int*,np)
void C_HELPER_MODULE_add_particle(float* xp_n, float * yp_n, float * up_n, float * vp_n,
	     float * dw_n,float * ar_n, float * h_var_n, short * iflag_n);

void C_HELPER_MODULE_SPH_setparameters_fortran(float* rho0_in,float* viscos_val_in,float* dw_min_fric_in,
	     float* coef_in, float* vlx_in, float* vly_in, int * np_in, int * np_b_in, int * npv_in, int * i_openbc_in,
	     float* distmin_in, float* tol_in, int * ivar_dt_in, float* dt_in, float* CFL_in, float* tmax_in, float* out_in,
	     float* trec_ini_in, int * i_restartRun_in, float* hsm_b_max_in, int * n0_in, int * idebug_in,
	     int * iMUSCL_in, int * i_dw_iter_in, int * i_max_iter_in, float* ref_p_in, float* ref_h_in,
	     float* dw_min_ref_in, float* xmin_ref_in, float* ymin_ref_in, float* dxx_ref_in, float* dyy_ref_in,
	     int * ncx_ref_in, int * ncy_ref_in, float * dx_grd_in, float * dy_grd_in);

}

#define C_GET_FROM_FORTRAN(fortranvar,varname)\
		GETTERNAME(fortranvar) (varname)

#define C_SET_TO_FORTRAN(fortranvar,varname)\
		SETTERNAME(fortranvar) (varname)

#define C_GETARRAY_FROM_FORTRAN(fortranvar,varname,length) \
		GETTERNAME(fortranvar)(varname,length)

#define C_SETARRAY_TO_FORTRAN(fortranvar,varname,length) \
		SETTERNAME(fortranvar)(varname,length)



#endif /* FORTRANGETSET_PROTOTYPES_H_ */
