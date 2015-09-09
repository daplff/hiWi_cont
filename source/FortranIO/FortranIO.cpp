#include "FortranIO.h"
#include "../Datastructures/ParticlesStructure.h"
#include "fortrangetset_prototypes.h"
#include <iostream>
#include "../Datastructures/Parameters.h"

FortranIO::FortranIO():
getXpos(true),
	getYpos(true),
	getXvel(true),
	getYvel(true),
	getSurfElev(true),
	getArea(true),
	getSmoothlen(true),
	getFlag(true),
	setXpos(false),
	setYpos(false),
	setXvel(false),
	setYvel(false),
	setSurfElev(false),
	setArea(false),
	setSmoothlen(false),
	setFlag(false)
{
}

FortranIO::~FortranIO()
{
}

void FortranIO::setRuntimeVariables(ParticlesStructure& particles) {
	int len = particles.getXposArray().size();
	if (particles.getYposArray().size() != len)
		{
			int tempYlen = particles.getYposArray().size();
			std::cerr<<
					"Warning! Xpos and Ypos vectors have different lengths! Setting editing length to min of xlen ("
					<< len << ") and ylen (" << tempYlen << ")!" <<std::endl;
			len = (len > tempYlen ? tempYlen : len);
		}
	if(setXpos)
	{
		float * xpos_ptr = (particles.getXposArray()).data();
		C_SETARRAY_TO_FORTRAN(xp,xpos_ptr, len) ;
	}
	if(setYpos)
	{
		float * ypos_ptr = (particles.getYposArray()).data();
		C_SETARRAY_TO_FORTRAN(yp,ypos_ptr, len) ;
	}
	if(setXvel)
	{
		float * xvel_ptr = (particles.getXvelArray()).data();
		C_SETARRAY_TO_FORTRAN(up,xvel_ptr, len) ;
	}
	if(setYvel)
	{
		float * yvel_ptr = (particles.getYvelArray()).data();
		C_SETARRAY_TO_FORTRAN(vp,yvel_ptr, len) ;
	}
	if(setSurfElev)
	{
		float * surfelev_ptr = (particles.getSurfElevationArray()).data();
		C_SETARRAY_TO_FORTRAN(dw,surfelev_ptr, len) ;
	}
	if(setArea)
	{
		float * area_ptr = (particles.getAreaArray()).data();
		C_SETARRAY_TO_FORTRAN(areap,area_ptr, len) ;
	}
	if(setSmoothlen)
	{
		float * smoothlen_ptr = (particles.getSmoothlenArray()).data();
		C_SETARRAY_TO_FORTRAN(h_var, smoothlen_ptr, len) ;
	}
	if(setFlag)
	{
		short * iflag_ptr = (particles.getFlagArray()).data();
		C_SETARRAY_TO_FORTRAN(iflag,iflag_ptr, len) ;
	}




}

void FortranIO::getRuntimeVariables(ParticlesStructure& particles) {
//	float * xpos_ptr = (particles.getXposArray()).data();
//	float * ypos_ptr = (particles.getYposArray()).data();
//	float * xvel_ptr = (particles.getXvelArray()).data();
//	float * yvel_ptr = (particles.getYvelArray()).data();
//	float * area_ptr = (particles.getAreaArray()).data();
//	float * smoothlen_ptr = (particles.getSmoothlenArray()).data();
//	short * iflag_ptr = (particles.getFlagArray()).data();
//	int len = particles.getXposArray().size();
//	if (particles.getYposArray().size() != len)
//	{
//		int tempYlen = particles.getYposArray().size();
//		std::cerr<<
//				"Warning! Xpos and Ypos vectors have different lengths! Setting editing length to min of xlen ("
//				<< len << ") and ylen (" << tempYlen << ")!" <<std::endl;
//		len = (len > tempYlen ? tempYlen : len);
//	}
//
//	C_GETARRAY_FROM_FORTRAN(xp, xpos_ptr, len);
//	C_GETARRAY_FROM_FORTRAN(yp, ypos_ptr, len);
	if(getXpos || getYpos || getXvel || getYvel || getArea || getSmoothlen || getSurfElev || getFlag)
	{
		int len;
		C_GET_FROM_FORTRAN(np, &len);

		particles.resizeArrays(len);

		//	if (particles.getYposArray().size() != len)
		//		{
		//			int tempYlen = particles.getYposArray().size();
		//			std::cerr<<
		//					"Warning! Xpos and Ypos vectors have different lengths! Setting editing length to min of xlen ("
		//					<< len << ") and ylen (" << tempYlen << ")!" <<std::endl;
		//			len = (len > tempYlen ? tempYlen : len);
		//		}

		if(getXpos)
		{
			float * xpos_ptr = (particles.getXposArray()).data();
			C_GETARRAY_FROM_FORTRAN(xp,xpos_ptr, len) ;
		}
		if(getYpos)
		{
			float * ypos_ptr = (particles.getYposArray()).data();
			C_GETARRAY_FROM_FORTRAN(yp,ypos_ptr, len) ;
		}
		if(getXvel)
		{
			float * xvel_ptr = (particles.getXvelArray()).data();
			C_GETARRAY_FROM_FORTRAN(up,xvel_ptr, len) ;
		}
		if(getYvel)
		{
			float * yvel_ptr = (particles.getYvelArray()).data();
			C_GETARRAY_FROM_FORTRAN(vp,yvel_ptr, len) ;
		}
		if(getSurfElev)
		{
			float * surfelev_ptr = (particles.getSurfElevationArray()).data();
			C_GETARRAY_FROM_FORTRAN(dw,surfelev_ptr, len) ;
		}
		if(getArea)
		{
			float * area_ptr = (particles.getAreaArray()).data();
			C_GETARRAY_FROM_FORTRAN(areap,area_ptr, len) ;
		}
		if(getSmoothlen)
		{
			float * smoothlen_ptr = (particles.getSmoothlenArray()).data();
			C_GETARRAY_FROM_FORTRAN(h_var, smoothlen_ptr, len) ;
		}
		if(getFlag)
		{
			short * iflag_ptr = (particles.getFlagArray()).data();
			C_GETARRAY_FROM_FORTRAN(iflag,iflag_ptr, len) ;
		}
	}
}

void FortranIO::getRuntimeParameters() {
}

void FortranIO::setParameters(Parameters& parameters) {
	//parameters.printParams();
	C_HELPER_MODULE_SPH_setparameters_fortran(&parameters.rho0_in,
			&parameters.viscos_val_in,
			&parameters.dw_min_fric_in,
			&parameters.coef_in,
			&parameters.vlx_in,
			&parameters.vly_in,
			&parameters.np_in,
			&parameters.np_b_in,
			&parameters.npv_in,
			&parameters.i_openbc_in,
			&parameters.distmin_in,
			&parameters.tol_in,
			&parameters.ivar_dt_in,
			&parameters.dt_in,
			&parameters.CFL_in,
			&parameters.tmax_in,
			&parameters.out_in,
			&parameters.trec_ini_in,
			&parameters.i_restartRun_in,
			&parameters.hsm_b_max_in,
			&parameters.n0_in,
			&parameters.idebug_in,
			&parameters.iMUSCL_in,
			&parameters.i_dw_iter_in,
			&parameters.i_max_iter_in,
			&parameters.ref_p_in,
			&parameters.ref_h_in,
			&parameters.dw_min_ref_in,
			&parameters.xmin_ref_in,
			&parameters.ymin_ref_in,
			&parameters.dxx_ref_in,
			&parameters.dyy_ref_in,
			&parameters.ncx_ref_in,
			&parameters.ncy_ref_in,
			&parameters.dx_grd_in,
			&parameters.dy_grd_in);



}
