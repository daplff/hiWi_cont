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
FORTRANGETSETARRAY(double*,yp)
FORTRANGETSETARRAY(double*,xp)

}

#define C_GET_FROM_FORTRAN(fortranvar,varname)\
		GETTERNAME(fortranvar) (varname)

#define C_SET_TO_FORTRAN(fortranvar,varname)\
		GETTERNAME(fortranvar) (varname)

#define C_GETARRAY_FROM_FORTRAN(fortranvar,varname,length) \
		GETTERNAME(fortranvar)(varname,length)

#define C_SETARRAY_TO_FORTRAN(fortranvar,varname,length) \
		SETTERNAME(fortranvar)(varname,length)



#endif /* FORTRANGETSET_PROTOTYPES_H_ */
