#include "get_variable_macro.fpp"



program try_fortran
    implicit none

    real xp

    xp = 7

    write(*.*) xp

    fortran_get_variable_cbound( xp )

    write(*.*) xp

end program try_fortran


 CREATE_GETVAR_FUNCTION(xpp,real,cbound)
