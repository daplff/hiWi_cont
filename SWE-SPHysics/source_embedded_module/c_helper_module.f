!       FIXED FORM FORTRAN!!!
!       fortranfn.f
!
!       Created on: May 20, 2015
!     Author: wannerbe
!
      module c_helper_module
        use iso_c_binding

        contains
      subroutine fortran_copy_simul_variable(xvector, nparticles)
     1  bind(c, name = "C_HELPER_MODULE_set_simvars")
        use global_2D

        real (c_double), dimension(*)::xvector
        integer (c_int), value::nparticles
        integer i

        do i=1,nparticles
            xp(i) = xvector(i)
            iflag(i) = 1
        enddo

        if (nparticles.lt.np) then
            do i=nparticles+1,np
                iflag(i) = 0;
            enddo
        endif


        np = nparticles


       end subroutine

!----------------------------------------------------------------------


       subroutine fortran_copyback_simul_variable(xvector, nparticles)
     1  bind(c, name = "C_HELPER_MODULE_get_simvars")
        use global_2D

        real (c_double), dimension(*)::xvector
        integer (c_int), value::nparticles
        integer i, npmax

        if (np.lt.nparticles) then
            npmax = np
        else
            npmax = nparticles
        endif


        do i=1,npmax
            xvector(i) = iflag(i)*xp(i)
        enddo

       end subroutine

!        subroutine fortran_copyback_dostuff()
!     1  bind(c, name = "C_HELPER_MODULE_dostuff")
!        use global_2D
!
!        integer i
!
!        do i=1,np
!            xp(i) = xp(i)**3
!       enddo
!
!
!
!        end subroutine


      end module
