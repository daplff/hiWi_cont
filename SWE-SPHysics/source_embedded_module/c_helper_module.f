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

       subroutine get_fortran_variable_yp (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_yp")
       use global_2d

       real (c_double), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
           ctype_variable(i) = yp(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_yp (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_yp")
       use global_2d

       real(c_double), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
           yp(i) = ctype_variable(i)
       enddo

       end subroutine


        subroutine get_fortran_variable_xp (ctype_variable, length)
     1   bind (c, name = "FORTRAN_GETTER_xp")
        use global_2d

        real(c_double), dimension(*)::ctype_variable
        integer (c_int), value::length

        integer i

        do i=1,length
            ctype_variable(i) = xp(i)
        enddo

        end subroutine

        subroutine set_fortran_variable_xp (ctype_variable, length)
     1   bind (c, name = "FORTRAN_SETTER_xp")
        use global_2d

        real(c_double), dimension(*)::ctype_variable
        integer (c_int), value::length
        integer i

        do i=1,length
            xp(i) = ctype_variable(i)
        enddo

        end subroutine

        subroutine get_fortran_variable_np (ctype_variable)
     1  bind (c, name = "FORTRAN_GETTER_np")
        use global_2d

        integer(c_int) ctype_variable

        ctype_variable = np

        end subroutine

        subroutine set_fortran_variable_np (ctype_variable)
     1   bind (c, name = "FORTRAN_SETTER_np")
        use global_2d

        integer(c_int) ctype_variable

        np = ctype_variable

        end subroutine

      end module





