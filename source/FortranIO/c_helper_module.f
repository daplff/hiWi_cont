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

        real (c_float), dimension(*)::xvector
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

        real (c_float), dimension(*)::xvector
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

       real (c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
           ctype_variable(i) = yp(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_yp (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_yp")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
           yp(i) = ctype_variable(i)
       enddo

       end subroutine


        subroutine get_fortran_variable_xp (ctype_variable, length)
     1   bind (c, name = "FORTRAN_GETTER_xp")
        use global_2d

        real(c_float), dimension(*)::ctype_variable
        integer (c_int), value::length

        integer i

        do i=1,length
            ctype_variable(i) = xp(i)
        enddo

        end subroutine

        subroutine set_fortran_variable_xp (ctype_variable, length)
     1   bind (c, name = "FORTRAN_SETTER_xp")
        use global_2d

        real(c_float), dimension(*)::ctype_variable
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

        subroutine get_fortran_variable_time (ctype_variable)
     1  bind (c, name = "FORTRAN_GETTER_time")
        use global_2d

        real(c_float) ctype_variable

        ctype_variable = time

        end subroutine

        subroutine set_fortran_variable_time (ctype_variable)
     1  bind (c, name = "FORTRAN_SETTER_time")
        use global_2d

        real(c_float) ctype_variable

        time = ctype_variable

        end subroutine

      subroutine get_fortran_variable_dt (ctype_variable)
     1    bind (c, name = "FORTRAN_GETTER_dt")
      use global_2d

      real(c_float) ctype_variable

         ctype_variable = dt

      end subroutine

      subroutine set_fortran_variable_dt (ctype_variable)
     1 bind (c, name = "FORTRAN_SETTER_dt")
      use global_2d

      real(c_float) ctype_variable


          dt = ctype_variable

      end subroutine

      subroutine get_fortran_variable_out (ctype_variable)
     1    bind (c, name = "FORTRAN_GETTER_out")
      use global_2d

      real(c_float) ctype_variable

         ctype_variable = out

      end subroutine

      subroutine set_fortran_variable_out (ctype_variable)
     1 bind (c, name = "FORTRAN_SETTER_out")
      use global_2d

      real(c_float) ctype_variable


          out = ctype_variable

      end subroutine

        !fortranarray_getset of dw
       subroutine get_fortran_variable_dw (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_dw")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         ctype_variable(i) = dw(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_dw (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_dw")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         dw(i) = ctype_variable(i)
       enddo

       end subroutine
        !fortranarray_getset of areap
       subroutine get_fortran_variable_areap (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_areap")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         ctype_variable(i) = areap(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_areap (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_areap")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         areap(i) = ctype_variable(i)
       enddo

       end subroutine
      !fortranarray_getset of up
       subroutine get_fortran_variable_up (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_up")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         ctype_variable(i) = up(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_up (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_up")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         up(i) = ctype_variable(i)
       enddo

       end subroutine
      !fortranarray_getset of vp
       subroutine get_fortran_variable_vp (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_vp")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         ctype_variable(i) = vp(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_vp (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_vp")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         vp(i) = ctype_variable(i)
       enddo

       end subroutine
      !fortranarray_getset of h_var
       subroutine get_fortran_variable_h_var (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_h_var")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         ctype_variable(i) = h_var(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_h_var (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_h_var")
       use global_2d

       real(c_float), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         h_var(i) = ctype_variable(i)
       enddo

       end subroutine
      !fortranarray_getset of iflag
       subroutine get_fortran_variable_iflag (ctype_variable, length)
     1 bind (c, name = "FORTRAN_GETTER_iflag")
       use global_2d

       integer(c_short), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         ctype_variable(i) = iflag(i)
       enddo

       end subroutine

       subroutine set_fortran_variable_iflag (ctype_variable, length)
     1  bind (c, name = "FORTRAN_SETTER_iflag")
       use global_2d

       integer(c_short), dimension(*)::ctype_variable
       integer (c_int), value::length
       integer i

       do i=1,length
         iflag(i) = ctype_variable(i)
       enddo

       end subroutine

      subroutine get_fortran_variable_tmax (ctype_variable)
     1    bind (c, name = "FORTRAN_GETTER_tmax")
      use global_2d

      real(c_float) ctype_variable

         ctype_variable = tmax

      end subroutine

      subroutine set_fortran_variable_tmax (ctype_variable)
     1 bind (c, name = "FORTRAN_SETTER_tmax")
      use global_2d

      real(c_float) ctype_variable


          tmax = ctype_variable

      end subroutine

      subroutine add_particle (xp_n,yp_n,up_n,vp_n,
     + dw_n,ar_n,h_var_n, iflag_n) !same order as output
     + bind (c, name='C_HELPER_MODULE_add_particle')


      use global_2D

      real (c_float) xp_n,yp_n,up_n,vp_n,
     + h_var_n,dw_n,ar_n
      integer (c_short) iflag_n

      integer i
		np = np + 1
		i = np
         xp(i)=xp_n
         yp(i)=yp_n
         up(i)=up_n
         vp(i)=vp_n
         uo(i)=up_n
         vo(i)=vp_n
         pm(i)=dw_n * rho0 * ar_n	!mass?
         h_var(i)=h_var_n
         h_var0(i)=h_var_n
         rhop(i)=rho0 * dw_n		!this is some kind of pressure?
         dw(i)=dw_n !also calculated - depth
         dw0(i)=dw_n				! init depth
         alphap(i)= rho0 * dw_n * dm !IGNORE! it's calculated in ac_dw/ac_alpha
         cs(i)=sqrt(grav*dw_n)
         iflag(i)=1
         pm0(i)= dw_n * rho0 * ar_n
         ar(i)=ar_n					!this is area of particle








         !--------------------------------

         end subroutine


      end module





