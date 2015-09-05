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

!----------------------------------------------------------------------
c Copyright (c) 2013, Dr Renato Vacondio, Dr Benedict Rogers, Prof. Peter Stansby
c and Prof. Paolo Mignosa
c
c All rights reserved.
c
c This file is part of SWE-SPHYSICS.
c
c Redistribution and use in source and binary forms, with or without modification,
c are permitted provided that the following conditions are met:
c
c - Redistributions of source code must retain the above copyright notice,
c  this list of conditions and the following disclaimer.
c - Redistributions in binary form must reproduce the above copyright notice,
c  this list of conditions and the following disclaimer in the documentation
c  and/or other materials provided with the distribution.
c
c THE "SWE-SPHYSICS" IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
c ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
c OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
c THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
c SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
c OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
c HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
c TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
c EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
      subroutine getdata_from_c(rho0_in,viscos_val_in,dw_min_fric_in,
     + coef_in, vlx_in, vly_in, np_in, np_b_in, npv_in, i_openbc_in,
     + distmin_in, tol_in, ivar_dt_in, dt_in, CFL_in, tmax_in, out_in,
     + trec_ini_in, i_restartRun_in, hsm_b_max_in, n0_in, idebug_in,
     + iMUSCL_in, i_dw_iter_in, i_max_iter_in, ref_p_in, ref_h_in,
     + dw_min_ref_in, xmin_ref_in, ymin_ref_in, dxx_ref_in, dyy_ref_in,
     + ncx_ref_in, ncy_ref_in, dx_grd_in, dy_grd_in)
     + bind(c, name='SETPARAMETERS_FORTRAN_C')
      use global_2D

      real(c_float) rho0_in,viscos_val_in,dw_min_fric_in,
     + coef_in, vlx_in, vly_in,
     + distmin_in, tol_in,  dt_in, CFL_in, tmax_in, out_in,
     + trec_ini_in,  hsm_b_max_in,
     +  ref_p_in, ref_h_in,
     + dw_min_ref_in, xmin_ref_in, ymin_ref_in, dxx_ref_in, dyy_ref_in,
     + dx_grd_in, dy_grd_in

      integer(c_int) np_in, np_b_in, npv_in, i_openbc_in, ivar_dt_in,
     + i_restartRun_in, n0_in,
     + idebug_in, iMUSCL_in, i_dw_iter_in, i_max_iter_in,
     + ncx_ref_in, ncy_ref_in

      character supp_ini*4,name_ini*40

      integer i, iallo, ii, j, k, nplinkmax_coa, np_ob_y, i_openbc
      real area_min_coa, xp_ob_p, yp_ob_p

       !open(11,file='indat')

       rho0 = rho0_in
       write(*,*) 'rho0=',rho0, 'water density'
       viscos_val = viscos_val_in
       write(*,*) 'viscos_val=',viscos_val, 'artif viscosity'
       dw_min_fric = dw_min_fric_in
       write(*,*) 'dw_min_fric= ', dw_min_fric
       coef = coef_in
       write(*,*) 'coef=', coef
       vlx = vlx_in
       write(*,*)  'vlx=',vlx
       vly = vly_in
       write(*,*)  'vly=',vly
       np = np_in
       write(*,*)  'np=',np
       np_b = np_b_in
       write(*,*)  'np_b=',np_b
       npv = npv_in
       write(*,*)  'npv=',npv
       i_openbc = i_openbc_in
       write(*,*) 'i_openbc=', i_openbc
       distmin = distmin_in
       write(*,*)  'distmin=',distmin
       tol = tol_in
       write(*,*)  'tol=',tol
       ivar_dt = ivar_dt_in
       write(*,*)  'ivar_dt=',ivar_dt
       dt = dt_in
       write(*,*)  'dt=',dt
       CFL = CFL_in
       write(*,*)  'CFL=',CFL
       tmax = tmax_in
       write(*,*)  'tmax=',tmax
       out = out_in
       write(*,*)  'out=',out
       trec_ini = trec_ini_in
       write(*,*)  'trec_ini=',trec_ini
       i_restartRun  = i_restartRun_in
       write(*,*)  'i_restartRun=',i_restartRun
       hsm_b_max = hsm_b_max_in
       write(*,*)  'hsm_b_max=',hsm_b_max
       n0 = n0_in
       write(*,*)  'n0=',n0
       idebug = idebug_in
       write(*,*)  'idebug=',idebug
       iMUSCL = iMUSCL_in
       write(*,*)  'iMUSCL=',iMUSCL
       i_dw_iter = i_dw_iter_in
       write(*,*)  'i_dw_iter=',i_dw_iter
       i_max_iter = i_max_iter_in
       write(*,*)  'i_max_iter=',i_max_iter

       !refining
       ! read(11,44) area_lim
       ref_p = ref_p_in
       write(*,*)  'ref_p =',ref_p
       ref_h = ref_h_in
       write(*,*)  'ref_h =',ref_h
       dw_min_ref = dw_min_ref_in
       write(*,*)  'dw_min_ref=', dw_min_ref
       xmin_ref = xmin_ref_in
       write(*,*)  'xmin_ref=', xmin_ref
       ymin_ref = ymin_ref_in
       write(*,*)  'ymin_ref=', ymin_ref
       dxx_ref = dxx_ref_in
       write(*,*)  'dxx_ref=', dxx_ref
       dyy_ref = dyy_ref_in
       write(*,*)  'dyy_ref=', dyy_ref
       ncx_ref = ncx_ref_in
       write(*,*)  'ncx_ref=', ncx_ref
       ncy_ref = ncy_ref_in
       write(*,*)  'ncy_ref=', ncy_ref
       !reading data to export on regular grid
       dx_grd = dx_grd_in
       write(*,*)  'dx_grd=', dx_grd
       dy_grd = dy_grd_in
       write(*,*)  'dy_grd=', dy_grd

!       close(11)

!43     format(i6)
!44     format(f16.6)

! to be removed: no mirrored particles implemented
!      np_fixed=np

      one_over_2hsm_b=1/(2*hsm_b_max)
      dtmax=dt
      dt_vir=dt

      pi = 4.d0*datan(1.d0)
      twopi=2.*pi

      !kernel parameters
      alphad=15./(7.*pi)
      const_ker(1)=(1./6.)*alphad
      const_ker(2)=(2./3.)*alphad

       if (np .ge. npar ) then
         write(*,*) 'Number of particles exceeds npar in global_2D.f!'
         stop
       endif
       if (np .ge. npar ) then
         write(*,*) 'Number of particles exceeds npar in global_2D.f!'
         stop
       endif
       if (np_b .ge. npar ) then
         write(*,*) 'Number of bottom particles exceeds'
         write(*,*) 'npar in global_2D.f!: np_b= ', np_b, 'npar=', npar
         stop
       endif

C     Initial image to RUN

      if(i_restartRun.eq.1) then

         open(44,file='restart')
         read(44,100) itime,time,ngrab
         close(44)

100   format(i8,e16.8,i8)

         name_ini='ipart'
         write(*,*) name_ini

         write(supp_ini,'(i4.4)') ngrab
         name_ini='PART_'//supp_ini
         write(*,*) name_ini

       else

         itime = 0
         ngrab = 1
         grab_P = 0.0
         name_ini='ipart'

       endif

       print*,'i_restartRun ',i_restartRun
       write(*,*)'Reading in ', name_ini

c     --- Load Particle Position Data ---
       area_min=1.E18
       hmax=0
       open(13,file=name_ini)
       do i=1,np
           read(13,*) xp(i),yp(i),up(i),vp(i),dw(i),areap(i), h_var(i),
     + iflag(i)
c           write(*,*) xp(i),yp(i),up(i),vp(i),dw(i),areap(i), h_var(i),
c     + iflag(i)

          h_var0(i)=h_var(i)
          uo(i)=up(i) !inizialize velocity at n-1/2
          if ( hmax.lt.h_var(i) ) then
               hmax=h_var(i)
                 write(*,*) 'hmax=',hmax
          endif

           if (  areap(i) .lt. area_min )  then
                 area_min=areap(i)
                 write(*,*) 'area_min=',area_min, 'i= ', i
           endif

       enddo
       close(13)

       nstart=1
       write(*,*)' All ',np,' initial points read successfully'

c     --- End Load Particle Position Data ---

c     --- Load Virtual Particle Position Data ---

        open(16,file='ipartv')
        do i=1,npv
           read(16,*) xv(i), yv(i), vnxv(i), vnyv(i), dxv(i)
        enddo

c     --- END OF load Virtual Particle Position Data ---

c     --- Load bottom particle Position Data ---

       xmaxb=1.E-10
       xminb=1.E+10
       ymaxb=1.E-10
       yminb=1.E+10
       Vol_b_min=1.E18
       if (np_b.gt.0) then
          open(14,file='ibottom')
          do i=1,np_b
              read(14,*) xb(i), yb(i), hb(i), fr(i), hsm_b(i),
     + Vol_b(i)
              if(xb(i).gt.xmaxb) xmaxb=xb(i)
              if(xb(i).lt.xminb) xminb=xb(i)
              if(yb(i).gt.ymaxb) ymaxb=yb(i)
              if(yb(i).lt.yminb) yminb=yb(i)
              if( Vol_b_min.gt.Vol_b(i) ) Vol_b_min=Vol_b(i)
          enddo
          close(14)

          xmaxb = xmaxb + 0.1*hsm_b_max
          xminb = xminb - 0.1*hsm_b_max
          ymaxb = ymaxb + 0.1*hsm_b_max
          yminb = yminb - 0.1*hsm_b_max

          ncx_b = int( (xmaxb-xminb)/(2*hsm_b_max) ) + 1
          ncy_b = int( (ymaxb-yminb)/(2*hsm_b_max) ) + 1
          nct_b = ncx_b*ncy_b

! to be removed: better to dynamically allocate iboxb, ncb: introduce
!          if(nct_b .gt. nct_max) then
!                print*
!                print*,'ERROR in getdata.f'
!                print*,'No. of cells for bottom particles exceeds
!     +           nct_max in global_2D.f '
!                print*,'nct_b ',nct_b
!                print*,'nct_max ',nct_max
!                print*
!                stop
!          endif
       endif

c     --- End load bottom particle Position Data -

c     --- area_lim data ---
        nct_ref=ncx_ref*ncy_ref
        allocate(area_lim(nct_ref), STAT=iallo)
        if (iallo .ne. 0) then
            STOP 'allocation error area_lim'
        endif
        allocate(area_lim_coa(nct_ref), STAT=iallo)
        if (iallo .ne. 0) then
            STOP 'allocation error area_lim_coa'
        endif
        allocate(nc_coa(nct_ref), STAT=iallo)
        if (iallo .ne. 0) then
            STOP 'allocation error nc_coa'
        endif

        open(87,file='area_lim')
        read(87,*) area_lim(1), area_lim_coa(1)
        area_lim_min=area_lim(1)
        area_min_coa=area_lim_coa(1)
        write(*,*) 'area_min_coa=', area_min_coa
        do i=2,nct_ref

           read(87,*) area_lim(i), area_lim_coa(i)

           if (area_lim(i).lt.area_lim_min) then
              area_lim_min=area_lim(i)
           endif
           if (area_lim_coa(i).lt.area_min_coa) then
              area_min_coa=area_lim_coa(i)
              write(*,*) 'area_min_coa=', area_min_coa
           endif

        enddo
        close(87)

        nplinkmax_coa=n0*(int( (5.*dxx_ref)/sqrt(area_min_coa))+1 )**2
        nplinkmax_coa=min( nplinkmax_var,2000)
        write(*,*) 'nct_ref= ',nct_ref, 'nplinkmax_coa= ',nplinkmax_coa
        allocate(ibox_coa(nct_ref,nplinkmax_coa), STAT=iallo)
        if (iallo .ne. 0) then
            STOP 'allocation error for ibox_coa'
        endif

c     --- END OF area_lim data --

c ---  Load openbc data ---
       if (i_openbc.eq.1) then

       open(15,file='open_bc')
       read(15,*)   nopen_bou !number of open boundary conditions

       k=0 !counter of open boundary particles

       allocate( iflagbc(nopen_bou), STAT=iallo )
       allocate( xpbc(nopen_bou), STAT=iallo )
       allocate( ypbc(nopen_bou), STAT=iallo )
       allocate( lbc(nopen_bou), STAT=iallo )
       allocate( verbc(nopen_bou,2), STAT=iallo )
       allocate( verybc(nopen_bou), STAT=iallo )
       allocate( dxobc(nopen_bou), STAT=iallo )
       allocate( ndata_obc(nopen_bou), STAT=iallo )

       allocate(time_obc(nopen_bou,1000), STAT=iallo)
       allocate(dwbc_time(nopen_bou,1000), STAT=iallo)
       allocate(upbc_time(nopen_bou,1000), STAT=iallo)
       !allocate(ndata_obc(nopen_bou), STAT=iallo)   !BDR - Why twice???
       allocate(dwbc(nopen_bou), STAT=iallo)
       allocate(upbc(nopen_bou), STAT=iallo)

       do i=1,nopen_bou !loop over different boundary conditions !1

             read(15,*)  iflagbc(i)
             read(15,*)  xpbc(i)
             read(15,*)  ypbc(i)
             read(15,*)  lbc(i)
             read(15,*)  verbc(i,1)
             read(15,*)  verbc(i,2)
             read(15,*)  verybc(i)
             read(15,*)  dxobc(i)
             read(15,*)  ndata_obc(i) !number of data points for the i-th open boundary condition

             if (ndata_obc(i).gt.1000) then
                   write(*,*)  'increase allocation for ibc_time
     + variables'
                   stop
             endif

             do j=1,ndata_obc(i) !loop over time steps for i-th bc
               read(15,*) time_obc(i,j), dwbc_time(i,j), upbc_time(i,j)
             enddo !loop over time steps for i-th bc

             if (time_obc(i,ndata_obc(i)).lt.tmax) then
                 write(*,*) 'ERROR IN OPENBC variable data: max time of'
     + ,'boundary n=', i,'is',time_obc(i,j),'< tmax=',tmax
                 stop
             endif

             ! calculate np_ob_y: number of point for i open bc
             np_ob_y=int( lbc(i)/dxobc(i) )+1
             dxobc(i)=lbc(i)/np_ob_y

             do ii=1,np_ob_y !2

                do j=1,2 !coordinates of open bc particles !3

                k=k+1
                ibc(k)=i !index of boundary

                !transformatio to the new local coordinate system
                xp_ob_p=-(j-0.5)*dxobc(i)
                yp_ob_p=+(ii-0.5)*dxobc(i)*verybc(i)

                !back to the global coordinate system
                xp_ob(k)=xpbc(i)+xp_ob_p*verbc(i,1)-yp_ob_p*verbc(i,2)
                yp_ob(k)=ypbc(i)+xp_ob_p*verbc(i,2)+yp_ob_p*verbc(i,1)
                pm_ob(k)=dxobc(i)*dxobc(i)*rho0*dwbc_time(i,1) !this is correct just for supercritical flows or for outflow bc
                h_var_ob(k)=dxobc(i)*coef*2

                iflag_ob(k)=iflagbc(i)
                h_var0_ob(k)=h_var_ob(k)/2. !< it is useflul just when a ob particle become fluid particle
                dw0_ob(k)=dwbc_time(i,1)

                !write(*,*) 'open boundary particles', k, xp_ob(k), yp_ob(k), pm_ob(k), dw0_ob(k), h_var_ob(k), iflag_ob(k)

                !this is introuced for cases with totally dry bed
                if ( dxobc(i)*dxobc(i) .lt.area_min ) then
                   area_min=dxobc(i)*dxobc(i)
                endif

                if (hmax.lt.dxobc(i)*coef) then
                   hmax=dxobc(i)*coef
                endif

                enddo !3

             enddo !2

       enddo !loop over different boundary conditions !1

       close(15)

       np_ob=k

       else !no open boundaries

       nopen_bou=0
       np_ob=0

       endif

c ---  END OF Load openbc data ---

c
c ,,,, viscosity term eta2
c
       one_over_2hmax=1./(2.*hmax)
       eta  = 0.1*hmax
       eta2 = eta*eta

       !- Limits of domain -
       xmax_container = vlx
       xmin_container = 0
       ymax_container = vly
       ymin_container = 0

       xlim_left=0
       xlim_right=vlx
       xmin=xlim_left-2*hmax
       xmax=xlim_right+2*hmax

       ylim_left=0
       ylim_right=vly
       ymin=ylim_left-2*hmax
       ymax=ylim_right+2*hmax


       write(*,*)'xmin_container, xmax_container = ',
     &            xmin_container,xmax_container
       write(*,*)'ymin_container, ymax_container = ',
     &            ymin_container,ymax_container
       print*

c      -- Add small border region to prevent error --
c      -- when calculating icell.. in divide       --
       xmax = xmax + 0.1*hmax
       xmin = xmin - 0.1*hmax
       ymax = ymax + 0.1*hmax
       ymin = ymin - 0.1*hmax

       ! To place particles in a convenient place that leave the domain
       xtrash=2.*xmax
       ytrash=0.

       ncx = int( (xmax-xmin)*one_over_2hmax ) + 1
       ncy = int( (ymax-ymin)*one_over_2hmax ) + 1

       nct = ncx*ncy

       write(*,*)'xmin, xmax = ',xmin,xmax
       write(*,*)'ymin, ymax = ',ymin,ymax

       write(*,*)'ncx/y    = ',ncx,ncy
       write(*,*)'nct= ',nct

c-------------------------------------
c  --- export data on regular grid ---
c-------------------------------------

       nplot_x=int(vlx/dx_grd)+1
       nplot_y=int(vly/dy_grd)+1

       dx_grd=vlx/nplot_x
       dy_grd=vly/nplot_y

       !just for debug
       write(*,*) 'dx_grd,dy_grd =',dx_grd,dy_grd
       write(*,*) 'nplot_x,nplot_y =',nplot_x,nplot_y

       !allocate
       allocate(dw_grd(nplot_x,nplot_y), STAT=iallo)
       if (iallo .ne. 0) then
           STOP 'dw_grd allocation error'
       endif

       allocate(up_grd(nplot_x,nplot_y), STAT=iallo)
       if (iallo .ne. 0) then
           STOP 'up_grd allocation error'
       endif

       allocate(vp_grd(nplot_x,nplot_y), STAT=iallo)
       if (iallo .ne. 0) then
           STOP 'vp_grd allocation error'
       endif

       allocate(sum_grd(nplot_x,nplot_y), STAT=iallo)
       if (iallo .ne. 0) then
           STOP 'sum_grd allocation error'
       endif

       allocate(dx_fs_grd(nplot_x,nplot_y), STAT=iallo)
       if (iallo .ne. 0) then
           STOP 'dx_fs_grd allocation error'
       endif

       allocate(dy_fs_grd(nplot_x,nplot_y), STAT=iallo)
       if (iallo .ne. 0) then
           STOP 'dy_fs_grd allocation error'
       endif

       return
       end

c     ___________________



!----------------------------------------------------------------------
      end module





