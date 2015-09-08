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

      program SPHYSICSgen_2D

      implicit none
      
      integer, parameter :: npar=500000
      
      character (LEN=60) :: file_bedProfile

c -- fluid particles
      real xp(npar),yp(npar),dw(npar),up(npar),vp(npar), areap(npar),
     + h_var(npar)
      integer np, iflag(npar),i_fluid

      real area_max
      
c -- open bounaries
      real distmin
      
c -- closed boundaries

      real dxv_min
            
c -- virtual particles
      integer npv      
      
c -- bottom particles      
      real xb(npar), yb(npar), hb_SPH(npar), fr(npar), hsm_b(npar), 
     + Vol_b(npar), hsm_b_max
      integer np_b

      real g, grav, pi, rho0 , viscos_val, tol, dt, CFL, out, tmax, 
     + trec_ini, coef, vlx, vly, ref_p, ref_h, dx_grd, 
     + dy_grd, dw_min_fric
      
      integer i, idebug, ivisc, iMUSCL, i_dw_iter, i_max_iter, ivar_dt, 
     + n0, i_bedType, i_openbc, i_restartRun, iref, i_closebc, 
     + i_compile_opt, idouble
     
c -- restart file     
      integer nfile_Rest, np_rest, idummy
      real dummy
           
c -- refinement grid
      real dw_min_ref, xmin_ref, ymin_ref, dxx_ref, dyy_ref
      integer ncx_ref, ncy_ref      
      
      character supp*4, namefile*60
      
      
        print*
       write(*,*) '<SWE-SPHYSICS>  Copyright (C) <2013>'
       write(*,*) 
     & '<Dr Renato Vacondio, Dr Benedict Rogers, '
       write(*,*) 
     & 'Prof. Peter Stansby and Prof. Paolo Mignosa, '
       write(*,*) 'This program comes with ABSOLUTELY NO WARRANTY;    '
       write(*,*) 'This is free software, and you are welcome to      '
       write(*,*) 'redistribute it under conditions stated in         '
       write(*,*) 'the FreeBSD License;                               '

       print*
       print*,' ---       SPHYSICS_SWE_1D.F       ---'
       print*,' ---       Distributed under       ---'
       print*,' ---      the FreeBSD License      ---'
       print*
     
c       constants
c
      g=9.81
      grav=g
      pi=4.0*atan(1.0)

      write(*,*) 'Start a new RUN (0) or Restart an old RUN (1) ? '
      read(*,*) i_restartRun
      write(*,*) i_restartRun

      write(*,*) 'debug activated: 1 - for yes (output at every timestep)
     + 0 for no'
      read(*,*) idebug
      write(*,*) idebug
      
      write(*,*) 'fluid density (1000 kg/m3 for water):'
      read(*,*) rho0
      write(*,*) rho0

      write(*,*) 'stabilization term 1 - artificial viscosity, 
     + 2 - Lax Friedrichs flux, 3 - two shocks Riemann solver'
      read(*,*) ivisc
      write(*,*) ivisc

      write(*,*) 'valule of alpha for artificial viscosity'
      read(*,*) viscos_val  !2. 
      write(*,*) viscos_val  !2. 
      
      write(*,*) 'valule of alpha for artificial viscosity'
      read(*,*) dw_min_fric
      write(*,*) dw_min_fric

      write(*,*) '0 - No MUSCL reconstruction, 1 - MUSCL reconstruction)
     +'
      read(*,*) iMUSCL
      write(*,*) iMUSCL

      write(*,*) 'tolerance in the Newton - Raphson algorithm
     + (suggested 1.E-3)'
      read(*,*) tol
      write(*,*) tol
     
      write(*,*) 'number of time step for Newton - Raphson iterative 
     + procedure (suggested 10) if 1 the iterative procedure is operated 
     + every time steps'
      read(*,*) i_dw_iter
      write(*,*) i_dw_iter

      write(*,*) 'maximum number of iterations in the Newton - Raphson  
     + iterative procedure'
      read(*,*) i_max_iter
      write(*,*) i_max_iter

      write(*,*) 'variable time step: 1 for yes, 2 for no'
      read(*,*) ivar_dt
      write(*,*) ivar_dt 

      write(*,*) ' time step, or maximum time step if variable time 
     + step activated (s)'
      read(*,*) dt !=0
      write(*,*) dt !=0  

      write(*,*) 'Courant number'
      read(*,*) CFL
      write(*,*) CFL

      write(*,*) 'output interval (seconds)'
      read(*,*) out
      write(*,*) out

      write(*,*) 'End of the simulation (seconds)'
      read(*,*) tmax  
      write(*,*) tmax   

      write(*,*) 'time step for intitial output (seconds)'
      read(*,*) trec_ini 
      write(*,*) trec_ini 

      write(*,*) 'smoothing length coefficient: h=dx*coef 
     + (suggested 1.2)'
      read(*,*) coef
      write(*,*) coef

      write(*,*) 'x - length of the domain (m)'
      read(*,*) vlx
      write(*,*) vlx

      write(*,*) 'y - length of the domain (m)'
      read(*,*) vly
      write(*,*) vly
      
      write(*,*) 'x size of the regular grid for output'
      read(*,*) dx_grd
      write(*,*) dx_grd
      
      write(*,*) 'y size of the regular grid for output'
      read(*,*) dy_grd
      write(*,*) dy_grd
      
!
! -- open boundaries --
!
      write(*,*) 'open boundaries: 1 for yes, 0 for no'
      read(*,*) i_openbc
      write(*,*) i_openbc
      
      if (i_openbc.eq.1) then
          write(*,*) 'minimum distance between particles in the buffer 
     + zone (m)'
          read(*,*) distmin
          write(*,*) distmin
          call open_boundary(tmax)
      else
          distmin=0.
          i_openbc=0
      endif
 
!     
! -- define closed boundaries via MVPB method
!
      write(*,*) 'closed boundaries: 1 for yes, 0 for no'
      read(*,*) i_closebc
      write(*,*) i_closebc
      if (i_closebc.eq.1) then
          call closed_boundary(npv, dxv_min)
      else
          npv=0
      endif

!
! -- define bed elevation
!
      write(*,*) ' Choose Bed Profile  ??'
      write(*,*) ' 1 = flat bed in blocks '
      write(*,*) ' 2 = Parabolic basin, 3 = Load Bed Profile from File'
      read(*,*) i_bedType
      write(*,*) i_bedType

      if(i_bedType.eq.1)then !plane slope
        call slopingBed(np_b, xb, yb, hb_SPH, fr, hsm_b, Vol_b, 
     + hsm_b_max)
      elseif(i_bedType.eq.2)then !parabolic slope
        call parabolicBed(np_b, xb, yb, hb_SPH, fr, hsm_b, Vol_b, 
     + hsm_b_max)
      elseif(i_bedType.eq.3)then
        print*
        write(*,*) ' Enter filename of Bed Profile'
        read(*,*) file_bedProfile
        write(*,*) file_bedProfile
        call read_bedProfile(file_bedProfile, np_b, xb, yb, hb_SPH, fr, 
     + hsm_b, Vol_b, hsm_b_max)
      elseif(i_bedType.ne.0)then
        print*
        print*,'Incorrect Choice for Bed Profile'
        stop        
      endif
      
      if (np_b.gt.npar) then
          write(*,*) 'np_b > npar, increase npar.'
          stop
      endif
      
      write(*,*) 'write ibottom'
      open(77,file='ibottom')
      do i=1,np_b
           write(77,76) xb(i), yb(i), hb_SPH(i), fr(i), hsm_b(i), 
     + Vol_b(i)
      enddo
76    format( 6f18.6)
      close (77)
!      
! -- fluid particle position
!

      write(*,*) ' Choose fluid particle distribution  ??'
      write(*,*) ' 1 = square blocks '
      write(*,*) ' 2 = circular'
      read(*,*) i_fluid
      write(*,*) i_fluid
      
      if (i_fluid.eq.1) then
      
          call surface_define(coef, npar, np, xp, yp, dw, up, vp, 
     +  h_var, areap)
     
      elseif (i_fluid.eq.2) then
     
          call surface_circular(coef, npar, np, xp, yp, dw, up, vp, 
     +  h_var, areap, vlx, vly, dxv_min)
     
      else
     
        write(*,*) 'ERROR: i_fluid should be equal to 1 or 2'
        STOP
     
      endif   
           
       if (i_restartRun.eq.1) then  
          open(70,file='restart') 
          read(70,*) idummy, dummy, nfile_Rest, np_rest 
          close(70)
          np=np_rest
          write(supp,'(i4.4)') nfile_Rest
          namefile='PART_'//supp
       endif
       
       write(*,*) 'write ipart'
       open(76,file='ipart')
       area_max=0.
       if (i_restartRun.eq.0) then
       
           do i=1,np  
               write(76,100) xp(i),yp(i),up(i),vp(i),dw(i),areap(i),
     + h_var(i), 1   !BDR - the integer requires a format statement!!!!
c               write(76,*) xp(i),yp(i),up(i),vp(i),dw(i),areap(i),
c     + h_var(i), 1
               area_max=max(area_max,areap(i))
           enddo
       else
       
           open(24,file=namefile) 
           write(*,*) namefile

           np=np_rest

           do i=1,np_rest
              read(24,100) xp(i),yp(i),up(i),vp(i),dw(i),areap(i),
     + h_var(i),iflag(i)
              write(76,*)xp(i),yp(i),up(i),vp(i),dw(i),areap(i),
     + h_var(i),iflag(i)
              area_max=max(area_max,areap(i))
           enddo
           close(24)

       endif
       close (76) 
       

      write(*,*) 'write indat'

      if (np.gt.npar) then
          write(*,*) 'np > npar, increase npar.'
          stop
      endif
      
      write(*,*) 'refinement procedure 1 for yes 0 for no'
      read(*,*) iref
      write(*,*) iref

      if (iref.eq.1) then

          write(*,*) 'eta coefficient for refined particle, distance 
     + (suggested 0.4)'
          read(*,*) ref_p
          write(*,*) ref_p

         write(*,*) 'alpha coefficient for refined particle, smoothing 
     + length (suggested 0.9)'
          read(*,*) ref_h
          write(*,*) ref_h

          n0=7

      else

          ref_h=0.
          ref_p=0.4
          n0=1

      endif

      call refim_param(iref, vlx, vly, dw_min_ref, xmin_ref, 
     + ymin_ref, dxx_ref, dyy_ref, ncx_ref, ncy_ref, area_max)
      
!
! the indat file should be fixed when I'll fix the getdata in the actual engine
!

      open(75,file='indat')      
       write(75,44) rho0
       write(75,44) viscos_val
       write(75,44) dw_min_fric
       write(75,44) coef
       write(75,44) vlx
       write(75,44) vly
       write(75,43) np
       write(75,43) np_b
       write(75,43) npv !number of virtual particles
       write(75,43) i_openbc
       write(75,44) distmin
       write(75,45) tol
       write(75,43) ivar_dt
       write(75,44) dt
       write(75,44) CFL
       write(75,44) tmax
       write(75,44) out
       write(75,44) trec_ini
       write(75,43) i_restartRun       
       write(75,44) hsm_b_max !smoothing length for the bottom particles
       write(75,43) n0 !max number of particles in one cell
       write(75,43) idebug
       write(75,43) iMUSCL
       write(75,43) i_dw_iter
       write(75,43) i_max_iter
       
       !refining parameters
       !write(75,44) area_lim*ref_lim    !limits of area to refine
       write(75,44) ref_p             !position of refined particles (coeff)
       write(75,44) ref_h               !smooth. length of refined particles
       write(75,44) dw_min_ref             !minimum water depth to activate refining
       write(75,44) xmin_ref		!constant 0
       write(75,44) ymin_ref		!constant 0 -set in refim param
       write(75,44) dxx_ref
       write(75,44) dyy_ref
       write(75,43) ncx_ref
       write(75,43) ncy_ref
       
       !openbc
c       write(75,43) nopen_bou read this directly from the open_bc file
       
       !parameters to export on regular grid
       write(75,44) dx_grd
       write(75,44) dy_grd
       
       close(75)
       
      write(*,*) 'Which compiler is desired? '
      write(*,*) 'Linux: 1 = gfortran, 2 = ifort'
      write(*,*) 'Windows: 3 = ifort, 4 = Silverfrost FTN95'
      read(*,*) i_compile_opt
      write(*,*) i_compile_opt

      write(*,*) '1 for single precision, 2 for double precision'
      read(*,*) idouble
      write(*,*) idouble

      if(i_compile_opt.eq.1) then
        call tocompile_unixStyle(i_compile_opt,ivisc,idebug,
     &                           idouble,iMUSCL,iref)
      elseif(i_compile_opt.eq.2) then
        call tocompile_unixStyle(i_compile_opt,ivisc,idebug,
     &                           idouble,iMUSCL,iref)
      elseif(i_compile_opt.eq.3) then
        call tocompile_win_ifort(ivisc,idebug,idouble,iMUSCL,iref)
      else  !if(i_compile_opt.eq.4) then
        call tocompile_unixStyle(i_compile_opt,ivisc,idebug,
     &                           idouble,iMUSCL,iref)
      end if
                
43     format(i6)
44     format(f16.6)
45     format(e16.6)
100    format(7e16.8,i8)

      end 
      
!--- end of initial data for SPHYSICS_SWE_1D ---


c
c***********************************************************************
c      
c     ____________________ SUBROUTINE open_boundary

      subroutine open_boundary(tmax)

      implicit none

      character namefile*40

      integer number_obc, npoint, i, j, i_steady_obc, iflagbc

      real xp_obc, yp_obc, l_obc, ver_obc(2), verz_obc, dx_obc, dw_obc, 
     + up_obc, tmax, time

      open(101,file='open_bc')

      write(*,*) ' number of boundary conditions '
      read(*,*)   number_obc    
      write(*,*)  number_obc
      write(101,*)  number_obc

      do i=1,number_obc
      
         write(*,*) ' kind of open boundary condition: 2 for inflow, 
     + 3 for outflow '
         read(*,*)   iflagbc
         write(*,*)  iflagbc
         write(101,*)  iflagbc

         write(*,*) ' x-coordinate for the beginning of the openbc (m) '
         read(*,*)   xp_obc
         write(*,*)  xp_obc
         write(101,*)  xp_obc

         write(*,*) ' y-coordinate for the beginning of the openbc (m) '
         read(*,*)   yp_obc
         write(*,*)  yp_obc
         write(101,*)  yp_obc

         write(*,*) ' length of the bc (m)'
         read(*,*)   l_obc
         write(*,*)  l_obc
         write(101,*)  l_obc

         write(*,*) ' x - coord of the unit vector normal to the bc'
         read(*,*)   ver_obc(1)
         write(*,*)  ver_obc(1)
         write(101,*)  ver_obc(1)

         write(*,*) ' y - coord of the unit vector normal to the bc'
         read(*,*)   ver_obc(2)
         write(*,*)  ver_obc(2)
         write(101,*)  ver_obc(2)

         write(*,*) ' 1 if z is exiting from the plane x-y, -1 if it is 
     + entering in the plane x-y'
         read(*,*)   verz_obc
         write(*,*)  verz_obc
         write(101,*)  verz_obc

         write(*,*) ' dx for open boundary particles'
         read(*,*)   dx_obc
         write(*,*)  dx_obc
         write(101,*)  dx_obc

         write(*,*) ' steady bc 0 for yes, 1 for no'
         read(*,*)   i_steady_obc
         write(*,*)  i_steady_obc

         if (i_steady_obc.eq.0) then !steady open boundary conditions

             write(*,*) 'water depth for the obc'
             read(*,*)   dw_obc
             write(*,*)  dw_obc
             write(*,*) 'velocity for the obc'
             read(*,*)   up_obc
             write(*,*)  up_obc
 
             write(101,*) '2'
             write(101,*) '0.', dw_obc, up_obc
             write(101,*) tmax, dw_obc, up_obc

         else !open boundary conditions variable in time
             write(*,*) 'write file name:'
             read(*,*)   namefile
             !namefile=trim(namefile)
             write(*,*)  namefile
             open(102,file=namefile,status='unknown' )
             read(102,*) npoint
             write(*,*) npoint
             write(101,*) npoint
             

             do j=1,npoint
                read(102,*) time, dw_obc, up_obc
                write(101,*) time, dw_obc, up_obc
                write(*,*) time, dw_obc, up_obc
             enddo
             close(102)
         endif 

      enddo

      close(101)

      end subroutine
      
c
c***********************************************************************
c      
c     ____________________ SUBROUTINE closed_boundary
          
      subroutine closed_boundary(npv,dxv_min)
      
      implicit none
      
      integer, parameter :: npar=500000
      
      real dxvmax
      
      integer iblock,  kk, npv, npv_temp,
     +  npv_block, j, i, i_start1, i_start2, ini
      integer iflagv(npar)
      real xv_ini, yv_ini, xv_fin, yv_fin, vnx, vny, length,
     + dxv_min, distij, alpha, dxvv, magn_vec
      real xv(npar), yv(npar), vnxv(npar), vnyv(npar), dxv(npar)

      real xc, yc, rad, dxcor, t, dt, pi, rr

      integer np_crf, k

      pi=4.0*atan(1.0)
      
      write(*,*) 'write ipartv'
      open(78,file='ipartv')
      
      dxv_min=1.E18
      
      write(*,*) ' do you want to add a straight closed boundary? 
     + 1 for yes 0 for no'
      read(*,*)   iblock
      write(*,*)  iblock

      kk=0
      do while (iblock.eq.1) !1

      
         write(*,*) 'starting point x - coord (m)'
         read(*,*)   xv_ini
         write(*,*)  xv_ini
         
         write(*,*) 'starting point y - coord (m)'
         read(*,*)   yv_ini
         write(*,*)  yv_ini
         
         write(*,*) 'marker starting point'
         read(*,*)   i_start1
         write(*,*)  i_start1
         
         write(*,*) 'endpoint x - coord (m)'
         read(*,*)   xv_fin
         write(*,*)  xv_fin
         
         write(*,*) 'endpoint y - coord (m)'
         read(*,*)   yv_fin
         write(*,*)  yv_fin
         
         write(*,*) 'marker endpoint point'
         read(*,*)   i_start2
         write(*,*)  i_start2
         
         write(*,*) 'distance between virtual particles (sugg. 0.5 dx)'
         read(*,*)   dxvv
         write(*,*)  dxvv
         
         write(*,*) 'unit vector pointing inside the domain, x coord'
         read(*,*)   vnx
         write(*,*)  vnx
         
         write(*,*) 'unit vector pointing inside the domain, y coord'
         read(*,*)   vny
         write(*,*)  vny
         
         length=sqrt( (xv_fin-xv_ini)**2+(yv_fin-yv_ini)**2 )
         
         npv_block=int(length/dxvv)+1
         dxvv=length/npv_block
         
         alpha=atan2(yv_fin-yv_ini, xv_fin-xv_ini)
         
         write(*,*) 'alpha=' ,alpha
         
         !this add one particle at the corner at the starting point (just for internal angles < 180 deg)
         if (i_start1.eq.1) then
             ini=2
             npv_block=npv_block+1
         else
             ini=1
         endif
         
         !this add one particle at the corner at the endpoint point (just for internal angles < 180 deg)
         if (i_start2.eq.1) then
             npv_block=npv_block+1
         endif
         
         write(*,*) ' npv_block=',  npv_block

         do i=1,npv_block+1 !2
         
            kk=kk+1
            
            xv(kk)=xv_ini+(i-ini)*dxvv*cos(alpha)
            yv(kk)=yv_ini+(i-ini)*dxvv*sin(alpha)
            vnxv(kk)=vnx 
            vnyv(kk)=vny 
            dxv(kk)=dxvv
         
         enddo
         
         dxv_min=min(dxv_min,dxvv)

         write(*,*) ' do you want to add another closed boundary? 
     + 1 for yes 0 for no'
         read(*,*)   iblock
         write(*,*)  iblock

      enddo !1

! circular closed boundary conditions

      write(*,*) 'do you want to add a circular closed boundary?
     +  1 for yes 0 for no'
      read(*,*)   iblock
      write(*,*)  iblock

      do while (iblock.eq.1) !1

         write(*,*) ' x - coordinate of the centre of the circle (m)'
         read(*,*)   xc
         write(*,*)  xc
         
         write(*,*) ' y - coordinate of the centre of the circle (m)'
         read(*,*)   yc
         write(*,*)  yc
         
         write(*,*) ' radius of the circle (m)'
         read(*,*)   rad
         write(*,*)  rad
         
         write(*,*) 'particle distance virtual particles (m)'
         read(*,*)   dxvv
         write(*,*)  dxvv

         np_crf=int(0.5*pi*rad/dxvv)+1; !number of particle in the i-th circle
         dt=0.5*pi/np_crf !d angle
         dxcor=dt*rad !dx
           
         t=0;
         do j=1,np_crf
            t=t+dt;
            do k=1,4
               kk=kk+1
               if (k.eq.1) then
                   xv(kk)=xc+rad*cos(t)
                   yv(kk)=yc+rad*sin(t)

               elseif (k.eq.2) then
                   xv(kk)=xc+rad*cos(t+pi*0.5)
                   yv(kk)=yc+rad*sin(t+pi*0.5)

               elseif (k.eq.3) then
                   xv(kk)=xc+rad*cos(t+pi)
                   yv(kk)=yc+rad*sin(t+pi)

               elseif (k.eq.4) then
                   xv(kk)=xc+rad*cos(t+1.5*pi)
                   yv(kk)=yc+rad*sin(t+1.5*pi)

               endif

               rr=sqrt( (xv(kk)-xc)*(xv(kk)-xc)+
     + (yv(kk)-yc)*(yv(kk)-yc) )
               vnxv(kk)=-(xv(kk)-xc)/rr
               vnyv(kk)=-(yv(kk)-yc)/rr
               dxv(kk)=dxvv
                 
            enddo
         enddo

         dxv_min=min(dxv_min,dxvv)

      write(*,*) 'do you want to add another circular closed boundary?
     +  1 for yes 0 for no'
      read(*,*)   iblock
      write(*,*)  iblock

      enddo !1
          
      npv_temp=kk
      
! check the distance between particles and delete particles too close before writing the file  

      kk=0

      do i=1,npv_temp
      
         iflagv(i)=1
         do j=i+1,npv_temp
            if (iflagv(i).eq.1) then
            distij=sqrt( (xv(i)-xv(j))*(xv(i)-xv(j))+
     +       ( yv(i)-yv(j))*(yv(i)-yv(j) ) )
            
            if (distij.lt.dxv_min*0.5) then
               write(*,*) 'WARNING VIRTUAL PARTICLE', i,
     + 'with coordinate', xv(i), yv(i), ' and V. particle ',j, 
     + 'with coordinate', xv(j), yv(j), ' are too close, particle',i, 
     + 'is removed' 
               iflagv(i)=0
               magn_vec=sqrt( ( 0.5*vnxv(i)+0.5*vnxv(j) )**2+ 
     + ( 0.5*vnyv(i)+0.5*vnyv(j) )**2 )
               vnxv(j)=0.5*(vnxv(i)+vnxv(j))/magn_vec
               vnyv(j)=0.5*(vnyv(i)+vnyv(j))/magn_vec
            endif
            endif
         enddo
         if (iflagv(i).eq.1) kk=kk+1
      enddo
      
      npv=kk
      
! write the ipartv file      

      write(*,*) 'write ipartv'
      open(78,file='ipartv')
      do i=1,npv_temp
         if (iflagv(i).gt.0) then
             write(78,*) xv(i), yv(i), vnxv(i), vnyv(i), dxv(i)
         endif
      enddo
      close (78) 
    
      end subroutine
      
c
c***********************************************************************
c      
c     ____________________ SUBROUTINE slopingBed

      subroutine slopingbed(np_b, xb, yb, hb_SPH, fr, hsm_b, Vol_b, 
     + hsm_b_max)
      
      implicit none
      
      integer np_b, npx, npy, i, j, iblock, kk
      real xb(np_b), yb(np_b), hb_SPH(np_b), fr(np_b), hsm_b(np_b), 
     + Vol_b(np_b), hsm_b_max
      real xbini, ybini, lxb, lyb, dxb, dyb, coef, b_elev, fr_manning
      
      iblock=1
      kk=0
      hsm_b_max=0
      
      do while (iblock.eq.1)
      
         write(*,*) ' initial position of the block, x - coord'
         read(*,*)   xbini
         write(*,*)  xbini
         
         write(*,*) ' initial position of the block, y - coord'
         read(*,*)   ybini
         write(*,*)  ybini
         
         write(*,*) ' length of the block'
         read(*,*)   lxb
         write(*,*)  lxb
         
         write(*,*) ' heigth of the block'
         read(*,*)   lyb
         write(*,*)  lyb
         
         write(*,*) ' bottom particle spacing'
         read(*,*)   dxb
         write(*,*)  dxb
         
         write(*,*) ' coefficient for the smoothing length (sugg. 1.2)'
         read(*,*)   coef
         write(*,*)  coef
         
         write(*,*) ' manning coefficient of the block'
         read(*,*)   fr_manning
         write(*,*)  fr_manning
         
         write(*,*) ' elevation of the block'
         read(*,*)   b_elev
         write(*,*)  b_elev
         
         npx=int(lxb/dxb)+1
         npy=int(lyb/dxb)+1
         
         dxb=lxb/npx
         dyb=lyb/npy
         hsm_b_max=max(hsm_b_max,0.5*(dxb+dyb)*coef)

         do i=1,npx
            do j=1,npy
               kk=kk+1
               xb(kk)=xbini+(i-0.5)*dxb
               yb(kk)=ybini+(j-0.5)*dyb
               hb_SPH(kk)=b_elev
               hsm_b(kk)=coef*0.5*(dxb+dyb)
               fr(kk)=fr_manning
               Vol_b(kk)=dxb*dyb
               
            enddo
         enddo
      
         write(*,*) ' do you want to add another block 1 for yes'
         read(*,*)   iblock
         write(*,*)  iblock
      
      enddo
      
      np_b=kk
      
      return

      end subroutine
      
c
c***********************************************************************
c      
c     ____________________ SUBROUTINE parabolic basin

      subroutine parabolicBed(np_b, xb, yb, hb_SPH, fr, hsm_b, Vol_b, 
     + hsm_b_max)
      
      implicit none
      
      real xb(np_b), yb(np_b), hb_SPH(np_b), fr(np_b), hsm_b(np_b),
     + Vol_b(np_b), hsm_b_max
      
      real a, h0, lxb, lyb, coef, fr_manning, dxb, dyb
      integer np_b, npx, npy, i, j, kk
      
      kk=0
      
         write(*,*) 'Eq. is y=h0*( x**2+y**2 )/a**2, parameter a:'
         read(*,*)   a
         write(*,*)  a
         
         write(*,*) 'Eq. is y=h0*( x**2+y**2 )/a**2, parameter h0:'
         read(*,*)   h0
         write(*,*)  h0
         
         write(*,*) ' length of the domain'
         read(*,*)   lxb
         write(*,*)  lxb
         
         write(*,*) ' heigth of the domain'
         read(*,*)   lyb
         write(*,*)  lyb
         
         write(*,*) ' bottom particle spacing'
         read(*,*)   dxb
         write(*,*)  dxb
         
         write(*,*) ' coefficient for the smoothing length (sugg. 1.2)'
         read(*,*)   coef
         write(*,*)  coef
         
         write(*,*) ' manning coefficient'
         read(*,*)   fr_manning
         write(*,*)  fr_manning
         
         npx=int(lxb/dxb)+1
         npy=int(lyb/dxb)+1
         
         dxb=lxb/npx
         dyb=lyb/npy
         
         hsm_b_max=coef*(dxb+dyb)*0.5

      do i=1,npx+2
          do j=1,npy+2
               kk=kk+1
               xb(kk)=(i-1)*dxb
               yb(kk)=(j-1)*dyb
               hsm_b(kk)=coef*(dxb+dyb)*0.5
               hb_SPH(kk)=h0*( (xb(kk)-lxb*0.5)**2+(yb(kk)-lyb*0.5)**2 )
     +               /a**2
               fr(kk)=fr_manning
               Vol_b(kk)=dxb*dyb
          enddo
      enddo

      np_b=kk

      return

      end subroutine
      
c***********************************************************************
c      
c     ____________________ SUBROUTINE read_bedProfile
      
      subroutine read_bedProfile(file_bedProfile, np_b, xb, yb, hb_SPH, 
     + fr, hsm_b, Vol_b, hsm_b_max)
     

      implicit none
      
      character file_bedProfile*60
      
      !parameter (np_b_max=10000)
      integer np_b, i
      real xb(10000), yb(10000), hb_SPH(10000), fr(10000), 
     +  hsm_b(10000), Vol_b(10000), hsm_b_max   !BDR
      
      hsm_b_max=0.
               
      open(101,file=file_bedProfile,status='old')      
      read(101,*) np_b         
      do i=1,np_b
         read(101,*) xb(i), yb(i), hb_SPH(i), fr(i), hsm_b(i), Vol_b(i)
         hsm_b_max=max(hsm_b(i),hsm_b_max)
      enddo
      
      close(101)

      return

      end subroutine
c
c***********************************************************************
c      
c     _______________SUBROUTINE definition of water surface elevation
      
      subroutine surface_define(coef, npar, np, xp, yp, dw, up, vp, 
     +  h_var, areap)

      implicit none

      integer npar, np, i, j, kk, iblock, np_x, np_y
      real coef, xp(npar), yp(npar), dw(npar), up(npar), vp(npar), 
     +  h_var(npar), areap(npar)

      real xp_block(4), yp_block(4), dw_block(4), up_block(4), 
     + vp_block(4),dx_block, dx, dy, lx, ly, xbini, ybini
      
      
      iblock=1

      np=0 
      kk=0

      do while (iblock.eq.1) 
      
         write(*,*) ' initial position of the block, x - coord'
         read(*,*)   xbini
         write(*,*)  xbini
         
         write(*,*) ' initial position of the block, y - coord'
         read(*,*)   ybini
         write(*,*)  ybini
         
         write(*,*) ' length of the block'
         read(*,*)   lx
         write(*,*)  lx
         
         write(*,*) ' heigth of the block'
         read(*,*)   ly
         write(*,*)  ly

         xp_block(1)=xbini
         yp_block(1)=ybini
         
         xp_block(2)=xbini+lx
         yp_block(2)=ybini
         
         xp_block(3)=xbini
         yp_block(3)=ybini+ly
         
         xp_block(4)=xbini+lx
         yp_block(4)=ybini+ly
         
         
         write(*,*) ' water surface elevation at South - West (m)'
         read(*,*)   dw_block(1)
         write(*,*)  dw_block(1)
         write(*,*) ' water surface elevation at South - East (m)'
         read(*,*)   dw_block(2)
         write(*,*)  dw_block(2)
         write(*,*) ' water surface elevation at North - West (m)'
         read(*,*)   dw_block(3)
         write(*,*)  dw_block(3)
         write(*,*) ' water surface elevation at North - East (m)'
         read(*,*)   dw_block(4)
         write(*,*)  dw_block(4)
         write(*,*) ' x - velocity component at South - West (m)'
         read(*,*)   up_block(1)
         write(*,*)  up_block(1)
         write(*,*) ' x - velocity component at South - East (m)'
         read(*,*)   up_block(2)
         write(*,*)  up_block(2)
         write(*,*) ' x - velocity component at North - West (m)'
         read(*,*)   up_block(3)
         write(*,*)  up_block(3)
         write(*,*) ' x - velocity component at North - East (m)'
         read(*,*)   up_block(4)
         write(*,*)  up_block(4)
         write(*,*) ' y - velocity component at South - West (m)'
         read(*,*)   vp_block(1)
         write(*,*)  vp_block(1)
         write(*,*) ' y - velocity component at South - East (m)'
         read(*,*)   vp_block(2)
         write(*,*)  vp_block(2)
         write(*,*) ' y - velocity component at North - West (m)'
         read(*,*)   vp_block(3)
         write(*,*)  vp_block(3)
         write(*,*) ' y - velocity component at North - East (m)'
         read(*,*)   vp_block(4)
         write(*,*)  vp_block(4)
         write(*,*) ' particle spacing for this block (m)' 
         read(*,*)   dx_block
         write(*,*)  dx_block

         np_x=int( lx/dx_block )+1
         dx = lx/np_x
         np_y=int( ly/dx_block )+1
         dy = ly/np_y

!generation of fluid particles

         do i=1,np_x
            do j=1,np_y
              kk=kk+1
              xp(kk)=xp_block(1)+(i-0.5)*dx
              yp(kk)=yp_block(1)+(j-0.5)*dy
              
              !free surface elevation 
              call interp_bi(xp_block,yp_block,dw_block,xp(kk),yp(kk),
     + dw(kk))
              !x velocity component
              call interp_bi(xp_block,yp_block,up_block,xp(kk),yp(kk),
     + up(kk))
              !y velocity component
              call interp_bi(xp_block,yp_block,vp_block,xp(kk),yp(kk),
     + vp(kk))

              areap(kk)=dx*dy
              h_var(kk)=coef*(dx+dy)*0.5
                           
            enddo  
         enddo

         write(*,*) ' Add another block (1=yes)'
         read(*,*) iblock
         write(*,*) iblock
      enddo
      
      np=kk
      
      end subroutine

c
c***********************************************************************
c      
c     _______________SUBROUTINE definition of water surface elevation for circular domain
      
      subroutine surface_circular(coef, npar, np, xp, yp, dw, up, vp, 
     +  h_var, areap, vlx, vly, dxv_min)

      implicit none

      integer npar, np, i, j, kk, np_x, np_y
      real coef, xp(npar), yp(npar), dw(npar), up(npar), vp(npar), 
     +  h_var(npar), areap(npar), vlx, vly, dxv_min

      real dx_inside, up_inside, vp_inside, dw_inside,
     +  xc, yc, rad, dx_out, dw_out, up_out, vp_out
     
      real dt, radius, dxcor, t, pi, dxlim
      
      real xcand, ycand, radmax
     
      integer ncrf, np_crf, k
      
      pi=4.0*atan(1.0)
      dxlim=dxv_min*0.8

      np=0 
      kk=0

      write(*,*) ' x - coordinate of the centre of the circle (m)'
      read(*,*)   xc
      write(*,*)  xc
         
      write(*,*) ' y - coordinate of the centre of the circle (m)'
      read(*,*)   yc
      write(*,*)  yc
         
      write(*,*) ' radius of the inside circle(m)'
      read(*,*)   rad
      write(*,*)  rad
         
      write(*,*) 'particle distance inside the circle (m)'
      read(*,*)   dx_inside
      write(*,*)  dx_inside
         
      write(*,*) ' water surface elevation inside the circle (m)'
      read(*,*)   dw_inside
      write(*,*)  dw_inside

      write(*,*) ' x - velocity component inside the circle (m)'
      read(*,*)   up_inside
      write(*,*)  up_inside

      write(*,*) ' y - velocity component inside the circle (m)'
      read(*,*)   vp_inside
      write(*,*)  vp_inside
      
      write(*,*) 'particle distance outside the circle (m)'
      read(*,*)   dx_out
      write(*,*)  dx_out

      write(*,*) ' water surface elevation outside the circle (m)'
      write(*,*) ' 0 if you want to simulate the dry case'
      read(*,*)   dw_out
      write(*,*)  dw_out
      
      write(*,*) ' x - velocity component outside the circle (m/s)'
      read(*,*)   up_out
      write(*,*)  up_out

      write(*,*) ' y - velocity component outside the circle (m/s)'
      read(*,*)   vp_out
      write(*,*)  vp_out

      kk=0

      dx_out=sqrt(dw_inside/dw_out)*dx_inside

      ncrf=int(rad/dx_inside)+1 !number of circles
      dx_inside=rad/ncrf !dx for the circles

      do i=1,ncrf !loop over inside circles

           radius=(i-0.5)*dx_inside; !radius for i-th circle
           np_crf=int(0.5*pi*radius/dx_inside)+1; !number of particle in the i-th circle
           dt=0.5*pi/np_crf !d angle
           dxcor=dt*radius !dx
           
           t=0;
           do j=1,np_crf
              t=t+dt;
              do k=1,4
                 kk=kk+1
                 if (k.eq.1) then
                     xp(kk)=xc+radius*cos(t)
                     yp(kk)=yc+radius*sin(t)
                 elseif (k.eq.2) then
                     xp(kk)=xc+radius*cos(t+pi*0.5)
                     yp(kk)=yc+radius*sin(t+pi*0.5)
                 elseif (k.eq.3) then
                     xp(kk)=xc+radius*cos(t+pi)
                     yp(kk)=yc+radius*sin(t+pi)
                 elseif (k.eq.4) then
                     xp(kk)=xc+radius*cos(t+1.5*pi)
                     yp(kk)=yc+radius*sin(t+1.5*pi)
                 endif
                 
                 dw(kk)=dw_inside
                 up(kk)=up_inside
                 vp(kk)=vp_inside
                 h_var(kk)=coef*0.5*(dx_inside+dxcor)
                 areap(kk)=dx_inside*dxcor
                 
              enddo
           enddo

      enddo

      radmax=0.5*(0.5*vlx+0.5*vly)
      ncrf=int( (radmax-rad)/dx_out )+1 !number of circles
      dx_out=(radmax-rad)/ncrf !dx for the circles

      if (dw_out.gt.0) then
      
      do i=1,ncrf !loop over outdside circles

           radius=rad+(i-0.5)*dx_out; !radius for i-th circle
           np_crf=int(0.5*pi*radius/dx_out)+1; !number of particle in the i-th circle
           dt=0.5*pi/np_crf !d angle
           dxcor=dt*radius !dx
           
           t=0;
           do j=1,np_crf
              t=t+dt;
              do k=1,4
                 if (k.eq.1) then
                     xcand=xc+radius*cos(t)
                     ycand=yc+radius*sin(t)
                 elseif (k.eq.2) then
                     xcand=xc+radius*cos(t+pi*0.5)
                     ycand=yc+radius*sin(t+pi*0.5)
                 elseif (k.eq.3) then
                     xcand=xc+radius*cos(t+pi)
                     ycand=yc+radius*sin(t+pi)
                 elseif (k.eq.4) then
                     xcand=xc+radius*cos(t+1.5*pi)
                     ycand=yc+radius*sin(t+1.5*pi)
                 endif
                 
                 if ( xcand.lt.vlx-dxlim .and. xcand.gt.dxlim .and.
     +                ycand.lt.vly-dxlim .and. ycand.gt.dxlim )then
                    kk=kk+1
                    xp(kk)=xcand
                    yp(kk)=ycand
                    dw(kk)=dw_out
                    up(kk)=up_out
                    vp(kk)=vp_out
                    h_var(kk)=coef*0.5*(dx_out+dxcor)
                    areap(kk)=dx_out*dxcor
                 
                 endif
                 
              enddo
           enddo

      enddo
      
      endif
      
      np=kk
      
      end subroutine
      
c
c***********************************************************************
c      
c     _______________SUBROUTINE refim_param

      subroutine refim_param(iref, vlx, vly, dw_min, xmin_ref, ymin_ref, 
     + dxx_ref, dyy_ref, ncx_ref, ncy_ref, area_max)

      implicit none
      
      integer iref, iallo, iblock, ncx_ref, ncy_ref, nct_ref, i, ii, j
      
      real vlx, vly, dw_min, xmin_ref, ymin_ref, dxx_ref, dyy_ref
      real xmin_ref_lim, ymin_ref_lim, xmax_ref_lim, ymax_ref_lim,
     + ref_lim_treshold
      real area_max
      real, allocatable :: ref_lim(:), ref_coa_lim(:)
           
      !grid for refinement
      write(*,*) 'size of the grid for refinement along x direction (m)'
      read(*,*)   dxx_ref
      write(*,*)  dxx_ref
      
      write(*,*) 'size of the grid for refinement along y direction (m)'
      read(*,*)   dyy_ref
      write(*,*)  dyy_ref
      
      write(*,*) 'minimum water depth for splitting (m)'
      read(*,*)   dw_min
      write(*,*)  dw_min
      
      ncx_ref=int(vlx/dxx_ref)+1
      ncy_ref=int(vly/dyy_ref)+1
      nct_ref=ncx_ref*ncy_ref
      xmin_ref=0
      ymin_ref=0
            
      !allocate memory
      allocate(ref_lim(nct_ref), STAT=iallo)
      if (iallo .ne. 0) then
           STOP 'allocation error for ref_lim'
      endif
      allocate(ref_coa_lim(nct_ref), STAT=iallo)
      if (iallo .ne. 0) then
           STOP 'allocation error for ref_coa_lim'
      endif
      

      ref_lim(1:nct_ref) = 1.E18 !it means no refinement 
      ref_coa_lim(1:nct_ref)=dxx_ref*dyy_ref*1.E-2 !it means no coalescing

      if (iref.eq.1) then
         iblock=1
      else
         iblock=0
      endif
      
      do while (iblock.eq.1)
            
         write(*,*) 'x_min_ref_lim (m)'
         read(*,*)   xmin_ref_lim 
         write(*,*)  xmin_ref_lim
      
         write(*,*) 'y_min_ref_lim (m)'
         read(*,*)   ymin_ref_lim 
         write(*,*)   ymin_ref_lim
      
         write(*,*) 'x_max_ref_lim (m)'
         read(*,*)   xmax_ref_lim 
         write(*,*) xmin_ref_lim
      
         write(*,*) 'y_max_ref_lim (m)'
         read(*,*)   ymax_ref_lim 
         write(*,*)   ymax_ref_lim 
         
         write(*,*) 'maximum area allowed before splitting (m**2)'
         read(*,*)   ref_lim_treshold
         write(*,*)  ref_lim_treshold
      
         do i=1,ncx_ref
            do j=1,ncy_ref
              !cell number
              ii= i + (j - 1)*ncx_ref
              if ( (i-0.5)*dxx_ref.lt.xmax_ref_lim.and.
     +          (i-0.5)*dxx_ref.gt.xmin_ref_lim.and.
     +          (j-0.5)*dyy_ref.lt.ymax_ref_lim.and.
     +          (j-0.5)*dyy_ref.gt.ymin_ref_lim) then
                ref_lim(ii)=ref_lim_treshold*area_max
                !ref_coa_lim(ii)=1.E6 !not yet implemented
              endif
           enddo
         enddo
      
         write(*,*) ' Add another block for refinement(1=yes)'
         read(*,*) iblock
         write(*,*) iblock

      end do

      write(*,*) 'write area_lim'
      open(79,file='area_lim')
      do i=1,nct_ref
           write(79,*) ref_lim(i), ref_coa_lim(i)
      enddo
      close (79) 
   
      end subroutine
     
c
c***********************************************************************
c
c     ____________________ SUBROUTINE interp_bi(x,y,xdata,ydata,n)

c
c it perform a bilinear interpolation over four points with coord. xdata, ydata
c

      subroutine interp_bi(xdata,ydata,zdata,x,y,z)
      
      implicit none
      
      real x, y, z, xdata(4), ydata(4), zdata(4)
      real a, b, c, d
      
      a= zdata(1)/( ( xdata(2)-xdata(1))*(ydata(3)-ydata(1)) )
      b= zdata(2)/( ( xdata(2)-xdata(1))*(ydata(3)-ydata(1)) )
      c= zdata(3)/( ( xdata(2)-xdata(1))*(ydata(3)-ydata(1)) )
      d= zdata(4)/( ( xdata(2)-xdata(1))*(ydata(3)-ydata(1)) )
      
      z=a*(xdata(2)-x)*(ydata(3)-y)+b*(x-xdata(1))*(ydata(3)-y)+
     +  c*(xdata(2)-x)*(y-ydata(1))+d*(x-xdata(1))*(y-ydata(1))
     
      return
     
      end subroutine

c
c***********************************************************************
c
c  
c     ____________________ SUBROUTINE TOCOMPILE_UNIXSTYLE


      subroutine tocompile_unixStyle(i_compile_opt,ivisc,idebug,
     &                               idouble,iMUSCL,iref)

      CHARACTER(LEN=10) :: FMT,FMT1
      CHARACTER(LEN=1)  :: TAB
      CHARACTER(LEN=4)  :: OBJFINAL
      CHARACTER(LEN=6)  :: OBJ2
      CHARACTER(LEN=9)  :: obj_string
      TAB=CHAR(9)

      if(i_compile_opt.eq.1)then
        print*,'Using gfortran for unix/linux '
      elseif(i_compile_opt.eq.2)then
        print*,'Using Intel ifort for unix/linux '
      elseif(i_compile_opt.eq.4)then
        print*,'Using SilverFrost FTN95 '
      endif

	 
      FMT="(A)"
      FMT1="(2A)"
      

      
      open(22,file='SPHYSICS.mak')
      
      if(i_compile_opt.eq.1)then
        !- Using gfortran for unix/linux -
        OBJFINAL = '.o'
        OBJ2 = OBJFINAL//' \'
        obj_string = 'objects='
        write(22,FMT) 'FC=gfortran'
        if (idouble.eq.2) then  !Use Double Precision
          if (idebug.eq.1) then
            write(22,FMT) 'OPTIONS= -g -O0 '
     &        //' -fdefault-real-8'
c     &        //' -ffpe-trap=invalid,zero,denormal'
      !- Debugging options, Uncomment as required -
c     &        //' -ftree-vectorize'
c     &        //' -ffast-math -funroll-loops'
c     &        //' -fbounds-check'
c     &        //' -frange-check'
c     &        //' -Wunderflow'
     &        //' -Wuninitialized'
     &        //' -ffpe-trap=invalid,zero,overflow'
     &        //' -fstack-check'
     &        //' -fstack-protector'
c     &        //' -ftrapv'
c     &        //' -ftracer'
c     &        //' -g'
     &        //' -fimplicit-none'
     &        //' -unused'
c     &        //' -fno-automatic'
          else
            write(22,FMT) 'OPTIONS= -O3 -fdefault-real-8'
          endif
        else                    !Use Single Precision
           if (idebug.eq.1) then
            write(22,FMT) 'OPTIONS= -g -O0 '
c    &        //' -ffpe-trap=invalid,zero,denormal'
       !- Debugging options, Uncomment as required -
c     &        //' -ftree-vectorize'
c     &        //' -ffast-math -funroll-loops'
     &        //' -fbounds-check'
     &        //' -frange-check'
     &        //' -Wunderflow'
     &        //' -Wuninitialized'
     &        //' -ffpe-trap=invalid,zero,overflow'
     &        //' -fstack-check'
     &        //' -fstack-protector'
c    &        //' -ftrapv'
c     &        //' -ftracer'
c     &        //' -g'
     &        //' -fimplicit-none'
     &        //' -unused'
c     &        //' -fno-automatic'
         else
            write(22,FMT) 'OPTIONS= -O3'
          endif
        endif

        write(22,FMT) 'srcdir=.'
        write(22,FMT) 'idir=../../execs'
        write(22,FMT) 'bakdir=../../execs.bak'

      elseif(i_compile_opt.eq.2)then

        !- Using Intel IFORT for unix/linux -
        OBJFINAL = '.o'
        OBJ2 = OBJFINAL//' \'
        obj_string = 'objects='
        write(22,FMT) 'FC=ifort'
        if (idouble.eq.2) then  !Use Double Precision
          if (idebug.eq.1) then
            write(22,FMT) 'OPTIONS= -g -O0 -r8 -fpe0'
      !- Debugging options, Uncomment as required -
     &       //' -ipo'
c     &       //' -g -debug all -check all'
c     &       //' -implicitnone'
c     &       //' -warn unused'
c     &       //' -fp-stack-check -heap-arrays'
c     &       //' -ftrapuv'
c     &       //' -check pointers'
c     &       //' -check bounds'
c     &       //' -check uninit'
c     &       //' -fpstkchk'
c     &       //' -traceback'
          else
            write(22,FMT) 'OPTIONS= -03 -r8'
     &       //' -ipo'
          endif
        else                    !Use Single Precision
          if (idebug.eq.1) then
            write(22,FMT) 'OPTIONS= -g -O0 -fpe0'
      !- Debugging options, Uncomment as required -
     &       //' -ipo'
c     &       //' -g -debug all -check all'
c     &       //' -implicitnone'
c     &       //' -warn unused'
c     &       //' -fp-stack-check -heap-arrays'
c     &       //' -ftrapuv'
c     &       //' -check pointers'
c     &       //' -check bounds'
c     &       //' -check uninit'
c     &       //' -fpstkchk'
c     &       //' -traceback'
          else
            write(22,FMT) 'OPTIONS= -fast'
          endif
        endif
        write(22,FMT) 'srcdir=.'
        write(22,FMT) 'idir=../../execs'
        write(22,FMT) 'bakdir=../../execs.bak'
      elseif(i_compile_opt.eq.4)then
        !- Using SilverFrost FTN95 -
        OBJFINAL = '.obj'
        OBJ2 = OBJFINAL//' \'
        obj_string = 'OBJFILES='
        if (idouble.eq.2) then  !Use Double Precision
          if (idebug.eq.1) then
            write(22,FMT) 'OPTIONS= /CHECK /DREAL'
          else
            write(22,FMT) 'OPTIONS= /OPTIMISE /DREAL'
          endif
        else                    !Use Single Precision
          if (idebug.eq.1) then
            write(22,FMT) 'OPTIONS= /CHECK'
          else
            write(22,FMT) 'OPTIONS= /OPTIMISE'
          endif
        endif
      endif
            
      write(22,FMT)
      write(22,FMT) obj_string//' global_2D'//OBJ2
      write(22,FMT1)TAB,'ini_divide_2D'//OBJ2
      write(22,FMT1)TAB,'SPHYSICS_SWE_2D'//OBJ2
      write(22,FMT1)TAB,'getdata_2D'//OBJ2
      write(22,FMT1)TAB,'check_limits_2D'//OBJ2
      write(22,FMT1)TAB,'poute_2D'//OBJ2
      write(22,FMT1)TAB,'poute_grid_2D'//OBJ2
      write(22,FMT1)TAB,'divide_2D'//OBJ2
      write(22,FMT1)TAB,'divide_vir_2D'//OBJ2
      write(22,FMT1)TAB,'variable_time_step_2D'//OBJ2
      write(22,FMT1)TAB,'ac_2D'//OBJ2
      if (iref.eq.0) then
          write(22,FMT1)TAB,'ac_dw_var_2D'//OBJ2
          write(22,FMT1)TAB,'celij_dw_2D'//OBJ2
          write(22,FMT1)TAB,'self_dw_2D'//OBJ2
      elseif (iref.eq.1) then
          write(22,FMT1)TAB,'ac_dw_var_hj_2D'//OBJ2
          write(22,FMT1)TAB,'celij_dw_hj_2D'//OBJ2
          write(22,FMT1)TAB,'self_dw_hj_2D'//OBJ2
          write(22,FMT1)TAB,'loop_2D'//OBJ2
      else
          write(*,*) 'ERROR: iref should be 0 or 1'
          write(*,*) 'ivisc=', ivisc
          STOP
      endif
      write(22,FMT1)TAB,'celij_dw_vir_2D'//OBJ2
      write(22,FMT1)TAB,'celij_dw_ob_2D'//OBJ2
      write(22,FMT1)TAB,'kernel_cubic_2D'//OBJ2
      write(22,FMT1)TAB,'step_leap_frog_2D'//OBJ2
      write(22,FMT1)TAB,'ac_alpha_2D'//OBJ2
      write(22,FMT1)TAB,'celij_alpha_2D'//OBJ2
      write(22,FMT1)TAB,'self_alpha_2D'//OBJ2
      write(22,FMT1)TAB,'celij_alpha_vir_2D'//OBJ2
      write(22,FMT1)TAB,'grid_h_var_2D'//OBJ2
      
      if (ivisc.eq.3) then
         write(22,FMT1)TAB,'celij_balsara_2D'//OBJ2
         write(22,FMT1)TAB,'self_balsara_2D'//OBJ2
         write(22,FMT1)TAB,'celij_balsara_ob_2D'//OBJ2
         write(22,FMT1)TAB,'celij_vir_balsara_2D'//OBJ2
         write(22,FMT1)TAB,'riemann_balsara'//OBJ2
      elseif (ivisc.eq.2) then
         write(22,FMT1)TAB,'celij_visc_2D'//OBJ2
         write(22,FMT1)TAB,'self_visc_2D'//OBJ2
         write(22,FMT1)TAB,'celij_ob_2D'//OBJ2
         write(22,FMT1)TAB,'celij_vir_2D'//OBJ2
         write(22,FMT1)TAB,'viscosity_LF_2D'//OBJ2
      elseif (ivisc.eq.1) then
         write(22,FMT1)TAB,'celij_visc_2D'//OBJ2
         write(22,FMT1)TAB,'self_visc_2D'//OBJ2
         write(22,FMT1)TAB,'celij_ob_2D'//OBJ2
         write(22,FMT1)TAB,'celij_vir_2D'//OBJ2
         write(22,FMT1)TAB,'viscosity_artificial_2D'//OBJ2
      else
         write(*,*) 'ERROR: ivisc should be 1, 2 or 3'
         write(*,*) 'ivisc=', ivisc
         STOP
      endif
      
      
      write(22,FMT1)TAB,'divide_b_2D'//OBJ2
      write(22,FMT1)TAB,'bottom_2D'//OBJ2
      write(22,FMT1)TAB,'celij_hb_2D'//OBJ2
      write(22,FMT1)TAB,'source_slope_2D'//OBJ2
      write(22,FMT1)TAB,'ac_b_2D'//OBJ2
      write(22,FMT1)TAB,'celij_b_2D'//OBJ2
      write(22,FMT1)TAB,'celij_b_c_2D'//OBJ2


      if (iMUSCL.eq.1) then
          write(22,FMT1)TAB,'limiter_minmod'//OBJ2
      elseif (iMUSCL.eq.0) then
          write(22,FMT1)TAB,'limiter_noMUSCL'//OBJ2
      else
           write(*,*) 'ERROR: iMUSCL should be 0 or 1'
           write(*,*) 'iMUSCL=', iMUSCL
           STOP
      endif

      write(22,FMT1)TAB,'refinement_2D'//OBJ2
      write(22,FMT1)TAB,'refinement_v_2D'//OBJ2

      write(22,FMT1)TAB,'open_bc_2D'//OBJ2
      write(22,FMT1)TAB,'interp_openbc_2D'//OBJ2
      write(22,FMT1)TAB,'recount_2D'//OBJ2
      write(22,FMT1)TAB,'celij_alpha_ob_2D'//OBJ2
      write(22,FMT1)TAB,'divide_ob_2D'//OBJ2
      write(22,FMT1)TAB,'ac_corr_2D'//OBJ2
      write(22,FMT1)TAB,'celij_corr_2D'//OBJ2
      write(22,FMT1)TAB,'self_corr_2D'//OBJ2
      write(22,FMT1)TAB,'open_bc_pos_2D'//OBJFINAL

      write(22,FMT)      
      
      if(i_compile_opt.eq.4)then
        write(22,FMT)'SPHYSICS_SWE_2D.exe: $(OBJFILES)'

        write(22,FMT)      
        write(22,FMT)'clean:'
        write(22,FMT1)TAB,'del *.obj'
      else
        write(22,FMT)'#'      
        write(22,FMT)'SPHYSICS_SWE_2D: $(objects)'
        write(22,FMT1)TAB,
     &               '$(FC) $(OPTIONS) -o SPHYSICS_SWE_2D $(objects)'
        write(22,FMT)'#'
        write(22,FMT1)TAB,'if [ -d $(bakdir) ]; then \'
        write(22,FMT1)TAB,'echo "execs.bak Directory Exists"; else \'
        write(22,FMT1)TAB,'mkdir $(bakdir); \'
        write(22,FMT1)TAB,'echo "execs.bak Directory Created"; \'
        write(22,FMT1)TAB,'fi'
        write(22,FMT)'#'
        write(22,FMT1)TAB,'if [ -d $(idir) ]; then \'
        write(22,FMT1)TAB,'echo "execs Directory Exists"; else \'
        write(22,FMT1)TAB,'mkdir $(idir); \'
        write(22,FMT1)TAB,'echo "execs Directory Created"; \'
        write(22,FMT1)TAB,'fi'
        write(22,FMT)'#' 
        write(22,FMT1)TAB,'-if [ -f $(idir)/SPHYSICS_SWE_2D ]; then \'
        write(22,FMT1)TAB,'mv -f $(idir)/SPHYSICS_SWE_2D $(bakdir)/; \'
        write(22,FMT1)TAB,'echo Old SPHYSICS_SWE_2D moved to execs.bak'
     &  //' from execs; \'
        write(22,FMT1)TAB,'fi'
        write(22,FMT)'#'
        write(22,FMT1)TAB,'mv SPHYSICS_SWE_2D $(idir)'
        write(22,FMT1)TAB,'echo New SPHYSICS_SWE_2D moved to execs'
        write(22,FMT)'#'
        write(22,FMT)'clean:'
        write(22,FMT1)TAB,'rm *.o'
        write(22,FMT1)TAB,'rm *~' 
        write(22,FMT)'#'
        write(22,FMT)'%.o: %.f'
        write(22,FMT1)TAB,'$(FC) $(OPTIONS) -c -o $@ $<'
      endif
      
      close(22)


      end subroutine

c
c***********************************************************************
c
c  
c     ____________________ SUBROUTINE TOCOMPILE_WIN_IFORT

      subroutine tocompile_win_ifort(ivisc,idebug,idouble,iMUSCL)

      CHARACTER(LEN=10) :: FMT,FMT1
      CHARACTER(LEN=1)  :: TAB
      CHARACTER(LEN=4)  :: OBJFINAL
      CHARACTER(LEN=6)  :: OBJ2
      CHARACTER(LEN=9)  :: obj_string

      TAB=CHAR(9)

      print*,'Using Intel Fortran for Windows'

      FMT="(A)"
      FMT1="(2A)"
      open(22,file='SPHYSICS.mak')

      OBJFINAL = '.obj'
      OBJ2 = OBJFINAL//' \'
      obj_string = 'OBJFILES='

      if (idouble.eq.2) then

      if (idebug.eq.1) then
        write(22,FMT) 'OPTIONS= /NOLOGO'
        write(22,FMT) 'COPTIONS= /03'
      else
        write(22,FMT) 'OPTIONS= /NOLOGO'
        write(22,FMT) 'COPTIONS= /03'
      endif

      else

      if (idebug.eq.1) then
        write(22,FMT) 'OPTIONS= /NOLOGO'
        write(22,FMT) 'COPTIONS= /03'
      else
        write(22,FMT) 'OPTIONS= /NOLOGO'
        write(22,FMT) 'COPTIONS= /03'
      endif

      endif

      write(22,FMT)
      write(22,FMT) obj_string//' global_2D'//OBJ2
      write(22,FMT1)TAB,'ini_divide_2D'//OBJ2
      write(22,FMT1)TAB,'SPHYSICS_SWE_2D'//OBJ2
      write(22,FMT1)TAB,'getdata_2D'//OBJ2
      write(22,FMT1)TAB,'check_limits_2D'//OBJ2
      write(22,FMT1)TAB,'poute_2D'//OBJ2
      write(22,FMT1)TAB,'poute_grid_2D'//OBJ2
      write(22,FMT1)TAB,'divide_2D'//OBJ2
      write(22,FMT1)TAB,'divide_vir_2D'//OBJ2
      write(22,FMT1)TAB,'variable_time_step_2D'//OBJ2
      write(22,FMT1)TAB,'ac_2D'//OBJ2
      if (iref.eq.0) then
          write(22,FMT1)TAB,'ac_dw_var_2D'//OBJ2
          write(22,FMT1)TAB,'celij_dw_2D'//OBJ2
          write(22,FMT1)TAB,'self_dw_2D'//OBJ2
      elseif (iref.eq.1) then
          write(22,FMT1)TAB,'ac_dw_var_hj_2D'//OBJ2
          write(22,FMT1)TAB,'celij_dw_hj_2D'//OBJ2
          write(22,FMT1)TAB,'self_dw_hj_2D'//OBJ2
          write(22,FMT1)TAB,'loop_2D'//OBJ2
      else
          write(*,*) 'ERROR: iref should be 0 or 1'
          write(*,*) 'ivisc=', ivisc
          STOP
      endif
      write(22,FMT1)TAB,'celij_dw_vir_2D'//OBJ2
      write(22,FMT1)TAB,'celij_dw_ob_2D'//OBJ2
      write(22,FMT1)TAB,'kernel_cubic_2D'//OBJ2
      write(22,FMT1)TAB,'step_leap_frog_2D'//OBJ2
      write(22,FMT1)TAB,'ac_alpha_2D'//OBJ2
      write(22,FMT1)TAB,'celij_alpha_2D'//OBJ2
      write(22,FMT1)TAB,'self_alpha_2D'//OBJ2
      write(22,FMT1)TAB,'celij_alpha_vir_2D'//OBJ2
      write(22,FMT1)TAB,'grid_h_var_2D'//OBJ2
      
      if (ivisc.eq.3) then
         write(22,FMT1)TAB,'celij_balsara_2D'//OBJ2
         write(22,FMT1)TAB,'self_balsara_2D'//OBJ2
         write(22,FMT1)TAB,'celij_balsara_ob_2D'//OBJ2
         write(22,FMT1)TAB,'celij_vir_balsara_2D'//OBJ2
         write(22,FMT1)TAB,'riemann_balsara'//OBJ2
      elseif (ivisc.eq.2) then
         write(22,FMT1)TAB,'celij_visc_2D'//OBJ2
         write(22,FMT1)TAB,'self_visc_2D'//OBJ2
         write(22,FMT1)TAB,'celij_ob_2D'//OBJ2
         write(22,FMT1)TAB,'celij_vir_2D'//OBJ2
         write(22,FMT1)TAB,'viscosity_LF_2D'//OBJ2
      elseif (ivisc.eq.1) then
         write(22,FMT1)TAB,'celij_visc_2D'//OBJ2
         write(22,FMT1)TAB,'self_visc_2D'//OBJ2
         write(22,FMT1)TAB,'celij_ob_2D'//OBJ2
         write(22,FMT1)TAB,'celij_vir_2D'//OBJ2
         write(22,FMT1)TAB,'viscosity_artificial_2D'//OBJ2
      else
         write(*,*) 'ERROR: ivisc should be 1, 2 or 3'
         write(*,*) 'ivisc=', ivisc
         STOP
      endif
      
      
      write(22,FMT1)TAB,'divide_b_2D'//OBJ2
      write(22,FMT1)TAB,'bottom_2D'//OBJ2
      write(22,FMT1)TAB,'celij_hb_2D'//OBJ2
      write(22,FMT1)TAB,'source_slope_2D'//OBJ2
      write(22,FMT1)TAB,'ac_b_2D'//OBJ2
      write(22,FMT1)TAB,'celij_b_2D'//OBJ2
      write(22,FMT1)TAB,'celij_b_c_2D'//OBJ2


      if (iMUSCL.eq.1) then
          write(22,FMT1)TAB,'limiter_minmod'//OBJ2
      elseif (iMUSCL.eq.0) then
          write(22,FMT1)TAB,'limiter_noMUSCL'//OBJ2
      else
           write(*,*) 'ERROR: iMUSCL should be 0 or 1'
           write(*,*) 'iMUSCL=', iMUSCL
           STOP
      endif

      write(22,FMT1)TAB,'refinement_2D'//OBJ2
      write(22,FMT1)TAB,'refinement_v_2D'//OBJ2

      write(22,FMT1)TAB,'open_bc_2D'//OBJ2
      write(22,FMT1)TAB,'interp_openbc_2D'//OBJ2
      write(22,FMT1)TAB,'recount_2D'//OBJ2
      write(22,FMT1)TAB,'celij_alpha_ob_2D'//OBJ2
      write(22,FMT1)TAB,'divide_ob_2D'//OBJ2
      write(22,FMT1)TAB,'ac_corr_2D'//OBJ2
      write(22,FMT1)TAB,'celij_corr_2D'//OBJ2
      write(22,FMT1)TAB,'self_corr_2D'//OBJ2
      write(22,FMT1)TAB,'open_bc_pos_2D'//OBJFINAL

      write(22,FMT)'.f.obj:'
      write(22,FMT1)TAB,'ifort $(OPTIONS) $(COPTIONS) /O3 /c $<'
      write(22,FMT)      
      write(22,FMT)'SPHYSICS_2D.exe: $(OBJFILES)'
      write(22,FMT1)TAB,'xilink /OUT:$@ $(OPTIONS) $(OBJFILES)'

      write(22,FMT)      
      write(22,FMT)'clean:'
      write(22,FMT1)TAB,'del *.mod *.obj' !arno
       
      close(22)


	return
	end
c***********************************************************************


 

