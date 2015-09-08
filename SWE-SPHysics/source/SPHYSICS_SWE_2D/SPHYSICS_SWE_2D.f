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
       
      program sph
       
      use global_2D
       
      character supp*4,name*40, name2*40, name3*40, name4*40
      
      integer sumth !EDITED
      integer i
      real dummy
              
       REAL time_begin, time_end
       CALL CPU_TIME (time_begin)
       
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
c
c  ...  file specifications
c
        
c
c  ...  constants initialization
c 
c      g=9.81
c      grz=-g
      grav=9.81     
      pi=4.*atan(1.)
      dm=2 !number of dimension (1 in 1d 2 in 2d)
      sumth=500 !EDITED
      call getdata
      
      !call dosomethingwithfortran(xp,np) !EDITED
	
      open(18,file='matlabin')

      write(18,*) np
      write(18,*) vlx
      write(18,*) vly
      write(18,*) out

      close(18)
      
      ! link - list of bottom particles       
       if (np_b.gt.0) then
          call divide_b          
          call bottom !bottom interpolation 
      endif
      
      ! RV 16/05/10 dw is the water elevation it is transformed in the actual water depth just here
      do i=1,np
         dw(i)=dw(i)-h_t(i)
c         write(*,*) dw(i), h_t(i)
         if (dw(i).lt.0) then
c             write(*,*) 'negative water depth is calculated at part. # '
c     + ,i, 'check the initial condition in the SPHYSICS_SWE_gen'
            iflag(i)=0
            dw(i)=0.
         end if
	 cs(i)=sqrt(dw(i)*grav)
	 pm(i)=rho0*dw(i)*areap(i)
         pm0(i)=pm(i)
         rhop(i)=rho0*dw(i)
         dw0(i)=dw(i)         
         uo(i)=up(i)
         vo(i)=vp(i)
         
      enddo
       
      !call dosomethingwithfortran(xp,np) !EDITED
      call check_limits	
      call ini_divide(2) !zeroing nc and allocate ibox
      call divide(1,np,2) 
      call ini_divide(3) !zeroing nc
      call divide_ob(1,np_ob,3)
      call ini_divide(1)
      call divide_vir(1,npv,1)
 
c
c  --- write initial conditions in file PART_0001
c  
      if (i_restartRun.eq.0) then
         open(19,file='DT',status='replace') 
         write(19,*) 'time dt_new'
	  write(19,*) time, dt
         name='PART_0001'
         name2='GRD_d0001'
         name3='GRD_u0001'
         name4='GRD_v0001'
         write(*,*) name
         open(23,file=name,status='replace')
         open(24,file=name2,status='replace')
         open(25,file=name3,status='replace')
         open(26,file=name4,status='replace')
         call interp_openbc 
         call open_bc(1,np_ob) 
         call poute(22) 
         call poute_grid(24)
         close(23)
         close(24)
         close(25)
         close(26)
      else
         open(19,file='DT',status='old',position='append')
      endif
      close(19)
      
      call interp_openbc 
      call open_bc(1,np_ob)
      call ac_dw !computing rhop, dw, h_var, alphap
             
c
c  .............................. MAIN LOOP ...................................
c

       grab_P=0
              
       do while (time.lt.tmax)
         	         
         !-- Call Main Subroutines --  
         call step
                   
         !-- File Output Routines --
         if(grab_P.ge.out) then 
       
           if (time.ge.trec_ini) then
              ngrab=ngrab+1
              write(*,*) ngrab
              write(supp,'(i4.4)') ngrab
              name='PART_'//supp
              name2='GRD_d'//supp
              name3='GRD_u'//supp
              name4='GRD_v'//supp
              write(*,*) name
              open(23,file=name,status='replace')
           
              open(24,file=name2,status='replace')
              open(25,file=name3,status='replace')
              open(26,file=name4,status='replace')
    
              call poute(22)
              call poute_grid(24)
              !call dosomethingwithfortran(xp,sumth) !EDITED
              close(23)
              close(24)
              close(25)
              close(26)
           
              open(44,file='restart',status='replace')
              write(44,100) itime,time,ngrab, np
	      close(44)
	      
	   endif
       
           grab_P=0.
       
         endif
            
         if(itime.eq.itime*10/10)then
           write(*,*) 'time, dt hmax ',time,dt, hmax, '  ok'     
         endif
         itime = itime+1
         time = time + dt 
         grab_P=grab_P+dt
         

       enddo
                                  
       write(*,*) 'End '
       !call dosomethingwithfortran(xp,np) !EDITED
       CALL CPU_TIME ( time_end )
       write(*,*)'time_begin',time_begin,'seconds'
       write(*,*)'time_end',time_end,'seconds'
       write(*,*)'Time of operation was',time_end - time_begin,'seconds'
       
100   format(i8,e16.8,i8, i8, i8)
       
       end       !end main program
       
