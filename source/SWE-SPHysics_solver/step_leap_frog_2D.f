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

ccccccccccccccccccccccccccccccc
c	Leap-frog Method      c
ccccccccccccccccccccccccccccccc

      subroutine step
      use global_2D
      
      integer i, iref

c
c  ---  time step ---
c
      if (ivar_dt.eq.1) then
	    call variable_time_step
	    dt=dt_new
	    !to plot result at precise timesteps
	    if (grab_P+dt.gt.out) then
	        dt=out-grab_p
	    endif
            if (itime.eq.0) then
               dt_old=dt
            endif
      endif

c
c  ---  accelerations ---
c
      call ac !internal forces
      call source_slope !source terms
      
c
c  --- update open boundaries condition in time
c
      call interp_openbc 
    	 
c
c  ---  velocities n+1/2,velocities v n+1 ---
c
      do i=1,np
            
         uo(i) = uo(i) + udot(i)*(dt + dt_old)*0.5 !vel n+1/2
         vo(i) = vo(i) + vdot(i)*(dt + dt_old)*0.5 !vel n+1/2
         
         up(i) = uo(i) + 0.5*udot(i)*dt !vel n+1
         vp(i) = vo(i) + 0.5*vdot(i)*dt !vel n+1

      enddo
      
c --- vel and dw for open boundary particles ---      
      
      call open_bc(1,np_ob) 
      call open_bc_pos 

c
c  ---  particle positions time n+1 ---
c
      
      do i=1,np
         
         xp(i) = xp(i) + uo(i)*dt  !position n+1
         yp(i) = yp(i) + vo(i)*dt  

      enddo
c
c --- particle refinement	
c		
       iref=0
c       write(*,*) area_max, area_lim_min, ref_h
       if ((area_max.gt.area_lim_min).and.(ref_h.gt.0)) then     
       
             !this is useflul for interpolated velocities
             npold=np

             do i=1,npold
                pmold(i)=pm(i) 
             end do
                      
             call refinement
             iref=1
       endif 
       
c      if ((mod(itime,1).eq.0).and.(ref_h.gt.0)) then      
c           call coalescing
c       endif
    
        call ini_divide(2) !< added for openbc condition
        call divide(1,np,2) 
	call ini_divide(1)
        call divide_vir(1,npv,1)	
	call ini_divide(3)
	call divide_ob(1,np_ob,3) 

        call check_limits 
            
c      
c --- open boundaries: it re-numbering the particles
c 
        if (mod(itime,100).eq.0) then      
           call recount	
           write(*,*) 'recount'

           call ini_divide(2) !< added for openbc condition
           call divide(1,np,2) 
	   call ini_divide(1)
           call divide_vir(1,npv,1)	
	   call ini_divide(3)
	   call divide_ob(1,np_ob,3) 
	endif
            		
c
c  ---  bottom elevation for all particles ---
c	
        if (np_b.gt.0) then 
            call bottom
        endif
     
c
c  ---  update h_var, dw, rhop, alhphap, cs ---
c 
      n_assign=0
       
      if (np.gt.0) call ac_dw !computing rhop, dw, h_var, alphap                    
       
       !call open_bc again to update dw
       call open_bc(1,np_ob)       
       
       !define velocity for refined particles
       if (iref.eq.1) then  
            call refinement_v
       endif
 
       write(*,*) 'np=',np
       dt_old=dt
          
      if (idebug.eq.1) then
      
          open(23,file='tempris.txt')
          do i=1,np
             if (iflag(i).eq.1) then
             write(23,100) xp(i),yp(i),up(i),vp(i),dw(i)+h_t(i),
     +    h_t(i),udot(i),ax(i),t_stx(i),vdot(i),ay(i),t_sty(i),h_var(i),
     +    pm(i) 
             endif
          enddo
          close(23)
      
          open(23,file='tempris_ob.txt')
          do i=1,np_ob
          if (iflag_ob(i).gt.0) then
        write(23,101) iflag_ob(i),xp_ob(i),yp_ob(i),up_ob(i),vp_ob(i),
     + dw_ob(i),rhop_ob(i),h_var_ob(i), pm_ob(i)
          endif
          enddo
          close(23)
      
      endif
      
100   format(18e16.8)
101   format(i6,8e16.8)

      end

cccccccccccccccccccccccccccccccc
c end of Leap-frog Method      c
cccccccccccccccccccccccccccccccc

