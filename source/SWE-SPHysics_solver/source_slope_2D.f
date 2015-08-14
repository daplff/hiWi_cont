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

c --- PURPOSE ---
c     calculating the slope and the friction source terms 

      subroutine source_slope
      
      use global_2D
      
      integer i 
      real tn1, tn2, dn, term2, fric_x, fric_y, dw_tresh, vmag
      
      real dotkv(2)
      
      call ac_b !computing dh_SPH and ddh_SPH  
      
      do i=1,np
         if (iflag(i).eq.1) then
   
c --- SPH evaluation of slope source term    
                 
          dotkv(1)=ddH_SPH_xx(i)*up(i)+ddH_SPH_xy(i)*vp(i)
          dotkv(2)=ddH_SPH_xy(i)*up(i)+ddH_SPH_yy(i)*vp(i)
          
          tn1= dotkv(1)*up(i)+dotkv(2)*vp(i)
          
          tn2=+ax(i)*dH_SPH_x(i)+ay(i)*dH_SPH_y(i)
         
          dn= 1 + dH_SPH_x(i)*dH_SPH_x(i)+dH_SPH_y(i)*dH_SPH_y(i)
         
          t_stx(i)=( (grav+tn1+tn2) /dn )*dH_SPH_x(i)                                 
          t_sty(i)=( (grav+tn1+tn2) /dn )*dH_SPH_y(i)     

C          t_stx(i)=grav*dH_SPH_x(i)                                 
C          t_sty(i)=grav*dH_SPH_y(i)                      

c --- evaluation of friction term
          vmag=sqrt(up(i)*up(i)+vp(i)*vp(i))
          dw_tresh=max(dw(i),dw_min_fric)

          term2=grav*fr_SPH(i)*fr_SPH(i)*(vmag/(dw_tresh)**1.33)
          fric_x=up(i)*term2
          fric_y=vp(i)*term2
          
          udot(i)=ax(i)-t_stx(i)-fric_x
          vdot(i)=ay(i)-t_sty(i)-fric_y
          
          endif
       enddo
          
100     format(i6,6e16.6)    
      end
