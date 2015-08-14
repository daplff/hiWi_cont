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

      subroutine celij_ob(i,j1,j2)
c
      use global_2D
      
      integer i, j1, j2, j, jj, kind
      real drx, dry, rr2, dux, duy, rhobar, betai, p_v, rad, dot2, 
     + betaj

      real p_ast,rdw_l,rdw_r,r_vel_l_n,r_vel_r_n, rcs_l,rcs_r,
     + rup_l, rup_r, rvp_l, rvp_r
	 
        if(nc(j2,3).ne.0) then !1
         
          do jj=1,nc(j2,3) !2
           j = ibox(j2,3,jj)

            drx = xp(i) - xp_ob(j)
            dry = yp(i) - yp_ob(j)
            rr2 = drx*drx + dry*dry
            dux = up(i) - up_ob(j)
            duy = vp(i) - vp_ob(j)

	    if( rr2.lt. 4*h_var(i)*h_var(i) ) then !5

c ---  reconstruction of water depth at interface

              rdw_l=dw_ob(j)              
              rdw_r=dw(i)

c ---  reconstruction of velocities at interface  
                         
              rup_l=up_ob(j)
              rvp_l=vp_ob(j)
           
              rup_r=up(i)
              rvp_r=vp(i)

	      rad=sqrt(rr2)
             
              r_vel_l_n=( rup_l*drx+rvp_l*dry )/ rad
              r_vel_r_n=( rup_r*drx+rvp_r*dry )/ rad

              rcs_l=sqrt(grav*rdw_l)
              rcs_r=sqrt(grav*rdw_r)  

              call balsara(p_ast,rdw_l,rdw_r,r_vel_l_n,r_vel_r_n,
     + rcs_l,rcs_r)
           
            endif !5

! --- frx i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) ) !3
     + .and.rr2.gt.1.e-18) then 
          
               betai=rhop(i)*dm/alphap(i)
               p_v=p_ast*betai/( rhop(i)*rhop(i) )
                             
	       call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab) 
     
               ax(i)= ax(i) - pm_ob(j)*p_v*frx 
               ay(i)= ay(i) - pm_ob(j)*p_v*fry 
               
               !density acc
               dot2 = + (dux*frx + duy*fry)
               ar(i) = ar(i) + (pm_ob(j)/(dw_ob(j)*rho0))*dot2 !density acc
        
	      endif !3
	      
! --- frx j-particle ---
            if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) )
     + .and.rr2.gt.1.e-18) then !5 
     
               betaj=rhop_ob(j)*dm/alphap_ob(j)
               p_v=p_ast*betaj/( rhop_ob(j)*rhop_ob(j) )
     
	       call kernel(alphad, const_ker,-drx,-dry,rad,h_var(i),
     +     frx,fry,fac,Wab) 
               ax(i)= ax(i) + pm_ob(j)*p_v*frx 
               ay(i)= ay(i) + pm_ob(j)*p_v*fry
               
            endif   	      
	      
         enddo   !2
	endif !1

      end
