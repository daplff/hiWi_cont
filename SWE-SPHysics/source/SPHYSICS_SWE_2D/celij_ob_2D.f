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
      real drx, dry, rr2, dux, duy, dot, rhobar, one_over_rhobar, cbar, 
     + hbar, betai, p_v, rad, dot2, betaj, pi_visc
	 
        if(nc(j2,3).ne.0) then !1
         
          do jj=1,nc(j2,3) !2
           j = ibox(j2,3,jj)

            drx = xp(i) - xp_ob(j)
            dry = yp(i) - yp_ob(j)
            rr2 = drx*drx + dry*dry
            dux = up(i) - up_ob(j)
            duy = vp(i) - vp_ob(j)
            dot = drx*dux + dry*duy
            rhobar  = 0.5*rho0*( dw(i) + dw_ob(j) )
            one_over_rhobar = 1/rhobar
            cbar= 0.5*( cs(i)+cs_ob(j) )
            hbar = 0.5*( h_var(i)+h_var_ob(j) )
            
! --- frx i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) ) !3
     + .and.rr2.gt.1.e-18) then 
     
     
               betai=rhop(i)*dm/alphap(i)
               p_v=betai*0.5*grav/rho0
               
               call viscosity(pi_visc,dot,drx,dry,dux,duy,kind,rr2,
     +                  cbar,rhobar,one_over_rhobar,hbar)
               
	       rad=sqrt(rr2)
	       call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab) 
     
               ax(i)= ax(i) - pm_ob(j)*(p_v+0.5*pi_visc)*frx 
               ay(i)= ay(i) - pm_ob(j)*(p_v+0.5*pi_visc)*fry 
               
               !density acc
               dot2 = + (dux*frx + duy*fry)
               ar(i) = ar(i) + (pm_ob(j)/(dw_ob(j)*rho0))*dot2 !density acc

             !just for debug
c               if (i.eq.1) then
c                 write(*,*) 'ob', i, j, rad, frx, pm_ob(j), 
c     + pm_ob(j)*(p_v+0.5*pi_visc)*frx , ax(i), ay(i)
c              endif
        
	      endif !3
	      
! --- frx j-particle ---
            if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) )
     + .and.rr2.gt.1.e-18) then !5 
     
               betaj=rhop_ob(j)*dm/alphap_ob(j)
               p_v=betaj*0.5*grav/rho0
               
               call viscosity(pi_visc,dot,drx,dry,dux,duy,kind,rr2,
     +                  cbar,rhobar,one_over_rhobar,hbar)

     
     	       rad=sqrt(rr2)
	       call kernel(alphad, const_ker,-drx,-dry,rad,h_var(i),
     +     frx,fry,fac,Wab) 
               ax(i)= ax(i) + pm_ob(j)*(p_v+0.5*pi_visc)*frx 
               ay(i)= ay(i) + pm_ob(j)*(p_v+0.5*pi_visc)*fry

             !just for debug
c               if (i.eq.1) then
c                 write(*,*) 'ob', i, j, rad, frx, pm_ob(j), 
c     + pm_ob(j)*(p_v+0.5*pi_visc)*frx , ax(i), ay(i)
c               endif
               
            endif   	      
	      
         enddo   !2
	endif !1

      end
