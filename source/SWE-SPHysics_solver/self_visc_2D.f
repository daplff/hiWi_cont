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

      subroutine self(j1)
c
      use global_2D
      
      real fi(2), r_lim(2), drx, dry, rr2, rad, dux, duy, rhobar, 
     + one_over_rhobar, abs_vr, abs_vl, cbar, hbar, dot, betai, betaj, 
     + p_v, dot2, pi_visc, rup_l, rvp_l, rup_r, rvp_r
      integer j1, j2, ii, i, j, jj, jj_start, kind

      jj_start = 1

      do ii=1,nc(j1,2) !2
         i = ibox(j1,2,ii)

         jj_start = jj_start + 1          

         do jj=jj_start,nc(j1,2) !3
            j = ibox(j1,2,jj)

             drx = xp(i) - xp(j)
             dry = yp(i) - yp(j)
             rr2 = drx*drx + dry*dry
             dux = up(i) - up(j)
             duy = vp(i) - vp(j)

            rhobar  = 0.5*rho0*( dw(i) + dw(j) )
            one_over_rhobar = 1/rhobar
	    cbar = 0.5*( cs(i)   + cs(j)   ) 
	    hbar=0.5*( h_var(i)+h_var(j) )
	    
c ---  reconstruction of velocities at interface  
	    if( (rr2.lt.max( 4*h_var(i)*h_var(i),4*h_var(j)*h_var(j)) )
     +              .and.rr2.gt.1.e-18)  then !5
                         
            if (abs(dux).gt.0.00001) then         
                r_lim(1)=( grad_up(j,1)*drx+grad_up(j,2)*dry )
     +           /( dux )
            else
                r_lim(1)=10
            endif
            if (abs(duy).gt.0.00001) then         
                r_lim(2)=( grad_vp(j,1)*drx+grad_vp(j,2)*dry )
     +           /( duy )
            else
                r_lim(2)=10
            endif
            call limiter(r_lim,fi)
            rup_l=up(j)+0.5*fi(1)*( dux ) !velocities on the left side
            rvp_l=vp(j)+0.5*fi(2)*( duy )
            
            if (abs(dux).gt.0.00001) then    
                r_lim(1)=( grad_up(i,1)*drx+grad_up(i,2)*dry )/dux
            else
              r_lim=10
            endif
            if (abs(duy).gt.0.00001) then         
                r_lim(2)=( grad_vp(i,1)*drx+grad_vp(i,2)*dry )/duy
            else
                r_lim(2)=10
            endif
            call limiter(r_lim,fi)
            rup_r=up(i)-0.5*fi(1)*( dux ) !velocities on the right side
            rvp_r=vp(i)-0.5*fi(2)*( duy )
            
            rhobar  = 0.5*rho0*( dw(i) + dw(j) )
            one_over_rhobar = 1/rhobar
	    abs_vr=sqrt(up(i)*up(i)+vp(i)*vp(i))
	    abs_vl=sqrt(up(j)*up(j)+vp(j)*vp(j))
	    cbar = max( cs(i)+abs_vr, cs(j)+abs_vl)
	    hbar = 0.5*( h_var(i)+h_var(j) )
	    
	    dot = drx*(rup_r-rup_l) + dry*(rvp_r-rvp_l)
             !dot = drx*dux +dry* duy
            
            endif !5

! --- i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) ) !4
     + .and.rr2.gt.1.e-18) then !5  !RV: modified (h_var)
	        
	        rad=sqrt(rr2)
	        call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab)      

                betai=rhop(i)*dm/alphap(i)

                p_v= 0.5*betai*grav/rho0
        
	     call viscosity(pi_visc,dot,drx,dry,dux,duy,kind,rr2,
     +                  cbar,rhobar,one_over_rhobar,hbar)

c conservative version (bonet)
                  
                ax(i)= ax(i) - pm(j)*(p_v+0.5*pi_visc)*frx
                ax(j)= ax(j) + pm(i)*(p_v+0.5*pi_visc)*frx
                
                ay(i)= ay(i) - pm(j)*(p_v+0.5*pi_visc)*fry
                ay(j)= ay(j) + pm(i)*(p_v+0.5*pi_visc)*fry

                  dot2 = + ( dux*frx + duy*fry )
                 !dot2=frx*(rup_r-rup_l) + fry*(rvp_r-rvp_l)
                 ar(i) = ar(i) + (pm(j)/rhop(j))*dot2 !density acc

             !just for debug
c                if (i.eq.1) then
c                 write(*,*) i, j, rad, frx, pm_ob(j), 
c     + pm_ob(j)*(p_v+0.5*pi_visc)*frx , ax(i), ay(i)
c                elseif (j.eq.1) then
c                 write(*,*) j, i, rad, frx, pm(i), 
c     + pm(i)*(p_v+0.5*pi_visc)*frx , ax(j), ay(j)
c                endif
                 
            endif
	    
! --- j-particle ---
            if( ( rr2.lt.( 4*h_var(j)*h_var(j) ) )
     + .and.rr2.gt.1.e-18) then !5  

	        !call kernel(-drx,-dry,rr2,j)
	        rad=sqrt(rr2)
	        call kernel(alphad, const_ker,-drx,-dry,rad,h_var(j),
     +     frx,fry,fac,Wab)      

                betaj=rhop(j)*dm/alphap(j)

                p_v= 0.5*betaj*grav/rho0

	     call viscosity(pi_visc,dot,drx,dry,dux,duy,kind,rr2,
     +                  cbar,rhobar,one_over_rhobar,hbar)

c conservative version (bonet)
                  
                 ax(i)= ax(i) + pm(j)*(p_v+0.5*pi_visc)*frx
                 ax(j)= ax(j) - pm(i)*(p_v+0.5*pi_visc)*frx
                
                 ay(i)= ay(i) + pm(j)*(p_v+0.5*pi_visc)*fry
                 ay(j)= ay(j) - pm(i)*(p_v+0.5*pi_visc)*fry 

                  dot2 = dux*frx + duy*fry
                 !dot2= frx*(rup_r-rup_l) + fry*(rvp_r-rvp_l)
                 ar(j) = ar(j) - (pm(i)/rhop(i))*dot2  !density acc

             !just for debug
c                if (i.eq.1) then
c                 write(*,*) j, rad, frx, pm_ob(j), 
c     + pm_ob(j)*(p_v+0.5*pi_visc)*frx , ax(i), ay(i)
c                elseif (j.eq.1) then
c                 write(*,*) j, i, rad, frx, pm(i), 
c     + pm(i)*(p_v+0.5*pi_visc)*frx , ax(j), ay(j)
c                endif
                 
            endif

        enddo !3
      enddo !2

      end
c

