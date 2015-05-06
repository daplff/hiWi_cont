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
      subroutine celij(j1,j2)
c
      use global_2D  
      
      real fi(2), r_lim(2), drx, dry, rr2, rad, dux, duy, rhobar, 
     + one_over_rhobar, abs_vr, abs_vl, cbar, hbar, betai, betaj, 
     + p_v, dot2, rup_l, rvp_l, rup_r, rvp_r, ddw, p_ast, 
     + r_vel_l_n, r_vel_r_n, rcs_l, rcs_r, rdw_l, rdw_r
      integer j1, j2, ii, i, j, jj
	 
      if(nc(j2,2).ne.0) then !2

        do ii=1,nc(j1,2) !3
          i = ibox(j1,2,ii)
         
          do jj=1,nc(j2,2) !4
           j = ibox(j2,2,jj)

            drx = xp(i) - xp(j)
            dry = yp(i) - yp(j)
            rr2 = drx*drx + dry*dry
            dux = up(i) - up(j)
            duy = vp(i) - vp(j)

	    if( rr2.lt.max( 4*h_var(i)*h_var(i),4*h_var(j)*h_var(j))) 
     +	            then !5

            rad=sqrt(rr2)

c ---  reconstruction of water depth at interface
            ddw=dw(i)-dw(j)
            if (abs(ddw).gt.0.00001) then 
               r_lim(1)=( grad_dw(j,1)*drx+grad_dw(j,2)*dry )/ddw 
            else
               r_lim(1)=10
            endif
            r_lim(2)=0
            call limiter(r_lim,fi)
            rdw_l=dw(j)+0.5*fi(1)*ddw
            
            if (abs(ddw).gt.0.00001) then 
                r_lim(1)=( grad_dw(i,1)*drx+grad_dw(i,2)*dry )/ddw
            else
                r_lim(1)=10
            endif
            r_lim(2)=0
            call limiter(r_lim,fi)
            rdw_r=dw(i)-0.5*fi(1)*ddw

c ---  reconstruction of velocities at interface  
                         
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
             
            r_vel_l_n=( rup_l*drx+rvp_l*dry )/ rad
            r_vel_r_n=( rup_r*drx+rvp_r*dry )/ rad

            rcs_l=sqrt(grav*rdw_l)
            rcs_r=sqrt(grav*rdw_r)  

            call balsara(p_ast,rdw_l,rdw_r,r_vel_l_n,r_vel_r_n,
     + rcs_l,rcs_r)
              
c            write(*,*) i,j, sqrt(p_ast/(0.5*9806)), rdw_r,
c    + rdw_l,  dw(i), dw(j)
           
            endif !5
            

! --- frx i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) )
     + .and.rr2.gt.1.e-18) then !5  !RV: modified (h_var)
               
	        call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab)   

                betai=rhop(i)*dm/alphap(i)
                p_v=p_ast*betai/( rhop(i)*rhop(i) )
                                   
                ax(i)= ax(i) - pm(j)*p_v*frx
                ax(j)= ax(j) + pm(i)*p_v*frx
                
                ay(i)= ay(i) - pm(j)*p_v*fry
                ay(j)= ay(j) + pm(i)*p_v*fry
                
                dot2 = - (dux*frx + duy*fry)
                ar(i) = ar(i) + (pm(j)/rhop(j))*dot2 !density acc
                               
            endif
	    
! --- frx j-particle ---
            if( ( rr2.lt.( 4*h_var(j)*h_var(j) ) )
     + .and.rr2.gt.1.e-18) then !5  

	        call kernel(alphad, const_ker,-drx,-dry,rad,h_var(j),
     +     frx,fry,fac,Wab)    

                betaj=rhop(j)*dm/alphap(j)
                p_v=p_ast*betaj/( rhop(j)*rhop(j) )
                                  
                ax(i)= ax(i) + pm(j)*p_v*frx
                ax(j)= ax(j) - pm(i)*p_v*frx
                
                ay(i)= ay(i) + pm(j)*p_v*fry
                ay(j)= ay(j) - pm(i)*p_v*fry

                dot2 = dux*frx + duy*fry
                ar(j) = ar(j) - (pm(i)/rhop(i))*dot2  !density acc

            endif

         enddo   !4
        enddo !3
	 endif !2

      end
