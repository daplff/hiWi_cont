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

      subroutine celij_alpha_vir(i,j1,j2)
c
      use global_2D
      
      integer i, j1, j2, j, jj, k
      real drx, dry, rr2, rad, xvir, yvir, c_mvir, pm_vir, rhop_vir
      real drxv, dryv, dt_cand, rrv, rrvn, lim_dist
	 

        if(nc(j2,1).ne.0) then !2
         
          do jj=1,nc(j2,1) !4
           j = ibox(j2,1,jj)

           drxv = xp(i)-xv(j)
           dryv = yp(i)-yv(j)
           rrv=sqrt(drxv*drxv+dryv*dryv)
           rrvn=max( rrv,0.2*dxv(j) )
           drxv= drxv*(rrvn/rrv)
           dryv= dryv*(rrvn/rrv)

           lim_dist=dxv(j)*h_var(i)/h_var0(i)
        
           do k=1,2 !3 loop for virtual particles

            !virtual particle position   
            if (k.eq.1) then         
            !   xvir=xv(j)+( xv(j)-xp(i) )
            !   yvir=yv(j)+( yv(j)-yp(i) )

               xvir=xv(j)-drxv*lim_dist/max(lim_dist,drxv*vnxv(j))
               yvir=yv(j)-dryv*lim_dist/max(lim_dist,dryv*vnyv(j))

            elseif (k.eq.2) then         
           !    xvir=xv(j)+3.*( xv(j)-xp(i) )
           !    yvir=yv(j)+3.*( yv(j)-yp(i) )
             xvir=xv(j)-3*drxv*lim_dist/max(lim_dist,drxv*vnxv(j))
             yvir=yv(j)-3*dryv*lim_dist/max(lim_dist,dryv*vnyv(j))

            endif
            
            drx=xp(i)-xvir
            dry=yp(i)-yvir
            rr2=(drx*drx+dry*dry)
            
            !c_mvir=1.*( (dxv/h_var(i))/coef_mv-1)+1
            c_mvir=2.4*dxv(j)/h_var(i)
            ! c_mvir=1.
             
              pm_vir=pm0(i)*c_mvir
              
            rhop_vir=rhop(i) !virtual depth
            
! --- i-particle ---

	    if( rr2.lt.4*h_var(i)*h_var(i).and.rr2.gt.1.E-18) then !5  !<=======RV: modified (h_var)

	         rad=sqrt(rr2)
	         call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +           frx,fry,fac,Wab)    
 
                 alphap(i)=alphap(i)-rad*pm_vir*fac
                 
                 !time step to avoid particle wall penetration                 
            dt_cand=(0.5*sqrt(rr2))/(abs(up(i)*drx+vp(i)*dry)+1.E-10)
                 dt_vir=min(dt_cand,dt_vir)
                 
            endif
            
         enddo   !5
         
         enddo   !4
	 endif !2

      end
