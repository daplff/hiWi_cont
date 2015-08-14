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

      subroutine celij_vir(i,j1,j2)
c
      use global_2D

      real grad_upv(2), grad_vpv(2), fi(2), r_lim(2), drxv, dryv
      real xvir, yvir, drx, dry, rr2, rad, c_mvir, pm_vir, dw_vir
      real rhop_vir, cs_vir, dux, duy, dot2
      real betai, p_v
      integer i, j1, j2, j, k, jj

      real up_n, vp_n, up_t, vp_t, upv_n, vpv_n, upv_t, vpv_t, upv, vpv, 
     + radv, pippo, rrv, rrvn, lim_dist
 
      real p_ast,rdw_l,rdw_r,r_vel_l_n,r_vel_r_n, rcs_l,rcs_r
	 

        if(nc(j2,1).ne.0) then !2
         
          do jj=1,nc(j2,1) !4
           j = ibox(j2,1,jj)
        
           drxv = xp(i)-xv(j)
           dryv = yp(i)-yv(j)
           rrv=sqrt(drxv*drxv+dryv*dryv)
           rrvn=max( rrv,0.2*dxv(j) )
           drxv= drxv*(rrvn/rrv)
           dryv= dryv*(rrvn/rrv)
           radv=sqrt(drxv*drxv+dryv*dryv)

           lim_dist=dxv(j)*h_var(i)/h_var0(i)
        
           do k=1,2 !3 loop for virtual particles

            !virtual particle position   
            if (k.eq.1) then         
               !xvir=xv(j)+( xv(j)-xp(i) )
               !yvir=yv(j)+( yv(j)-yp(i) )

               xvir=xv(j)-drxv*lim_dist/max(lim_dist,drxv*vnxv(j))
               yvir=yv(j)-dryv*lim_dist/max(lim_dist,dryv*vnyv(j))

            elseif (k.eq.2) then         
               !xvir=xv(j)+3.*( xv(j)-xp(i) )
               !yvir=yv(j)+3.*( yv(j)-yp(i) )

                xvir=xv(j)-3*drxv*lim_dist/max(lim_dist,drxv*vnxv(j))
                yvir=yv(j)-3*dryv*lim_dist/max(lim_dist,dryv*vnyv(j))

            endif
                        
            drx=xp(i)-xvir
            dry=yp(i)-yvir
            rr2=(drx*drx+dry*dry)
             
	    if( rr2.lt.4*h_var(i)*h_var(i).and.rr2.gt.1.E-18) then
                 
             c_mvir=2.4*dxv(j)/h_var(i)
             pm_vir=pm0(i)*c_mvir

             rad=sqrt(rr2)

             dux=2*up(i)
             duy=2*vp(i)

c velocity of the virtual particle with slip condition
             
             up_n= ( up(i)*vnxv(j)+vp(i)*vnyv(j) )*vnxv(j)
             vp_n= ( up(i)*vnxv(j)+vp(i)*vnyv(j) )*vnyv(j)

             up_t=up(i)-up_n
             vp_t=vp(i)-vp_n

             upv_n=up_n-up_n*( rad/radv )
             vpv_n=vp_n-vp_n*( rad/radv )
             
             upv_t=up_t !this means slip condition
             vpv_t=vp_t
             
             upv=upv_n+upv_t
             vpv=vpv_n+vpv_t

c right and left state of the riemann problem
             
             r_vel_r_n=( up(i)*drx+vp(i)*dry ) / rad
             r_vel_l_n=( upv*drx+vpv*dry ) / rad

             rcs_l=sqrt(grav*dw(i))
             rcs_r=rcs_l

             rdw_l=dw(i)
             rdw_r=dw(i)

             call balsara(p_ast,rdw_l,rdw_r,r_vel_l_n,r_vel_r_n,
     + rcs_l,rcs_r)

c             p_ast=0.5*9806*dw(i)*dw(i)

             c_mvir=2.4*dxv(j)/h_var(i)
             pm_vir=pm0(i)*c_mvir
             
             dw_vir=dw(i)
             
             rhop_vir=rho0*dw_vir
     
             betai=rhop(i)*dm/alphap(i)
            
	     call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab)     

              betai=rhop(i)*dm/alphap(i)
              p_v=p_ast*betai/( rhop(i)*rhop(i) )

c              write(33,*) i, xp(i)-1000, yp(i)-1000, 
c     + 0.5*9806*dw(i)*dw(i), p_ast, upv, vpv, up(i), vp(i)
             
              ax(i)= ax(i) - pm_vir*p_v*frx
              ay(i)= ay(i) - pm_vir*p_v*fry 
   
              !density acc
              dot2 = - (dux*frx + duy*fry)
              ar(i) = ar(i) + (pm_vir/rhop_vir)*dot2 !density acc

            endif
            
         enddo !5
         
         enddo   !4
	 endif !2

      end
