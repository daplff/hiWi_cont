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
c
c SPH interpolation with corr kernel over fluid particles to obtain u_i and dw_i
c for particles in open boundary area
c      
      
      subroutine open_bc(ini,ifin)
c
      use global_2D
      
      integer ini,ifin
      integer i, icell, jcell, j1
      
      real dxx, dyy, sqdw, uo_ob_p, up_ob_p, upi_p, vpi_p, 
     + uoi_p, voi_p, vp_ob_p, vo_ob_p

!to be removed
      !sm_maxob=0. !maximum smoothing lenght for open boundary particles
      
      do i=ini,ifin !1
         if (iflag_ob(i).gt.1) then !2  !just for boundary condition particles
         
         ! zeroing variables
           upi(i)=0
           vpi(i)=0
           uoi(i)=0
           voi(i)=0
           dwi(i)=0
           sum_wab(i)=0
           
c        -- calculating position of particle i --
           dxx = xp_ob(i) - xmin
           dyy = yp_ob(i) - ymin
           icell = int( dxx*one_over_2hmax ) + 1
           jcell = int( dyy*one_over_2hmax ) + 1
           j1 = icell + (jcell - 1)*ncx 
           
           if (icell.lt.ncx) call celij_openbc(i,j1,j1+1) !EAST

           if (icell.gt.1) call celij_openbc(i,j1,j1-1) !WEST
             
           if (jcell.lt.ncy) then
                call celij_openbc(i,j1,j1+ncx) !NORTH
                if (icell.gt.1) call celij_openbc(i,j1,j1+ncx-1) !NORTH WEST
                if (icell.lt.ncx) call celij_openbc(i,j1,j1+ncx+1) !NORTH EAST
           endif
             
           if (jcell.gt.1) then
                call celij_openbc(i,j1,j1-ncx) !SOUTH
                if (icell.gt.1) call celij_openbc(i,j1,j1-ncx-1) !SOUTH WEST
                if (icell.lt.ncx) call celij_openbc(i,j1,j1-ncx+1) !SOUTH EAST
           endif  
           
           call celij_openbc(i,j1,j1)

           if (sum_wab(i).gt.0.) then 
                     
               upi(i)=upi(i)/sum_wab(i)
               vpi(i)=vpi(i)/sum_wab(i)
               uoi(i)=uoi(i)/sum_wab(i)
               voi(i)=voi(i)/sum_wab(i)
               dwi(i)=dwi(i)/sum_wab(i)
               
            else
       
               upi(i)=0
               vpi(i)=0
               uoi(i)=0
               voi(i)=0
               dwi(i)=dw0_ob(i)

c               write(*,*) 'open bc part',i, '(x,y)', xp_ob(i), 
c     + yp_ob(i), ' no interaction with fluid p. '
               
           endif
           
           !new coordinate system
           upi_p=+upi(i)*verbc(ibc(i),1)+vpi(i)*verbc(ibc(i),2) !velocity normal to the boundary
           vpi_p=-upi(i)*verbc(ibc(i),2)+vpi(i)*verbc(ibc(i),1) !velocity tangent to the boundary
           uoi_p=+uoi(i)*verbc(ibc(i),1)+voi(i)*verbc(ibc(i),2)
           voi_p=-uoi(i)*verbc(ibc(i),2)+voi(i)*verbc(ibc(i),1)

c           write(*,*) i, xp_ob(i), yp_ob(i), upi(i), vpi(i), 
c     + dwi(i), sum_wab(i), verbc(ibc(i),1), verbc(ibc(i),2), 
c     + upi_p, vpi_p
                      
           !imposition of bc upstream
           if (iflag_ob(i).eq.2) then !inflow  !3
           
               vp_ob_p=0
               vo_ob_p=0
           
               up_ob_p=upbc(ibc(i))
               uo_ob_p=upbc(ibc(i))
               
               if ( up_ob_p.gt.sqrt(grav*dwbc(ibc(i))) ) then
                   dw_ob(i)=dwbc(ibc(i)) !supercritical flow
               else
                sqdw=(0.5/sqrt(grav))*(upi_p-up_ob_p)+sqrt(dwi(i))
                dw_ob(i)=sqdw*sqdw
               endif
           
           elseif (iflag_ob(i).eq.3) then !outflow
           
               if ( upi_p.gt.sqrt(grav*dwi(i)) ) then
                   dw_ob(i)=dwi(i)
                   up_ob_p=upi_p !supercritical flow
                   uo_ob_p=uoi_p
                   vp_ob_p=vpi_p
                   vo_ob_p=voi_p
               else
                   dw_ob(i)=dwbc(ibc(i))

                   up_ob_p=upi_p-2*sqrt(grav)*
     +              ( sqrt(dwi(i))-sqrt( dwbc(ibc(i)) ) )
                   uo_ob_p=uoi_p-2*sqrt(grav)*
     +              ( sqrt(dwi(i))-sqrt( dwbc(ibc(i)) ) )
                   vp_ob_p=vpi_p
                   vo_ob_p=voi_p
               endif
                              
           endif !3
           
           !back to old coordinate system
           up_ob(i)=up_ob_p*verbc(ibc(i),1)-vp_ob_p*verbc(ibc(i),2)
           vp_ob(i)=up_ob_p*verbc(ibc(i),2)+vp_ob_p*verbc(ibc(i),1)
           uo_ob(i)=uo_ob_p*verbc(ibc(i),1)-vo_ob_p*verbc(ibc(i),2)
           vo_ob(i)=uo_ob_p*verbc(ibc(i),2)+vo_ob_p*verbc(ibc(i),1)
           
           cs_ob(i)=sqrt(dw_ob(i)*grav)
           rhop_ob(i)=rho0*dw_ob(i)
           alphap_ob(i)=rhop_ob(i)*dm

!to be removed
!           sm_maxob=max( sm_maxob,h_var_ob(i) ) !maximum smoothing length

  
         endif !2
         
      enddo !1
      
      end subroutine
      
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^      

      subroutine celij_openbc(i,j1,j2)
       
      use global_2D
      
      integer i, j1, j2, jj, j
      real drx, dry, rr2, dux, rad
      
          if(nc(j2,2).ne.0) then !1
         
          do jj=1,nc(j2,2) !2
             j = ibox(j2,2,jj)

            drx = xp_ob(i) - xp(j)
            dry = yp_ob(i) - yp(j)
            rr2 = drx*drx  + dry*dry

	    if( ( rr2.le.( 4*h_var_ob(i)*h_var_ob(i) ) ) !3
     + .and.iflag(j).eq.1.and.rr2.ge.1.E-18) then !5 
	       
	     rad=sqrt(rr2)
	     call kernel(alphad,const_ker,drx,dry,rad,h_var_ob(i),
     +       frx,fry,fac,Wab)    

               upi(i)=upi(i)+up(j)*pm(j)/rhop(j)*Wab
               vpi(i)=vpi(i)+vp(j)*pm(j)/rhop(j)*Wab
               uoi(i)=uoi(i)+uo(j)*pm(j)/rhop(j)*Wab
               voi(i)=voi(i)+vo(j)*pm(j)/rhop(j)*Wab
               dwi(i)=dwi(i)+dw(j)*pm(j)/rhop(j)*Wab
               
               sum_wab(i)=sum_wab(i)+pm(j)/rhop(j)*Wab
               
            endif
            
          enddo
          endif
          
          
               
      end subroutine
      
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^      
