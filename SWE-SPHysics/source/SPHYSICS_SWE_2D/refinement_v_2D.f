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

! define new velocities for refined particles conserving the uh*Area quantity.       
       
      subroutine refinement_v
       
      use global_2D

      integer ii, i, k, j
      real sum_a, c_area, upold, vpold, upoldo, vpoldo
      
c      open(61, file='c_area.txt')
       
      do ii=1,ntotref
         
         i=li_ref(ii) !index of refined particle (original one)
      
         sum_a=0 !sum of area for refined particles
         
         !calculating the summation of mass for refined particles
         do j=1,7
            k=irli(i,j)
            if (k.ne.0) then
                sum_a=sum_a+pm(k)/( rhop(k) )
            endif
         enddo
         
         c_area=( pmold(i)/(dw(i)*rho0) )/sum_a
         !c_area=1
         
        !velocities for refined particles (new one)
        upold=up(i)
        vpold=vp(i)
        upoldo=uo(i)
        vpoldo=vo(i)
         do j=1,7
            k=irli(i,j)
            if (k.ne.0) then
                up(k)=c_area*( dw(i)/dw(k) )*upold
                vp(k)=c_area*( dw(i)/dw(k) )*vpold
                uo(k)=c_area*( dw(i)/dw(k) )*upoldo
                vo(k)=c_area*( dw(i)/dw(k) )*vpoldo
            endif
         enddo
      
      enddo
      
c      close(61)
      
      return
      
      end
      
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++      
! SPH velocity interpolation for particle i from refinement
!           
           
      subroutine ac_vel(i)
c
      use global_2D 

      integer i, icell, jcell, j1
      real dxx, dyy
      
      sum_v=0
      up(i)=0
      vp(i)=0
      uo(i)=0
      vo(i)=0

c        -- calculating position of particle i --
      dxx = xp(i) - xmin                    
      dyy = yp(i) - ymin

      icell = int( dxx * one_over_2hmax ) + 1
      jcell = int( dyy * one_over_2hmax ) + 1
      ! j1 is the linear cell position in the matrix of 2h cells
      j1 = icell + (jcell - 1)*ncx 
             
      if (icell.lt.ncx) call celij_v(i,j1,j1+1) !EAST

      if (icell.gt.1) call celij_v(i,j1,j1-1) !WEST
             
      if (jcell.lt.ncy) then
          call celij_v(i,j1,j1+ncx) !NORTH
          if (icell.gt.1) call celij_v(i,j1,j1+ncx-1) !NORTH WEST
          if (icell.lt.ncx) call celij_v(i,j1,j1+ncx+1) !NORTH EAST
      endif
             
      if (jcell.gt.1) then
          call celij_v(i,j1,j1-ncx) !SOUTH
          if (icell.gt.1) call celij_v(i,j1,j1-ncx-1) !SOUTH WEST
          if (icell.lt.ncx) call celij_v(i,j1,j1-ncx+1) !SOUTH EAST
      endif  
           
      call celij_v(i,j1,j1)
      
      !Shepard filter
      if (sum_v.gt.0.1) then
          
          up(i)=up(i)/sum_v
          vp(i)=vp(i)/sum_v
          
          uo(i)=uo(i)/sum_v
          vo(i)=vo(i)/sum_v
          
      endif     
              
      end      
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

      subroutine celij_v(i,j1,j2)

      use global_2D 

      integer i, j1, j2, jj, j
      real drx, dry, rr2, rad
	
        if(nc(j2,2).ne.0) then !1
         
          do jj=1,nc(j2,2) !2
           j = ibox(j2,2,jj)

            drx = xp(i) - xp(j)
            dry = yp(i) - yp(j)
            rr2 = drx*drx + dry*dry

	    if( (rr2.lt.( 4*h_var(i)*h_var(i) )).and.(j.le.npold) )then  !
               rad=sqrt(rr2)
	       call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab)  

               up(i)=up(i)+up(j)*(pmold(j)/rhop(j))*Wab
               vp(i)=vp(i)+vp(j)*(pmold(j)/rhop(j))*Wab
               
               uo(i)=uo(i)+uo(j)*(pmold(j)/rhop(j))*Wab
               vo(i)=vo(i)+vo(j)*(pmold(j)/rhop(j))*Wab
               
               sum_v=sum_v+(pmold(j)/rhop(j))*Wab
	             
	    endif !3
         enddo   !2
	endif !1

      end
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
