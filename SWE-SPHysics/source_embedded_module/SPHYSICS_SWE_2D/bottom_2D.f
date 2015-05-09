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
     
      subroutine bottom
      use global_2D
      
      integer i, icell, jcell, j1
      real dxx, dyy
      

     
      do i=1,np

c        -- Zeroing Variables --         
          h_t(i)=0
          sum_h_t(i)=0
      enddo

      do i=1,np

         if (iflag(i).eq.1) then
c        -- calculating position of particle i --
         dxx = xp(i) - xminb
         dyy = yp(i) - yminb
         icell = int( dxx*one_over_2hsm_b ) + 1
         jcell = int( dyy*one_over_2hsm_b ) + 1
         
         j1=icell + (jcell - 1)*ncx_b

         if ( (icell.ge.0 .and. icell.le.ncx_b).and.
     +(jcell.ge.0 .and. jcell.le.ncy_b) ) then
         
         if (icell.lt.ncx_b) call celij_hb(i,j1,j1+1) !EAST

         if (icell.gt.1) call celij_hb(i,j1,j1-1) !WEST
             
         if (jcell.lt.ncy_b) then
             call celij_hb(i,j1,j1+ncx_b) !NORTH
             if (icell.gt.1) call celij_hb(i,j1,j1+ncx_b-1) !NORTH WEST
             if (icell.lt.ncx_b) call celij_hb(i,j1,j1+ncx_b+1) !NORTH EAST
         endif
             
         if (jcell.gt.1) then
             call celij_hb(i,j1,j1-ncx_b) !SOUTH
             if (icell.gt.1) call celij_hb(i,j1,j1-ncx_b-1) !SOUTH WEST
             if (icell.lt.ncx_b) call celij_hb(i,j1,j1-ncx_b+1) !SOUTH EAST
         endif           

         call celij_hb(i,j1,j1)

         endif
                      
             !shepard correction
             if (sum_h_t(i) .gt.0.) then
                 h_t(i)=h_t(i)/sum_h_t(i)
             else
               iflag(i)=0
               write(*,*) 'particle xp,yp out of domain (', 
     +  xp(i),yp(i),')','i=',i
	       xp(i)=xtrash
 	       yp(i)=ytrash
	       up(i)=0.
 	       vp(i)=0.
 	       dw(i)=0.
 	       uo(i)=0.
 	       vo(i)=0.
 	       udot(i)=0.
 	       vdot(i)=0.

!                 if (sum_h_t(i).lt.0.8 .or. sum_h_t(i).gt.1.2) then
!                       write(21,*) 'WARNING particle', i, 
!     + 'with sumWij =', sum_h_t(i), 'position x,y', xp(i),yp(i)
!                 endif
             endif
      
         endif
      
      enddo

            
      end   

