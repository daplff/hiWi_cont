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


c subroutine for bottom elevation h_t, 
c                 first derivative of bottom elevation dh_SPH
c                 second derivative of bottom elevation ddh_SPH    
     
      subroutine ac_b
      use global_2D
     
      real dxx, dyy, det, one_over_det, a, b, c, d
      integer i, icell, jcell, j1
     
      do i=1,np

c        -- Zeroing Variables --         
c          h_t(i)=0
           dH_SPH_x(i)=0.
           dH_SPH_y(i)=0.
           ddH_SPH_xx(i)=0.
           ddH_SPH_xy(i)=0.
           ddH_SPH_yy(i)=0.
           sum_b_f(i,:)=0.
           sum_b(i)=0.
           fr_SPH(i)=0.
      enddo
      
c      
c       -- gradient correction --
c
     
      do i=1,np
      
         if (iflag(i).eq.1) then      
c        -- calculating position of particle i --
         dxx = xp(i) - xminb
         dyy = yp(i) - yminb
         icell = int( dxx*one_over_2hsm_b ) + 1
         jcell = int( dyy*one_over_2hsm_b ) + 1

         if ( (icell.ge.0 .and. icell.le.ncx_b).and.
     +(jcell.ge.0 .and. jcell.le.ncy_b) ) then
         
         j1=icell + (jcell - 1)*ncx_b
                        
         if (icell.lt.ncx_b) call celij_b_c(i,j1,j1+1) !EAST

         if (icell.gt.1) call celij_b_c(i,j1,j1-1) !WEST
             
         if (jcell.lt.ncy_b) then
             call celij_b_c(i,j1,j1+ncx_b) !NORTH
             if (icell.gt.1) call celij_b_c(i,j1,j1+ncx_b-1) !NORTH WEST
             if (icell.lt.ncx_b) call celij_b_c(i,j1,j1+ncx_b+1) !NORTH EAST
         endif
             
         if (jcell.gt.1) then
             call celij_b_c(i,j1,j1-ncx_b) !SOUTH
             if (icell.gt.1) call celij_b_c(i,j1,j1-ncx_b-1) !SOUTH WEST
             if (icell.lt.ncx_b) call celij_b_c(i,j1,j1-ncx_b+1) !SOUTH EAST
         endif           

         call celij_b_c(i,j1,j1)

         endif
         
         endif
         
      enddo
      
c      
c       -- inverting matrix correction --
c      
      
      do i=1,np
         if (iflag(i).eq.1) then  
         a=sum_b_f(i,1)
         b=sum_b_f(i,2)
         c=sum_b_f(i,3)
         d=sum_b_f(i,4)
         
         det=(a*d-b*c)
         if (abs(det).gt.1.E-6) then
         
             one_over_det=1/det
             sum_b_f(i,1)=d*one_over_det
             sum_b_f(i,2)=-b*one_over_det
             sum_b_f(i,3)=-c*one_over_det
             sum_b_f(i,4)=a*one_over_det
         
         else
         
             sum_b_f(i,1)=1.
             sum_b_f(i,2)=0.
             sum_b_f(i,3)=0.
             sum_b_f(i,4)=1.
         
         endif
         endif       
      enddo
      
c      
c       -- first and second derivatives of bottom --
c      
     
      do i=1,np
         if (iflag(i).eq.1) then
c        -- calculating position of particle i --
         dxx = xp(i) - xminb
         dyy = yp(i) - yminb
         icell = int( dxx*one_over_2hsm_b ) + 1
         jcell = int( dyy*one_over_2hsm_b ) + 1
         
         j1=icell + (jcell - 1)*ncx_b

         if ( (icell.ge.0 .and. icell.le.ncx_b).and.
     +(jcell.ge.0 .and.jcell.le.ncy_b) ) then
                        
         if (icell.lt.ncx_b) call celij_b(i,j1,j1+1) !EAST

         if (icell.gt.1) call celij_b(i,j1,j1-1) !WEST
             
         if (jcell.lt.ncy_b) then
             call celij_b(i,j1,j1+ncx_b) !NORTH
             if (icell.gt.1) call celij_b(i,j1,j1+ncx_b-1) !NORTH WEST
             if (icell.lt.ncx_b) call celij_b(i,j1,j1+ncx_b+1) !NORTH EAST
         endif
             
         if (jcell.gt.1) then
             call celij_b(i,j1,j1-ncx_b) !SOUTH
             if (icell.gt.1) call celij_b(i,j1,j1-ncx_b-1) !SOUTH WEST
             if (icell.lt.ncx_b) call celij_b(i,j1,j1-ncx_b+1) !SOUTH EAST
         endif           

         call celij_b(i,j1,j1)

         endif

         endif 

      enddo
      
      end     
      
 

      

      
      
