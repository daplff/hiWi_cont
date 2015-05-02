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

      subroutine ac_corr
c
      use global_2D
      
      real dxx, dyy, a, b, c, d, det, one_over_det
      integer i, j1, ly, lx, lx2, ly2, icell, jcell

c        -- Zeroing Variables --         
      do i=nstart,np
      
         sum_f(i,:)=0 !matrix for gradient correction
          
      enddo
      
c          -- fluid particles - 
                 
       do ly=1,ncy                              !2
       do lx=1,ncx                              !3
         j1 = lx + (ly-1)*ncx
         
         if(nc(j1,2).gt.0) then           !4

           lx2 = lx+1
           if(lx2.le.ncx)then
             call celij_corr(j1,j1+1)     !East
           endif      
      
           ly2=ly+1
           if(ly2.le.ncy)then
       
             !- Same row -
             call celij_corr(j1,j1+ncx)   !North
       
             lx2=lx-1
             if(lx2.ge.1) call celij_corr(j1,j1+ncx-1)   !North-West
       
             lx2=lx+1
             if(lx2.le.ncx) call celij_corr(j1,j1+ncx+1)   !North-East                 
       
           endif    !End of:   if(ly2.le.ncy)then
         endif    !End of:  if(nc(j1,kind_p1).gt.0) then
       
       enddo                                    !3
       enddo                                    !2
       
       do j1=1,nct
           if(nc(j1,2).gt.0)
     +		call self_corr(j1)
       enddo     

c --- matrix inversion

       do i=1,np

        if (iflag(i).eq.1) then  
         a=sum_f(i,1)
         b=sum_f(i,2)
         c=sum_f(i,3)
         d=sum_f(i,4)
         
         det=(a*d-b*c)
         if (abs(det).gt.1.E-6) then
         
             one_over_det=1/det
             sum_f(i,1)=d*one_over_det
             sum_f(i,2)=-b*one_over_det
             sum_f(i,3)=-c*one_over_det
             sum_f(i,4)=a*one_over_det
         
         else
         
             sum_f(i,1)=1.
             sum_f(i,2)=0.
             sum_f(i,3)=0.
             sum_f(i,4)=1.
         
         endif
        endif       

       enddo
 
       return
       end

