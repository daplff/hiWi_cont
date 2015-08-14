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

      subroutine celij_dw_ob(i,j1,j2)

      use global_2D
      
      integer i, j1, j2, jj, j
      real  drx, dry, rr2, rad
	 
        if(nc(j2,3).ne.0) then !1
         
          do jj=1,nc(j2,3) !2
           j = ibox(j2,3,jj)

            drx = xp(i) - xp_ob(j)
            dry = yp(i) - yp_ob(j)
            rr2 = drx*drx+dry*dry
            
! --- frx i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) ) !3
     + .and.rr2.gt.1.e-18) then 
               
	    rad=sqrt(rr2)
	    call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +     frx,fry,fac,Wab)  
	       
               rhop_sum=rhop_sum+pm_ob(j)*Wab
               alphap(i)=alphap(i)-rad*pm_ob(j)*fac 

               if (i.eq.71567) then
                 write(*,*) 'ob', i, j, drx, dry, rad*pm_ob(j)*fac, 
     + rhop_sum
               endif
             
	    endif !3
	   	      
         enddo   !2
	endif !1

      end
