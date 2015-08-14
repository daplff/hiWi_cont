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

      subroutine self_corr(j1)
c
      use global_2D
      
      integer j1, jj_start, ii, i, j, jj
      real drx, dry, rr2, rad

       jj_start = 1

       do ii=1,nc(j1,2) !2
         i = ibox(j1,2,ii)

           jj_start = jj_start + 1
         
         do jj=jj_start,nc(j1,2) !3
            j = ibox(j1,2,jj)

             drx = xp(i) - xp(j)
             dry = yp(i) - yp(j)
             rr2 = drx*drx + dry*dry

! --- i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) ) !4
     + .and.rr2.gt.1.e-18) then !5  !RV: modified (h_var)

	         rad=sqrt(rr2)
	         call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +           frx,fry,fac,Wab) 

             sum_f(i,1)=sum_f(i,1)-frx*drx*Vol(j)
             sum_f(i,2)=sum_f(i,2)-frx*dry*Vol(j)
             sum_f(i,3)=sum_f(i,3)-fry*drx*Vol(j)
             sum_f(i,4)=sum_f(i,4)-fry*dry*Vol(j)

            endif
	    
! --- j-particle ---
            if( ( rr2.lt.(4*h_var(j)*h_var(j) ) )
     + .and.rr2.gt.1.e-18) then !5  

	         rad=sqrt(rr2)
	         call kernel(alphad, const_ker,-drx,-dry,rad,h_var(j),
     +           frx,fry,fac,Wab)

             sum_f(j,1)=sum_f(j,1)+frx*drx*Vol(i)
             sum_f(j,2)=sum_f(j,2)+frx*dry*Vol(i)
             sum_f(j,3)=sum_f(j,3)+fry*drx*Vol(i)
             sum_f(j,4)=sum_f(j,4)+fry*dry*Vol(i)     

            endif

        enddo !3
      enddo !2

      end
c

