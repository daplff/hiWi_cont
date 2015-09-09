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

      subroutine celij_b_c(i,j1,j2)
c
      use global_2D  
      
      integer i, j1, j2, jj, j
      real drx, dry, rr2, rad
	 
          do jj=1,ncb(j2) !2
           j = iboxb(j2,jj)

            drx = xp(i) - xb(j)
            dry = yp(i) - yb(j)
c           drz = zp(i) - zp(j)
            rr2 = drx*drx + dry*dry

	    if ( (rr2.lt.( 4*hsm_b(j)*hsm_b(j) ))
     + .and.(rr2.gt.1.e-18) ) then !3

c            Calculating kernel & Normalized Kernel

	     !call kernel_b(alphad,drx,dry,rr2,hsm_b_ij(j1),frx,fry,Wab)
	     
	     rad=sqrt(rr2)
	     call kernel(alphad,const_ker,drx,dry,rad,hsm_b(j),
     +       frx,fry,fac,Wab)    !frx, fry are set to the kernel function's gradient times radial unit vector

             sum_b_f(i,1)=sum_b_f(i,1)-frx*drx*Vol_b(j)
             sum_b_f(i,2)=sum_b_f(i,2)-frx*dry*Vol_b(j)
             sum_b_f(i,3)=sum_b_f(i,3)-fry*drx*Vol_b(j)
             sum_b_f(i,4)=sum_b_f(i,4)-fry*dry*Vol_b(j)
             
             sum_b(i)=sum_b(i)+Wab*Vol_b(j)

               
	      endif !3
         enddo   !2

      end subroutine
      
