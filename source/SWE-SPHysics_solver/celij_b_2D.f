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

      subroutine celij_b(i,j1,j2)
c
      use global_2D
      
      integer i, j1, j2, jj, j
      real drx, dry, rr, rad, den, fab, frxc, fryc, rr2
	 
          do jj=1,ncb(j2) !2
           j = iboxb(j2,jj)

            drx = xp(i) - xb(j)
            dry = yp(i) - yb(j)
c           drz = zp(i) - zp(j)
            rr2 = drx*drx + dry*dry

	    if ( (rr2.lt.( 4*hsm_b(j)*hsm_b(j) )).and.
     + (rr2.gt.1.e-18) ) then !3

c            Calculating kernel & Normalized Kernel

	     rad=sqrt(rr2)
	     call kernel(alphad,const_ker,drx,dry,rad,hsm_b(j) ,
     +       frx,fry,fac,Wab)    
	     
c            kernel correction	     
	     
	     frxc= sum_b_f(i,1)*frx+sum_b_f(i,2)*fry	     
	     fryc= sum_b_f(i,3)*frx+sum_b_f(i,4)*fry

              !first derivatives
              dH_SPH_x(i)=dH_SPH_x(i)+Vol_b(j)*(hb(j)- h_t(i))*frxc
              dH_SPH_y(i)=dH_SPH_y(i)+Vol_b(j)*(hb(j)- h_t(i))*fryc
 
              den=(rr2+eta2)
              fab=-( hb(j)- h_t(i) )*( frxc*drx + fryc*dry )/den
                    
              !second derivatives ( Monaghan 2005 )          
              ddH_SPH_xx(i)=ddH_SPH_xx(i)+Vol_b(j)*
     + ( 4*drx*drx/den-1 )*fab
              ddH_SPH_xy(i)=ddH_SPH_xy(i)+Vol_b(j)*
     + ( 4*drx*dry/den )*fab
              ddH_SPH_yy(i)=ddH_SPH_yy(i)+Vol_b(j)*
     + ( 4*dry*dry/den-1 )*fab
              
              !SPH summation for the friction term
              fr_SPH(i)=fr_SPH(i)+Vol_b(j)*fr(j)*Wab/sum_b(i)
               
	      endif !3
         enddo   !2

      end subroutine
      
