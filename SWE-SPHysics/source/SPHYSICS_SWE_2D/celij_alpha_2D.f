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

      subroutine celij_alpha(j1,j2)
c
      use global_2D
      
      integer j1, j2, ii, i, j, jj
      real drx, dry, rr2, rad, frxc, fryc
            
        if(nc(j2,2).ne.0) then !2

        do ii=1,nc(j1,2) !3
          i = ibox(j1,2,ii)
         
          do jj=1,nc(j2,2) !4
           j = ibox(j2,2,jj)

            drx = xp(i) - xp(j)
            dry = yp(i) - yp(j)
            rr2 = drx*drx + dry*dry
            
! --- i-particle ---
	    if( ( rr2.lt.( 4*h_var(i)*h_var(i) ) )
     + .and.rr2.gt.1.e-18) then !5  !RV: modified (h_var)

	         rad=sqrt(rr2)
	         call kernel(alphad, const_ker,drx,dry,rad,h_var(i),
     +           frx,fry,fac,Wab)    
 
                 alphap(i)=alphap(i)-rad*pm(j)*fac

c            kernel correction	     
	     
	     frxc= sum_f(i,1)*frx+sum_f(i,2)*fry	     
	     fryc= sum_f(i,3)*frx+sum_f(i,4)*fry
                 
                 grad_up(i,1)=grad_up(i,1)+
     +                    (Vol(j))*(up(j)-up(i))*frxc !dup/dx
                 grad_up(i,2)=grad_up(i,2)+
     +                    (Vol(j))*(up(j)-up(i))*fryc !dup/dy
                
                 grad_vp(i,1)=grad_vp(i,1)+
     +                    (pm(j)/rhop(j))*(vp(j)-vp(i))*frxc !dvp/dx                 
                 grad_vp(i,2)=grad_vp(i,2)+
     +                    (Vol(j))*(vp(j)-vp(i))*fryc !dvp/dy

                 grad_dw(i,1)=grad_dw(i,1)+
     +                    (pm(j)/rhop(j))*(dw(j)-dw(i))*frxc !ddw/dx                 
                 grad_dw(i,2)=grad_dw(i,2)+
     +                    (Vol(j))*(dw(j)-dw(i))*fryc !ddw/dy

            endif
	    
! --- j-particle ---
            if( ( rr2.lt.( 4*h_var(j)*h_var(j) ) )
     + .and.rr2.gt.1.e-18) then !5  

	         rad=sqrt(rr2)
	         call kernel(alphad, const_ker,-drx,-dry,rad,h_var(j),
     +           frx,fry,fac,Wab)     
                 
                 alphap(j)=alphap(j)-rad*pm(i)*fac

c            kernel correction	     
	     
	     frxc= sum_f(j,1)*frx+sum_f(j,2)*fry	     
	     fryc= sum_f(j,3)*frx+sum_f(j,4)*fry
                 
                 grad_up(j,1)=grad_up(j,1)+
     +                    (Vol(i))*(up(i)-up(j))*frxc !dup/dx
                 grad_up(j,2)=grad_up(j,2)+
     +                    (Vol(i))*(up(i)-up(j))*fryc !dup/dy 
                
                 grad_vp(j,1)=grad_vp(j,1)+
     +                    (Vol(i))*(vp(i)-vp(j))*frxc !dvp/dx            
                 grad_vp(j,2)=grad_vp(j,2)+
     +                    (Vol(i))*(vp(i)-vp(j))*fryc !dvp/dy  

                 grad_dw(j,1)=grad_dw(j,1)+
     +                    (Vol(i))*(dw(i)-dw(j))*frxc !ddw/dx            
                 grad_dw(j,2)=grad_dw(j,2)+
     +                    (Vol(i))*(dw(i)-dw(j))*fryc !ddw/dy              

            endif

         enddo   !4
        enddo !3
	 endif !2

      end
