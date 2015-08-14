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
      subroutine kernel(alphad,const_ker,drx,dry,rad,h,frx,fry,fac,Wab)
      
      implicit none

      real alphad, const_ker(2), drx, dry, rad, h, frx, fry, fac, Wab
      real one_over_h, one_over_rad, qq , tmq, tm2, tm3, q2, q3

      one_over_h=1./h
      one_over_rad=1./rad

c     Cubic 2D Kernel

      qq  = rad*one_over_h

      if(qq.gt.1) then

         tmq = 2.-qq
         tm2 = tmq*tmq
         tm3 = tm2*tmq
         Wab =  const_ker(1)*one_over_h*one_over_h*tm3
 
         fac = alphad*(-0.5)*one_over_h*one_over_h*one_over_h*tm2 !dW/drad
         frx = fac * drx * one_over_rad
         fry = fac * dry * one_over_rad

      else
	
         q2  = qq*qq
         q3  = q2*qq

         Wab = const_ker(2)*one_over_h*one_over_h*
     + ( 1. - 1.5*q2 + 0.75*q3 )

         fac = const_ker(2)*one_over_h*one_over_h*one_over_h*
     + ( - 3.*qq + 3.*0.75*q2 ) !dW/drad
         frx = fac * drx * one_over_rad
         fry = fac * dry * one_over_rad
		  
      endif

      return 

      end
