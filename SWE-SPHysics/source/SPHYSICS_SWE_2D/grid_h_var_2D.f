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

      subroutine grid_h_var(i)
      use global_2D
      
      integer i
      
               hmax=h_var(i)
               one_over_2hmax=1./(2.*hmax)
               
               xmin=xlim_left-2.1*hmax
               xmax=xlim_right+2.1*hmax
               
               ymin=ylim_left-2.1*hmax
               ymax=ylim_right+2.1*hmax    
                          
               ncx = int( (xmax-xmin)*one_over_2hmax ) + 1
               ncy = int( (ymax-ymin)*one_over_2hmax ) + 1
               nct = ncx*ncy
c               write(*,*) 'grid_h_var'
c               write(*,*) xmin, xmax, ymin, ymax, hmax, ncx, ncy, nct
               
               call ini_divide(2)
               call divide(1,np,2)
               call ini_divide(1)
               call divide_vir(1,npv,1)
               call ini_divide(3)
               call divide_ob(1,np_ob,3)
               
               return
      end
