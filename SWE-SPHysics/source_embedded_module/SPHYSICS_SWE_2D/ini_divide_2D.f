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


      subroutine ini_divide(kind_p)
      use global_2D
      
      integer kind_p, i, iallo
      real den, num

      nc(:,kind_p)  = 0
       
       if (kind_p.eq.2) then

           !den=sqrt(max(area_min,hmax0*hmax0*0.01))
           !num=min(3.*hmax,hmax0*20)
           den=sqrt(area_min)
           num=3.*hmax
           nplinkmax_var=n0*(int(num/den)+1 )**2
           nplinkmax_var=max(nplinkmax_var,200)
           nplinkmax_var=min( nplinkmax_var,20000)

           if ( ALLOCATED(ibox) ) then
                deallocate(ibox, STAT=iallo)
                if (iallo .ne. 0) STOP 'ibox deallocation error'
           endif
           
           !allocate ibox memory
           allocate(ibox(nct,3,nplinkmax_var), STAT=iallo)
           if (iallo .ne. 0) then
             write(*,*) 'nplinkmax_var=',nplinkmax_var
             write(*,*) 'n0=',n0
             write(*,*) 'hmax=',hmax
             write(*,*) 'area_min=', area_min
             STOP 'ibox allocation error'
           endif

       endif
       
      return

      end
