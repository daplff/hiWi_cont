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

      subroutine poute(nf)
      use global_2D
      
      integer nf, i

      do i=1,np
         if (iflag(i).eq.1) then
         write(nf+1,100) xp(i), yp(i), up(i), vp(i), dw(i)+h_t(i),
     +    pm(i)/rhop(i), h_var(i), iflag(i)
         else
         write(nf+1,100) xp(i), yp(i), up(i), vp(i), dw(i)+h_t(i),
     +    pm(i), h_var(i), iflag(i)
         endif
      enddo

100   format(7e16.8,i8)

c     -------------------------------------

       write(*,*) 'In poute_1D, itime = ',itime
       write(*,*) 'time, dt  ',time,dt,'  ok'

       open(19,file='DT',status='old',POSITION='append')
         write(19,*) time, dt_new
       close(19)

      return 
      end

