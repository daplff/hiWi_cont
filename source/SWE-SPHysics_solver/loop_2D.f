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
      subroutine loop
      use global_2D

      integer nloop, lx, ly, j1, ii, i
      real xj1, yj1
       
       nloop=0
       
       do ly=1,ncy !1
       do lx=1,ncx !2
         j1 = lx + (ly-1)*ncx 
         iloop(j1)=0 !iloop(j1)=0
         
         do ii=1,nc(j1,2) !3
            i = ibox(j1,2,ii) !particle number
            
            if (Res_adm(i).gt.tol) then !4
                
                iloop(j1)=2
                
                nloop=nloop+1

         
                if (lx.gt.1) then
                    iloop(j1-1)=max( 1, iloop(j1-1) )   
                endif
                
                if (lx.lt.ncx) then
                    iloop(j1+1)=max( 1, iloop(j1+1) )
                endif
                
                if (ly.gt.1) then
                    iloop(j1-ncx)=max( 1, iloop(j1-ncx) )
                    if (lx.gt.1) then
                        iloop(j1-ncx-1)=max( 1, iloop(j1-ncx-1) )
                    endif
                    if (lx.lt.ncx) then
                        iloop(j1-ncx+1)=max( 1, iloop(j1-ncx+1) )
                    endif
                endif
                
                if (ly.lt.ncy) then
                    iloop(j1+ncx)=max( 1, iloop(j1+ncx) )
                    if (lx.gt.1) then
                        iloop(j1+ncx-1)=max( 1, iloop(j1+ncx-1) )
                    endif
                    if (lx.lt.ncx) then
                        iloop(j1+ncx+1)=max( 1, iloop(j1+ncx+1) )
                    endif
                endif
       
            end if !4
            
         enddo !3
       enddo !2
       enddo !1
       
       if (idebug.eq.1)write(*,*) 'nloop=',nloop
       
       end subroutine
