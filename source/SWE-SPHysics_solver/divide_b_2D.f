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
       
c     ist and ien are the starting and ending values of the positions in the linear
c     array ip (corresponding to a list of particle numbers) for the particles 
c     in cell ii
c

       
      subroutine divide_b
      use global_2D
      
      integer nplink, i, iallo, icell, jcell, ii, k
      
      real dxx, dyy 

!      
! --- allocate the memory
!
       write(*,*) Vol_b_min, hsm_b_max
       nplink=max( int( (4*hsm_b_max*hsm_b_max)/Vol_b_min)+1,100 )
       allocate( iboxb( nct_b ,nplink), STAT=iallo )
       write(*,*) nct_b, nplink
       
       if (iallo .ne. 0) then
             write(*,*) 'nplink= ',nplink
             write(*,*) 'nct_b= ', nct_b
             STOP 'ibox allocation error'
       endif
       
      allocate( ncb( nct_b ), STAT=iallo )
      
      if (iallo .ne. 0) then
             write(*,*) ' ncb= ',nplink
             write(*,*) 'nct_b= ', nct_b
             STOP 'ncb allocation error'
      endif

       !zeroing the variables
       do i=1,nct_b
          ncb(i)  = 0
          iboxb(i,1:nplink)  = 0
       enddo

       do k=1,np_b
         	
           dxx = xb(k) - xminb
           dyy = yb(k) - yminb

           !RV: variable smoothing length
           icell = int( dxx*one_over_2hsm_b ) + 1
           jcell = int( dyy*one_over_2hsm_b ) + 1
       
           ! ii is the linear cell position in the matrix of 2h cells
           ii    = icell + (jcell - 1)*ncx_b
                                                        
           ! nc is the number of particles in cell ii
           ncb(ii) = ncb(ii)+1	
                                      
          if(ncb(ii).gt.nplink_max)then
           print*
           print*,'ERROR in divide_b_2D.f'
           print*,'nc(ii,kind_p) >= nplink_max'
           print*,'itime',itime
           print*,'nc(ii)',ncb(ii)
           print*,'nplink_max',nplink_max
           print*,'k',k
           print*,'xp(k) yp(k)',xb(k),yb(k)
           print*,'icell,  jcell',icell,jcell
           print*,'Box ii ',ii
           stop
          end if
          
          iboxb(ii,ncb(ii))=k  !Tells us that particle with array location k (i.e. xp(k) )  
                                         !is in box ii which, so far, contains "nc(ii,mkind)" particles

       enddo 

c          write(*,*) 'number of particle for cells'
c          write(*,*) (nc(ii,kind_p), ii=1,ncx)
       
       return
       end

