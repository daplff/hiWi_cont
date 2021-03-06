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

      subroutine ac
c
      use global_2D
      
      integer i, j1, lx, ly, lx2, ly2, icell, jcell
      real dxx, dyy
      
c      open(33,file='debug_balsara_vir.txt')

      do i=nstart,np

c        -- Zeroing Variables --         
          ax(i) = 0.
          ay(i) = 0.
          sum_wab(i) = 0. 

          ar(i) = 0.
                    
      enddo
      
c        -- fluid particles --        
       do ly=1,ncy
       do lx=1,ncx
         j1 = lx + (ly-1)*ncx
         
         if(nc(j1,2).gt.0) then
c                            ! if the cell is not empty, then
c                            ! loop over it and over neighboring
c                            ! cells


c          -- Cells in the same XY sheet -
           lx2 = lx+1
           if(lx2.le.ncx)then
             call celij(j1,j1+1)     !East
           endif      
      
           ly2=ly+1
           if(ly2.le.ncy)then
       
             !- Same row -
             call celij(j1,j1+ncx)   !North
       
             lx2=lx-1
             if(lx2.ge.1) call celij(j1,j1+ncx-1)   !North-West
       
             lx2=lx+1
             if(lx2.le.ncx) call celij(j1,j1+ncx+1)   !North-East                 
       
           endif    !End of:   if(ly2.le.ncy)then
         endif    !End of:  if(nc(j1,2).gt.0) then
       
       enddo
       enddo
       
       do j1=1,nct
           if(nc(j1,2).gt.0)
     +		call self(j1)
       enddo
             
c          --- virtual boundary particles ---  
c          --- open boundary particles --- 

c just for debug
c      open(55,file='virtual_part.txt',status='replace') 

       do i=nstart,np !1

          if (iflag(i).eq.1) then
       
          !-- calculating position of particle i --
          dxx = xp(i) - xmin                    
          dyy = yp(i) - ymin

          icell = int( dxx * one_over_2hmax ) + 1
          jcell = int( dyy * one_over_2hmax ) + 1
          
          ! j1 is the linear cell position in the matrix of 2h cells
          j1 = icell + (jcell - 1)*ncx 
          
          if (icell.lt.ncx) then
              call celij_vir(i,j1,j1+1) !EAST
              call celij_ob(i,j1,j1+1) 
          endif

          if (icell.gt.1) then 
              call celij_vir(i,j1,j1-1) !WEST
              call celij_ob(i,j1,j1-1)
          endif
             
          if (jcell.lt.ncy) then
              call celij_vir(i,j1,j1+ncx) !NORTH
              call celij_ob(i,j1,j1+ncx)
              if (icell.gt.1) then
                  call celij_vir(i,j1,j1+ncx-1) !NORTH WEST
                  call celij_ob(i,j1,j1+ncx-1)
              endif
              if (icell.lt.ncx) then
                  call celij_vir(i,j1,j1+ncx+1) !NORTH EAST
                  call celij_ob(i,j1,j1+ncx+1)
              endif
          endif
             
          if (jcell.gt.1) then
             call celij_vir(i,j1,j1-ncx) !SOUTH
             call celij_ob(i,j1,j1-ncx)
             if (icell.gt.1) then
                 call celij_vir(i,j1,j1-ncx-1) !SOUTH WEST
                 call celij_ob(i,j1,j1-ncx-1)
             endif
             if (icell.lt.ncx) then
                 call celij_vir(i,j1,j1-ncx+1) !SOUTH EAST
                 call celij_ob(i,j1,j1-ncx+1)
             endif
          endif  

             call celij_vir(i,j1,j1)
             call celij_ob(i,j1,j1)

       endif

       enddo 

c       close(33)
             
       return
       end

