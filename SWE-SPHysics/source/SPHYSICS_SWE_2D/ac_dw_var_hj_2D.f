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

      subroutine ac_dw
c
      use global_2D

      real adh, area_min_new, dxx, dyy, hmax_def, Res, res_max, pippo
      integer i, i_hmaxdef, i_hmaxdef_gu, icell, jcell, j1, lx, ly, lx2, 
     + ly2, ncou, ii


      n_assign=0
      
      if (mod(itime,i_dw_iter).eq.0) then
             res_max=tol+1
      else
             res_max=0
      endif
      
c        -- First guess for h_var(i) and rhop(i) --
      hmax_def=0
      do i=1,np !2
         if (iflag(i).eq.1) then !3

c -- old guess for density

c             delta= ( (dm*dt)/alphapo(i) )*ar(i) !alphapo(i) is alpha at time n
c             rhop(i) = ( rho0*dwo(i) )*exp(delta) !first guess for rhop(i)
c             h_var(i) = h_var0(i)*( rho0*dw0(i)/rhop(i) )**(1/dm)

c -- new guess for density

              rhop(i)=rhop(i)+dt*rhop(i)*ar(i)
              h_var(i)=h_var(i)-(dt/dm)*h_var(i)*ar(i)

c to be removed
c              h_var(i) = min(240.,h_var(i))    

c      -- check hmax and re-assign variables if (h(i) > hmax) --
             if (h_var(i).gt.hmax_def) then
                 i_hmaxdef=i
                 hmax_def=h_var(i) 
             end if
             Res_adm(i) = tol+1
         endif
      enddo

      call grid_h_var(i_hmaxdef)

      call loop
      i_hmaxdef_gu=i_hmaxdef
      
      
      hmax_def=0
      
      ncou=0
      
      do while ( (res_max > tol).and.(ncou.le.i_max_iter) )  !1
                
      ncou=ncou+1
      res_max=0
      area_max=0
      area_min_new=1.E18
      
c --- bc and open bc contribution ---      

      do i=1,np !2

         if (iflag(i).eq.1) then !3
               
c        -- calculating position of particle i --
            dxx = xp(i) - xmin                    
            dyy = yp(i) - ymin
            icell = int( dxx * one_over_2hmax ) + 1
            jcell = int( dyy * one_over_2hmax ) + 1
            ! j1 is the linear cell position in the matrix of 2h cells
            j1 = icell + (jcell - 1)*ncx 
            
            if (iloop(j1).gt.0) then !4 

c        -- Zeroing Variables --
             rhop_sum_j(i)=0 
             alphap(i)=0 
             sum_wab(i)=0   
             rhop_sum=0  
    
c        -- calculating rhop_sum and alpha(i)

c        --- boundary and open particle contribution---              
             if (icell.lt.ncx) then
                 call celij_dw_ob(i,j1,j1+1)
                 call celij_dw_vir(i,j1,j1+1) !EAST
             endif

             if (icell.gt.1) then
                 call celij_dw_ob(i,j1,j1-1)
                 call celij_dw_vir(i,j1,j1-1) !WEST
             endif
             
             if (jcell.lt.ncy) then
                call celij_dw_ob(i,j1,j1+ncx)
                call celij_dw_vir(i,j1,j1+ncx) !NORTH
                if (icell.gt.1) then
                    call celij_dw_ob(i,j1,j1+ncx-1)
                    call celij_dw_vir(i,j1,j1+ncx-1) !NORTH WEST
                endif
                if (icell.lt.ncx) then
                    call celij_dw_ob(i,j1,j1+ncx+1)
                    call celij_dw_vir(i,j1,j1+ncx+1) !NORTH EAST
                endif
             endif
             
             if (jcell.gt.1) then
                call celij_dw_ob(i,j1,j1-ncx)
                call celij_dw_vir(i,j1,j1-ncx) !SOUTH
                if (icell.gt.1) then
                    call celij_dw_ob(i,j1,j1-ncx-1)
                    call celij_dw_vir(i,j1,j1-ncx-1) !SOUTH WEST
                endif
                if (icell.lt.ncx) then
                    call celij_dw_ob(i,j1,j1-ncx+1)
                    call celij_dw_vir(i,j1,j1-ncx+1) !SOUTH EAST
                endif
             endif  

             call celij_dw_ob(i,j1,j1)           
             call celij_dw_vir(i,j1,j1) !the same cell
             
             rhop_sum_j(i)=rhop_sum
             
             endif !4
             
         endif !3 (iflag)
      end do !2 (loop over all particle)
      
c --- fluid particle contribution ---   
             
      do ly=1,ncy !2
      do lx=1,ncx !3
         j1 = lx + (ly-1)*ncx
         
         if( (nc(j1,2).gt.0).and.iloop(j1).gt.0 ) then !4

c          -- Cells in the same XY sheet -
           lx2 = lx+1
           if(lx2.le.ncx)then
             call celij_dw(j1,j1+1)     !East
           endif      
      
           ly2=ly+1
           if(ly2.le.ncy)then
       
             !- Same row -
             call celij_dw(j1,j1+ncx)   !North
       
             lx2=lx-1
             if(lx2.ge.1) call celij_dw(j1,j1+ncx-1)   !North-West
       
             lx2=lx+1
             if(lx2.le.ncx) call celij_dw(j1,j1+ncx+1)   !North-East                 
       
           endif    !End of:   if(ly2.le.ncy)then
           
           lx2 = lx-1
           if(lx2.ge.1)then
             call celij_dw(j1,j1-1)     !west
           endif      
      
           ly2=ly-1
           if(ly2.ge.1)then
       
             !- Same row -
             call celij_dw(j1,j1-ncx)   !south
       
             lx2=lx-1
             if(lx2.ge.1) call celij_dw(j1,j1-ncx-1)   !south-West
       
             lx2=lx+1
             if(lx2.le.ncx) call celij_dw(j1,j1-ncx+1)   !south-East                 
       
           endif    !End of:   if(ly2.le.ncy)then
         
           call self_dw(j1)
           
           do ii=1,nc(j1,2)
           
              i = ibox(j1,2,ii)
       
              !-- self contribution -- 
              adh= (alphad*2)/( 3*h_var(i)*h_var(i) )   ! cubic kernel
              rhop_sum_j(i)=rhop_sum_j(i)+pm(i)*adh
              sum_wab(i)  = sum_wab(i)  + adh*pm(i)/( dw(i)*rho0 ) !just for debug   
              !-- end self contribution --
            
              !residual k - k+1
              Res=rhop(i)-rhop_sum_j(i) !residual
              Res_adm(i) = abs(Res/rhop(i))       !adimensional residual

              !maximum residual
              if (res_max.lt.Res_adm(i)) then
                  res_max=Res_adm(i)
c                  write(*,*) i, xp(i), yp(i), rhop(i), rhop_sum_j(i),  
c     + h_var(i), 'res_max=', res_max
              endif
            
c     -- density k+1     
              rhop(i)=rhop_sum_j(i)

c     -- update water depth --
              dw(i)=rhop(i)/rho0
              
           enddo
           
       endif    !4
       
       enddo !3
       enddo !2 end of loop over fluid particles 
         
c         area_min=area_min_new
         area_min=1.E18

c -- update smoothing length and area_min calculations         
         hmax_def=0
         do i=1,np !2
            if (iflag(i).eq.1) then !3         
                h_var(i) = h_var0(i)*( rho0*dw0(i)/rhop(i) )**(1/dm)  !density k+1 

c to be removed          
c                h_var(i) = min(240.,h_var(i))    

c                if (i.eq.38641) then
c                    write(*,*) i, h_var(i), Res_adm(i), rhop(i), 
c     +sum_wab(i)
c                   pippo=0
c                endif
                                        
                if (h_var(i).gt.hmax_def) then !4
                    i_hmaxdef=i
                    hmax_def=h_var(i)  
                endif !4
                
                if ( ( pm(i)/rhop(i) ).lt.area_min )  then !4
                       area_min=pm(i)/rhop(i)
                endif !4
                
            endif !3
         enddo !2
         call grid_h_var(i_hmaxdef)
         call loop !turn off the cells where the rhop(i) of all particles inside the cell remain constant
        
      enddo !1 (do while maximum residual)
      
      !just for debug
      if (idebug.eq.1) then
      write(*,*) 'iteration completed L2 res=', res_max, 'number of 
     + iteration=',ncou
      endif
      
      do i=1,np !2

!to be removed      
c        if ((alphap(i).eq.0).and.(iflag(i).ne.0)) then
c            alphap(i)=rhop(i)*dm
c             write(*,*) 'WARNING alphap(i)=0'
c             write(*,*) 'i=',i,'xp(i),yp(i)',xp(i), yp(i)
c             write(*,*) 'rhop(i)=',rhop(i), 'h_var(i)=',h_var(i)
c         endif    
      
c     -- update water depth --
          dw(i)=rhop(i)/rho0
          
         !find area_max
         if ( ( pm(i)/(rho0*dw(i)) ).gt.area_max )  then
               area_max=pm(i)/(rho0*dw(i)) !useful for refinement criteria
         endif
         
      enddo
        
c      -- alphap with update h_var for every particles --
         call ac_alpha
         
      end
