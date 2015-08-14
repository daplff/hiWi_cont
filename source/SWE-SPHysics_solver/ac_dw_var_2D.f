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

c    variable smoothing length - density calculation [ see Rodriguez-Paz and Bonet C.S. 2005] 
c   

      subroutine ac_dw
c
      use global_2D
      
      real adh, dxx, dyy, area_min_new,  hmax_def, Res
      integer i_hmaxdef, ncou_max, i, icell, jcell, j1, ncou, iter_fl
    
      hmax_def=0
      area_max=0
      area_min_new=1.E18
      i_hmaxdef=0
      ncou_max=0
      
      do i=1,np !1

         if (mod(itime,i_dw_iter).eq.0) then
             Res_adm(i)=tol+1
         else
             Res_adm(i)=0
         endif

         if (iflag(i).eq.1) then !2

c        -- First guess for h_var(i) and rhop(i) --
c            delta= ( (dm*dt)/alphapo(i) )*ar(i) !alphapo(i) is alpha at time n
c            rhop(i) = ( rho0*dwo(i) )*exp(delta) !first guess for rhop(i)
c            h_var(i) = h_var0(i)*( rho0*dw0(i)/rhop(i) )**(1/dm)
            
            rhop(i)=rhop(i)+dt*rhop(i)*ar(i)
            h_var(i)=h_var(i)-(dt/dm)*h_var(i)*ar(i)

c      -- check hmax and re-assign variables if (h(i) > hmax) --
            if (h_var(i)>hmax) then
               call grid_h_var(i)
            end if
               
c        -- calculating position of particle i --
            dxx = xp(i) - xmin                    
            dyy = yp(i) - ymin

            icell = int( dxx * one_over_2hmax ) + 1
            jcell = int( dyy * one_over_2hmax ) + 1
            
            ! j1 is the linear cell position in the matrix of 2h cells
            j1 = icell + (jcell - 1)*ncx 

         ncou=0
         iter_fl=1
         do while ( (Res_adm(i) > tol).and.(ncou.le.i_max_iter) ) !3

             ncou=ncou+1
             if (ncou.gt.ncou_max) ncou_max=ncou

c        -- Zeroing Variables --
             sum_wab(i)=0
             rhop_sum=0 !just for debug
             alphap(i)=0       
    
c        -- calculating rhop_sum and alpha(i)

         !-- fluid particles and open bc particles
             
             if (icell.lt.ncx) then
                 call celij_dw(i,j1,j1+1) !EAST
                 call celij_dw_ob(i,j1,j1+1)
                 call celij_dw_vir(i,j1,j1+1) !EAST
             endif

             if (icell.gt.1) then
                 call celij_dw(i,j1,j1-1) !WEST
                 call celij_dw_ob(i,j1,j1-1)
                 call celij_dw_vir(i,j1,j1-1) !WEST
             endif
             
             if (jcell.lt.ncy) then
                call celij_dw(i,j1,j1+ncx) !NORTH
                call celij_dw_ob(i,j1,j1+ncx)
                call celij_dw_vir(i,j1,j1+ncx) !NORTH
 
                if (icell.gt.1) then
                    call celij_dw(i,j1,j1+ncx-1) !NORTH WEST
                    call celij_dw_ob(i,j1,j1+ncx-1)
                    call celij_dw_vir(i,j1,j1+ncx-1) !NORTH WEST
                endif
                
                if (icell.lt.ncx) then
                    call celij_dw(i,j1,j1+ncx+1) !NORTH EAST
                    call celij_dw_ob(i,j1,j1+ncx+1)
                    call celij_dw_vir(i,j1,j1+ncx+1) !NORTH EAST
                endif
                
             endif
             
             if (jcell.gt.1) then
                call celij_dw(i,j1,j1-ncx) !SOUTH
                call celij_dw_ob(i,j1,j1-ncx)
                call celij_dw_vir(i,j1,j1-ncx) !SOUTH
                
                if (icell.gt.1) then
                    call celij_dw(i,j1,j1-ncx-1) !SOUTH WEST
                    call celij_dw_ob(i,j1,j1-ncx-1)
                    call celij_dw_vir(i,j1,j1-ncx-1) !SOUTH WEST
                endif
                if (icell.lt.ncx) then
                    call celij_dw(i,j1,j1-ncx+1) !SOUTH EAST
                    call celij_dw_ob(i,j1,j1-ncx+1)
                    call celij_dw_vir(i,j1,j1-ncx+1) !SOUTH EAST
                endif
             endif  

             call self_dw(i,j1)
             call celij_dw_ob(i,j1,j1)             
             call celij_dw_vir(i,j1,j1) !the same cell

            !-- self contribution -- 
             adh= (alphad*2)/( 3*h_var(i)*h_var(i) )   ! cubic kernel
             rhop_sum=rhop_sum+pm(i)*adh
             sum_wab(i)  = sum_wab(i)  + adh*pm(i)/( dw(i)*rho0 ) !just for debug 
               
c     -- END: calculating rhop_sum and alphap(i)

            Res=rhop(i)-rhop_sum !residual
            Res_adm(i) = abs(Res/rhop(i))       !adimensional residual
            
            !density k+1     
               rhop(i)=rhop(i)*( 1 -( Res*dm/( Res*dm + alphap(i)))) !density at iteration k+1
               
               if (rhop(i).le.0 .or. iter_fl.eq.0) then
                 rhop(i)=rhop_sum
                 h_var(i) = max(h_var0(i),
     +            h_var0(i)*( rho0*dw0(i)/rhop(i) )**(1/dm))
c                write(*,*) 'warning particle i', i, 'rhop(i)=',rhop(i),
c     + 'h_var/h_var0= ',h_var(i)/h_var0(i),
c     +'(x,y)= ',xp(i), yp(i),'n iter=', ncou
                 iter_fl=0
               else
                 h_var(i) = h_var0(i)*( rho0*dw0(i)/rhop(i) )**(1./dm)
               endif

!just for debugging
c               if (i.eq.1) then
c                 write(*,*) i, rhop(i), rhop_sum, h_var(i), Res_adm(i), 
c     + dw0(i), dm, h_var0(i), alphap(i)
c               endif
             
c      -- check hmax and re-assign variables if (h(i) > hmax) --
            if (h_var(i)>hmax) then
c              write(*,*) 'i=',i,' hmax =', h_var(i)
               call grid_h_var(i)
               
c        -- calculating position of particle i --
               dxx = xp(i) - xmin                       
               dyy = yp(i) - ymin
               
            !variable smoothing length
               icell = int( dxx * one_over_2hmax ) + 1
               jcell = int( dyy * one_over_2hmax ) + 1
            ! j1 is the linear cell position in the matrix of 2h cells
               j1 = icell + (jcell - 1)*ncx 
            end if

         end do !3

c     -- update water depth and celerity --
         dw(i)=rhop(i)/rho0
         cs(i)=sqrt(grav*dw(i))  
         Vol(i)=pm(i)/rhop(i) 
 
         if ( ( pm(i)/(rho0*dw(i)) ).gt.area_max )  then
               area_max=pm(i)/(rho0*dw(i))
               !just for debug
c               write(*,*) area_max
         endif
         
         if ( ( pm(i)/(rho0*dw(i)) ).lt.area_min_new )  then
               area_min_new=pm(i)/(rhop(i))
         endif

         if (h_var(i).gt.hmax_def.and.alphap(i).gt.0) then        !find the particle with hmax
            i_hmaxdef=i
            hmax_def=h_var(i)
         endif
         
      endif !2
      enddo !1
      
      write(*,*) 'max iteration number is', ncou_max
      
c      -- check hmax and re-assign particles --
         if (i_hmaxdef.gt.0) then
             area_min=area_min_new
             n_assign=n_assign+1
             call grid_h_var(i_hmaxdef)
         endif

         call ac_corr
         call ac_alpha

       close(88)

      end
