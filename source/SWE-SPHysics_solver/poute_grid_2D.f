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

c
c    export data in ESRI ascii grd file
c
         
      subroutine poute_grid(nf)
      
      use global_2D
      
      integer nf, i, j, icell, jcell, j1
      real m_dw_grd, m_up_grd, m_vp_grd, xpl, ypl, dxx, dyy
                  
      m_dw_grd=-1.E10
      m_up_grd=-1.E10
      m_vp_grd=-1.E10
      
c      open(11,file='intep_value.txt')
     
      do i=1,nplot_x !1
         do j=1,nplot_y !2
         
          dw_grd(i,j)=0.
          up_grd(i,j)=0.
          vp_grd(i,j)=0.
          sum_grd(i,j)=0.
          dx_fs_grd(i,j)=2*hmax
          !dx_fs_grd(i,j)=0.
          !dy_fs_grd(i,j)=0.
         
c        -- point's coordinate --
         xpl=(i-0.5)*dx_grd
         ypl=(j-0.5)*dy_grd
         
c        -- calculating position of particle i --
         dxx = xpl - xmin                    
         dyy = ypl - ymin

         icell = int( dxx * one_over_2hmax ) + 1
         jcell = int( dyy * one_over_2hmax ) + 1
         
         j1 = icell + (jcell - 1)*ncx
         
         if (icell.lt.ncx) then
             call celij_grid(i,j,j1,j1+1) !EAST
             call celij_grid_ob(i,j,j1,j1+1)
         endif


         if (icell.gt.1) then
             call celij_grid(i,j,j1,j1-1) !WEST
             call celij_grid_ob(i,j,j1,j1-1)
         endif
             
         if (jcell.lt.ncy) then

             call celij_grid(i,j,j1,j1+ncx) !NORTH
             call celij_grid_ob(i,j,j1,j1+ncx)

             if (icell.gt.1) then
                call celij_grid(i,j,j1,j1+ncx-1) !NORTH WEST
                call celij_grid_ob(i,j,j1,j1+ncx-1)
             endif

             if (icell.lt.ncx) then
                 call celij_grid(i,j,j1,j1+ncx+1) !NORTH EAST
                 call celij_grid_ob(i,j,j1,j1+ncx+1)
             endif

         endif
             
         if (jcell.gt.1) then

             call celij_grid(i,j,j1,j1-ncx) !SOUTH
             call celij_grid_ob(i,j,j1,j1-ncx)

             if (icell.gt.1) then
                 call celij_grid(i,j,j1,j1-ncx-1) !SOUTH WEST
                 call celij_grid_ob(i,j,j1,j1-ncx-1)
             endif

             if (icell.lt.ncx) then
                 call celij_grid(i,j,j1,j1-ncx+1) !SOUTH EAST
                 call celij_grid_ob(i,j,j1,j1-ncx+1)
             endif

         endif 
             
         call celij_grid(i,j,j1,j1) !the same cell
         call celij_grid_ob(i,j,j1,j1)
         
         !correct the data
      
c         if ( (sum_grd(i,j).gt.0).and.
c     + ( (abs( dx_fs_grd(i,j) ).lt.1.2*dx_grd)
c     + .or.(abs( dy_fs_grd(i,j) ).lt.1.2*dy_grd) ) ) then

c         if ( (sum_grd(i,j).gt.0).and.
c     + ( dx_fs_grd(i,j) .lt.dx_grd ) ) then

         if  ( sum_grd(i,j).gt.0.5 )  then

            dw_grd(i,j)=dw_grd(i,j)/sum_grd(i,j)
            up_grd(i,j)=up_grd(i,j)/sum_grd(i,j)
            vp_grd(i,j)=vp_grd(i,j)/sum_grd(i,j)
         else
            dw_grd(i,j)=-9999.
            up_grd(i,j)=-9999.
            vp_grd(i,j)=-9999.
         endif
            
            m_dw_grd=max(m_dw_grd,dw_grd(i,j))
            m_up_grd=max(m_up_grd,up_grd(i,j))
            m_vp_grd=max(m_vp_grd,vp_grd(i,j))
            
34      format(3f18.4) 
    
         enddo !2
      enddo    !1
      
c      !write the data in GRD_d
      
      write(nf,*) 'DSAA'
      write(nf,*) nplot_x, nplot_y
      write(nf,*)  0.0, vlx
      write(nf,*)  0.0, vly
      write(nf,*)  0.0, m_dw_grd
      
c     !write the data in GRD_u
     
      write(nf+1,*) 'DSAA'
      write(nf+1,*) nplot_x, nplot_y
      write(nf+1,*)  0.0, vlx
      write(nf+1,*)  0.0, vly
      write(nf+1,*)  0.0, m_up_grd

c      !write the data in GRD_v
      
      write(nf+2,*) 'DSAA'
      write(nf+2,*) nplot_x, nplot_y
      write(nf+2,*)  0.0, vlx
      write(nf+2,*)  0.0, vly
      write(nf+2,*)  0.0, m_vp_grd
    
      do j=1,nplot_y
         if (nplot_x.le.100) then
             write(nf,21) (dw_grd(i,j), i=1,nplot_x)
             write(nf+1,21) (up_grd(i,j), i=1,nplot_x)
             write(nf+2,21) (vp_grd(i,j), i=1,nplot_x)
         elseif (nplot_x.gt.100.and.nplot_x.le.1000) then
             write(nf,22) (dw_grd(i,j), i=1,nplot_x)
             write(nf+1,22) (up_grd(i,j), i=1,nplot_x)
             write(nf+2,22) (vp_grd(i,j), i=1,nplot_x)
         elseif (nplot_x.gt.1000.and.nplot_x.le.10000) then
             write(nf,23) (dw_grd(i,j), i=1,nplot_x)
             write(nf+1,23) (up_grd(i,j), i=1,nplot_x)
             write(nf+2,23) (vp_grd(i,j), i=1,nplot_x)
         elseif (nplot_x.gt.10000.and.nplot_x.le.100000) then
             write(nf,24) (dw_grd(i,j), i=1,nplot_x)
             write(nf+1,24) (up_grd(i,j), i=1,nplot_x)
             write(nf+2,24) (vp_grd(i,j), i=1,nplot_x)
         endif
      enddo

21    format(100f18.4)
22    format(1000f18.4)      
23    format(10000f18.4)      
24    format(100000f18.4)      
      return
      
      end subroutine

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^      
  
      subroutine celij_grid(i,j,j1,j2) !SOUTH
      
      use global_2D
      
      real xpl, ypl, drx, dry, rr2, rad
      integer i, j, j1, j2, jj, jp 
      
           do jj=1,nc(j2,2) !2
              jp = ibox(j2,2,jj)
              
              xpl=(i-0.5)*dx_grd
              ypl=(j-0.5)*dy_grd

              drx = xpl - xp(jp)
              dry = ypl - yp(jp)
              rr2 = drx*drx + dry*dry

	    if ( rr2.lt.( 4*h_var(jp)*h_var(jp) ).and.
     + rr2.ge.1.E-18 ) then

	       rad=sqrt(rr2)
	       call kernel(alphad, const_ker,drx,dry,rad,h_var(jp),
     +     frx,fry,fac,Wab) 

               !etat=dw(jp)+h_t(jp)
               dw_grd(i,j)=dw_grd(i,j)+(pm(jp)/rho0)*Wab
               up_grd(i,j)=up_grd(i,j)+up(jp)*(pm(jp)/rhop(jp))*Wab
               vp_grd(i,j)=vp_grd(i,j)+vp(jp)*(pm(jp)/rhop(jp))*Wab
            
               !correction
               sum_grd(i,j)  = sum_grd(i,j) + (pm(jp)/rhop(jp))*Wab

               !dx_fs_grd(i,j)=min( dx_fs_grd(i,j), rad )
               !dx_fs_grd(i,j)= dx_fs_grd(i,j)+drx
               !dy_fs_grd(i,j)= dy_fs_grd(i,j)+dry

c                if (j.eq.5 .and. i.eq.10) then 
c                    write(*,*) i, j, xpl, ypl, drx, dry, jp, xp(jp), 
c     + yp(jp), (pm(jp)/rho0), Wab,  dx_fs_grd(i,j), dy_fs_grd(i,j)
c                endif 

	      endif !3
         enddo   !2
      
         return
      
      end subroutine

!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^         
  
      subroutine celij_grid_ob(i,j,j1,j2) !SOUTH
      
      use global_2D
      
      real xpl, ypl, drx, dry, rr2, rad
      integer i, j, j1, j2, jj, jp 
      
           do jj=1,nc(j2,3) !2
              jp = ibox(j2,3,jj)
              
              xpl=(i-0.5)*dx_grd
              ypl=(j-0.5)*dy_grd

              drx = xpl - xp_ob(jp)
              dry = ypl - yp_ob(jp)
              rr2 = drx*drx + dry*dry

	    if ( rr2.lt.( 4*h_var_ob(jp)*h_var_ob(jp) ).and.
     + rr2.ge.1.E-18 ) then

	       rad=sqrt(rr2)
	       call kernel(alphad, const_ker,drx,dry,rad,h_var_ob(jp),
     +     frx,fry,fac,Wab) 

               !etat=dw(jp)+h_t(jp)
               dw_grd(i,j)=dw_grd(i,j)+
     + (pm_ob(jp)/rho0)*Wab
               up_grd(i,j)=up_grd(i,j)+
     + up(jp)*(pm_ob(jp)/rhop_ob(jp))*Wab
               vp_grd(i,j)=vp_grd(i,j)+
     + vp(jp)*(pm_ob(jp)/rhop_ob(jp))*Wab
            
               !correction
               sum_grd(i,j)  = sum_grd(i,j) + 
     + (pm_ob(jp)/rhop_ob(jp))*Wab

	      endif !3
         enddo   !2
      
         return
      
      end subroutine

      
      
      
      
