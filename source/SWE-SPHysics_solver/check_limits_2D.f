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
        subroutine check_limits
        use global_2D
        
        integer i, k, ncases
        real xp_p, xp_pt, yp_p, yp_pt, distminij

	ncases=0
               
         do i=1,np ! Check if any fluid particle is over the box
            
	   if ( iflag(i).eq.1.and.
     +        (xp(i).gt.xmax.or.xp(i).lt.xmin).or.
     +	      (yp(i).gt.ymax.or.yp(i).lt.ymin).or.
     +         h_var(i).gt.100*h_var0(i) ) then
     
              iflag(i)=0
              write(*,*) 'particle xp,yp out of domain (', 
     +  xp(i),yp(i),')','i=',i
	      ncases=ncases+1
	      xp(i)=xtrash
 	      yp(i)=ytrash
	      up(i)=0.
 	      vp(i)=0.
 	      dw(i)=0.
 	      uo(i)=0.
 	      vo(i)=0.
 	      udot(i)=0.
 	      vdot(i)=0.

            endif   
	  enddo

!--- check if fluid particles are entering in the buffer zone for open boundaries
	
       do  i=1,np !1 loop over each fluid particle
           if (iflag(i).ne.0) then !2
           
	   do k=1,nopen_bou !3 loop over each open bc
	   
	      !transform the coordinate of the point in the relative coord. system 
	      xp_pt=( xp(i)-xpbc(k) )
	      yp_pt=( yp(i)-ypbc(k) )
	      
	      xp_p=+xp_pt*verbc(k,1)+yp_pt*verbc(k,2)
	      yp_p=(-xp_pt*verbc(k,2)+yp_pt*verbc(k,1))*verybc(k)
	      
	      if ( (yp_p.gt.0 .and. yp_p.lt.lbc(k)) 
     +      .and. (xp_p.lt.0) ) then
     
                 call new_ob_flp(i,k) !it generates a new bc particle
                 xp(i)=xtrash
                 yp(i)=ytrash
	         up(i)=0.
	         vp(i)=0.
	         uo(i)=0.
	         vo(i)=0.
	         udot(i)=0.
 	         vdot(i)=0.
	         iflag(i)=0
	      
	      endif
	     
	   enddo !3
	   endif !2
       enddo !1
           	
!--- check if any openbc partile is entering in the domain
       	
       	do i=1,np_ob ! Check bc particles !1
       	   if (iflag_ob(i).gt.0) then     !2
	   
	      k=ibc(i)
	   
	   !transform the coordinate of the point in the relative coord. system
	      xp_pt=( xp_ob(i)-xpbc(k) )
	      yp_pt=( yp_ob(i)-ypbc(k) )
	      
	      xp_p=+xp_pt*verbc(k,1)+yp_pt*verbc(k,2)
	      yp_p=( -xp_pt*verbc(k,2)+yp_pt*verbc(k,1) )*verybc(k)
	   
	   !verify if the bc particle is INSIDE the domain
	      if (  (yp_p.gt.0 .and. yp_p.lt.lbc(k)) !3
     +      .and. (xp_p.gt.0) ) then
     
                  call new_fl_ob(i) !it generates a new fluid particle from the bc particle
                  call new_bcp(k,xp_p,yp_p) !it generates a new bc particle
                  
                  !cancel the old open bc particle
                  xp_ob(i)=xtrash
                  yp_ob(i)=ytrash	      
	          up_ob(i)=0.	
	          vp_ob(i)=0.      
	          uo_ob(i)=0.
	          vo_ob(i)=0.
	          iflag_ob(i)=0
	      
	      !eliminate openbc particles outside the domain
	       elseif ( ( (yp_p.gt.0 .and. yp_p.lt.lbc(k))
     +         .and. xp_p.lt.-2*dxobc(k)).or.
     +        (xp_ob(i).gt.xmax.or.xp_ob(i).lt.xmin.or.
     +         yp_ob(i).gt.ymax.or.yp_ob(i).lt.ymin) )  then
     
                  xp_ob(i)=xtrash
                  yp_ob(i)=ytrash
                  	      
                  up_ob(i)=0.	
	          vp_ob(i)=0.      
	          uo_ob(i)=0.
	          vo_ob(i)=0.
	          iflag_ob(i)=0
     
               endif !3
               
               !verify distance between openbc particle and any other openbc particle
               call open_distmin(distminij,xp_ob(i),yp_ob(i)) 
               
               if (distminij.lt.distmin) then !3
               
               	   write(*,*) 'openbc particle (',xp_ob(i),
     +          	   yp_ob(i),')eliminate because too close', 
     + 'distminij= ',distminij, 'distmin= ', distmin
               
                   xp_ob(i)=xtrash
                   yp_ob(i)=ytrash	       
                   up_ob(i)=0.	
	           vp_ob(i)=0.      
	           uo_ob(i)=0.
	           vo_ob(i)=0.
	           iflag_ob(i)=0
                
               endif !3
               
               
            endif !2
	enddo !1
			
      if (ncases.ne.0) write(*,*) 'Number ',ncases,' time',time 

      return
      
      end
!----------------------------------------------------------------------
!it generates a new bc particles when a fluid particle exit the domain
!----------------------------------------------------------------------
      subroutine new_ob_flp(ifl,iob)
      use global_2D
      
      integer ifl, iob, i
      real distminij
      
      
      !check if there's any openbc particle too close to the candidate particle
      call open_distmin(distminij,xp(ifl),yp(ifl))
      
      if (distminij.gt.distmin) then 
      
         i=np_ob+1
       
         iflag_ob(i)=iflagbc(iob)
         ibc(i)=iob
      
         xp_ob(i)=xp(ifl)
         yp_ob(i)=yp(ifl)
         pm_ob(i)=pm(ifl)
         h_var_ob(i)=2*h_var(ifl)       
         dw_ob(i)=dw(ifl)
         up_ob(i)=up(ifl)
         uo_ob(i)=uo(ifl)
         vp_ob(i)=vp(ifl)
         vo_ob(i)=vo(ifl)
         rhop_ob(i)=dw_ob(i)*rho0
         cs_ob(i)=sqrt(grav*dw_ob(i))
         alphap_ob(i)=alphap(ifl)
         dw0_ob(i)=dw0(ifl)
         h_var0_ob(i)=h_var0(ifl)
       
         np_ob=i

         !just for debug         
c         write(*,*) 'fluid part > openbc (x,y) 
c     + (', xp_ob(i), yp_ob(i), '), distminij=', distminij

      else
         !just for debug         
c         write(*,*) 'fluid particle not transformed in a open bc 
c     +    particle because too close (x,y) (', xp(ifl),yp(ifl), ')'   
         
      endif
       
       return
       
       end
c-----------------------------------------------------------------------
c it generates a new fluid particle from a bc particle
c ibo is the index of the bc particle
c-----------------------------------------------------------------------      
       subroutine new_fl_ob(iob)
       use global_2D
       
       integer iob, icell, jcell, ii
       real dxx, dyy 
       
       np=np+1
       
       xp(np)=xp_ob(iob)
       yp(np)=yp_ob(iob)
       pm(np)=pm_ob(iob)
       pm0(np)=pm_ob(iob)
       h_var(np)=h_var_ob(iob)/2.
       h_var0(np)=h_var0_ob(iob)
       up(np)=up_ob(iob)
       vp(np)=vp_ob(iob) 
       uo(np)=uo_ob(iob)
       vo(np)=vo_ob(iob) 
       dw(np)=dw_ob(iob)
       dw0(np)=dw0_ob(iob)     
       rhop(np)=dw(np)*rho0
       cs(np)=sqrt(grav*dw(np))
       alphap(np)=rhop(np)*dm
       iflag(np)=1
       
       !just for debug
c       write(*,*) xp(np), yp(np), up(np), vp(np), uo(np), vo(np), dw(np)
       
       !it assigns the fluid particle to his cell
       dxx = xp(np) - xmin   
       dyy = yp(np) - ymin
       icell = int( dxx * one_over_2hmax ) + 1
       jcell = int( dyy * one_over_2hmax ) + 1
       
       ! ii is the linear cell position in the matrix of 2h cells
       ii    = icell + (jcell - 1)*ncx 
       
       nc(ii,2) = nc(ii,2)+1	
                                      
       if(nc(ii,2).gt.nplinkmax_var)then
           print*
           print*,'ERROR in new_fl_ob'
           print*,'nc(ii,kind_p) >= nplinkmax_var'
           print*,'itime',itime
           print*,'nc(ii,kind_p)',nc(ii,2)
           print*,'kind_p       ',2
           print*,'nplinkmax_var',nplinkmax_var
           print*,'np',np
           print*,'xp(k),yp(k)',xp(np),',',yp(np)
           print*,'icell',icell,'jcell',jcell
           print*,'Box ii ',ii
           stop
       end if
          
      ibox( ii,2,nc(ii,2) )=np
    
       return
       
       end
       
c-----------------------------------------------------------------------
c it generates a new bc particle when a bc particle becomes a fluid particle
c k which open boundary conditions
c xpos,ypos position of the new fluid particle just entered into the domain in new reference system
c-----------------------------------------------------------------------
            
       subroutine new_bcp(k,xpos,ypos)
       use global_2D
       
       integer k, i
       real xpos, ypos, xp_ob_p, yp_ob_p, xp_cand, yp_cand, dummy
       real distminij
       
       !coordinate in new reference system
       xp_ob_p=xpos-2*dxobc(k)
       yp_ob_p=ypos
       
       !transform coordinate in absolute ref. system
       xp_cand=xpbc(k) + verbc(k,1)*xp_ob_p - verbc(k,2)*yp_ob_p
       yp_cand=ypbc(k) + (verbc(k,2)*xp_ob_p + verbc(k,1)*yp_ob_p)*
     + verybc(k)
       
       !check if there's any openbc particle too close to the candidate particle
       call open_distmin(distminij,xp_cand,yp_cand)
      
       if (distminij.gt.distmin) then 
      
       i=np_ob+1
      
       iflag_ob(i)=iflagbc(k)
       ibc(i)=k
       
       xp_ob(i)=xp_cand
       yp_ob(i)=yp_cand

       h_var_ob(i)=dxobc(k)*coef*2
       call open_bc(i,i,dummy) !< it calculates up_ob, uo_ob and dw_ob using sph interpolation
       
       dw0_ob(i)=dw_ob(i) 
       pm_ob(i)=dw_ob(i)*dxobc(k)*dxobc(k)*rho0   
       h_var0_ob(i)=dxobc(k)*coef
       rhop_ob(i)=dw_ob(i)*rho0
       cs_ob(i)=sqrt(grav*dw_ob(i))
       alphap_ob(i)=rhop_ob(i)*dm
       
       np_ob=i
       
!       write(*,*) 'new bc particles', k,xpos,ypos, xp_ob(i), yp_ob(i), 
!     + up_ob(i), vp_ob(i), dw_ob(i)
     
       else 
       
!       write(*,*) 'open_bc particle not added because part too close'
!       write(*,*) 'distmin/(0.5*dxobc(k))', distmin/(0.5*dxobc(k))
       
       endif
  
       
       return

       end 
          
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

!find the minimum distance between the (xp_cand, yp_cand) and any other openbc particle.
   
       subroutine open_distmin(distminij,xp_cand,yp_cand) 
       use global_2D 
       
       integer j1, icell, jcell
       real xp_cand, yp_cand, distminij, dxx, dyy
       
       distminij=1.E+9
       
c        -- calculating position of particle cand --
           dxx = xp_cand - xmin
           dyy = yp_cand - ymin
           icell = int( dxx*one_over_2hmax ) + 1
           jcell = int( dyy*one_over_2hmax ) + 1
           j1 = icell + (jcell - 1)*ncx 
           
           if (icell.lt.ncx) call celij_dist2(xp_cand,yp_cand,j1,j1+1,
     +      distminij) !EAST

           if (icell.gt.1) call celij_dist2(xp_cand,yp_cand,j1,j1-1,
     +      distminij) !WEST
             
           if (jcell.lt.ncy) then
                call celij_dist2(xp_cand,yp_cand,j1,j1+ncx,distminij) !NORTH
                if (icell.gt.1) call celij_dist2(xp_cand,yp_cand,j1,
     +           j1+ncx-1,distminij) !NORTH WEST
                if (icell.lt.ncx) call celij_dist2(xp_cand,yp_cand,
     +           j1,j1+ncx+1,distminij) !NORTH EAST
           endif
             
           if (jcell.gt.1) then
                call celij_dist2(xp_cand,yp_cand,j1,j1-ncx,distminij) !SOUTH
                if (icell.gt.1) call celij_dist2(xp_cand,yp_cand,j1,
     +           j1-ncx-1,distminij) !SOUTH WEST
                if (icell.lt.ncx) call celij_dist2(xp_cand,yp_cand,j1,
     +           j1-ncx+1,distminij) !SOUTH EAST
           endif  
           
           call celij_dist2(xp_cand,yp_cand,j1,j1,distminij)
           
           
           return
           
       end subroutine  
       
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   
       subroutine celij_dist2(xp_cand,yp_cand,j1,j2,distminij)
       use global_2D
       
       integer j1, j2, jj, j
       real xp_cand, yp_cand, distminij, drx, dry, rr2
      
       if(nc(j2,3).ne.0) then !1
         
          do jj=1,nc(j2,3) !2
             j = ibox(j2,3,jj)

            drx = xp_cand - xp_ob(j)
            dry = yp_cand - yp_ob(j)
            rr2 = drx*drx  + dry*dry

	    if (( rr2.lt.distminij*distminij).and. (rr2.gt.1.E-17).and.
     + 	    iflag_ob(j).gt.1 ) then !5

                distminij=sqrt(rr2)
               
            endif
            
          enddo
          endif
          
          return
 
      end subroutine

