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
       
      subroutine refinement
       
      use global_2D

      integer i, j, icell_ref, jcell_ref, j1_ref, i_nei, iref_i, kk, 
     + kk_r
      real angle_v, x_cand, y_cand, xx, yy, pippo
            
      !allocate ipvic memory
c      allocate(ipvic(nplinkmax_var), STAT=iallo)
c      if (iallo .ne. 0) then
c            STOP 'allocation error of ipvic'
c      endif
        
      !inizialize to zero
      li_ref(1:npar)=0
      irli(1:npar,1:7)=0
       
       kk=np
       kk_r=0
       
       do i=1,np !1
    
	  if ( xp(i).lt.xmax_container.and.xp(i).gt.xmin_container
     +	  .and.yp(i).lt.ymax_container.and.yp(i).gt.ymin_container )
     +  then !2
    
          !position of particle i in refinement cell
           xx = xp(i) - xmin_ref                   
           yy = yp(i) - ymin_ref
                    
           icell_ref = int( xx / dxx_ref )+1
           jcell_ref = int( yy / dyy_ref )+1
          ! j1 is the linear cell position in the matrix of 2h cells
           j1_ref = icell_ref + (jcell_ref - 1)*ncx_ref   
      
          if ( (( pm(i)/(rho0*dw(i)) ).gt.area_lim(j1_ref)).and.
     + (iflag(i).eq.1).and.dw(i).gt.dw_min_ref) then !3
          
             !update list of refined particles
              kk_r=kk_r+1
              li_ref(kk_r)=i

             if ((abs(vp(i)).gt.1.e-3) .or. (abs(up(i)).gt.1.e-3)) then             
                    angle_v=atan2(vp(i),up(i))
             else   
                    angle_v=0
             endif
                         
             do j=1,6 !3

c -- candidate position for refined particles
                x_cand=xp(i)+ref_p*h_var(i)*cos((j-1)*(pi/3)+angle_V)
                y_cand=yp(i)+ref_p*h_var(i)*sin((j-1)*(pi/3)+angle_v)
                
                iref_i=1
                i_nei=1

                kk=kk+1
                irli(i,j)=kk !list of daughter particles generated splitting particle i
                iflag(kk)=1
                xp(kk)=x_cand
                yp(kk)=y_cand
                up(kk)=up(i) !useless
                vp(kk)=vp(i) !useless
                h_var(kk)=h_var(i)*ref_h !useless
                dw(kk)=dw(i)    !useless
                rhop(kk)=rhop(i) !useless
                cs(kk)=cs(i) !useless
                pm(kk)=pm(i)*0.136882287617319   
                pm0(kk)=pm(i)           
                alphap(kk)=alphap(i) !useless
                h_var0(kk)=ref_h*h_var0(i)
                dw0(kk)=dw0(i)
                ar(kk)=ar(i)
                uo(kk)=uo(i)
                vo(kk)=vo(i)
                
             enddo !3
                h_var(i)=h_var(i)*ref_h
                pm(i)=pm(i)*0.178705766141917 
                h_var0(i)=ref_h*h_var0(i)
                dw0(i)=dw0(i)
                irli(i,7)=i !list of refined particles connected to particle i

          endif  !3
          endif !2
       enddo     !1
       
       ntotref=kk_r !total number of refined particles
       np=kk
       
       if (np .ge. npar ) then
         write(*,*) 'Number of particles exceeds npar in common!'
         stop
       endif
       
       return

       end
                  
