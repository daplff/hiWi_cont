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
c recount all particles ! it is called every 100 timesteps     
c      
      subroutine recount
      
      use global_2D
      
      real xp_n(npar),yp_n(npar),up_n(npar),vp_n(npar),uo_n(npar),
     + vo_n(npar),pm_n(npar),h_var_n(npar),rhop_n(npar),dw_n(npar),
     + alphap_n(npar),cs_n(npar),
     + h_var0_n(npar),dw0_n(npar),ar_n(npar), pm0_n(npar)
      integer iflag_n(npar)
      
      integer i, k
      
c      
c --- fluid particles ---
c
      
      k=0
      do i=1,np
         if (iflag(i).ge.1) then
         
             k=k+1
             
             xp_n(k)    =xp(i)
             yp_n(k)    =yp(i)
             up_n(k)    =up(i)
             vp_n(k)    =vp(i)
             uo_n(k)    =uo(i)
             vo_n(k)    =vo(i)
             pm_n(k)    =pm(i)
             h_var_n(k) =h_var(i)
             h_var0_n(k)=h_var0(i)
             rhop_n(k)  =rhop(i)
             dw_n(k)    =dw(i)
             dw0_n(k)=dw0(i)
             alphap_n(k)=alphap(i)
             cs_n(k)    =cs(i)
             ar_n(k)=ar(i)
             iflag_n(k)=iflag(i)
             pm0_n(k)=pm0(i)
             
c         else
         
c              write(*,*) 'fluid particle cancelled:'
c              write(*,*) i,xp(i), up(i), iflag(i)
         
         end if
      enddo
      
      np=k
      
      do i=1,np
      
         xp(i)=xp_n(i)
         yp(i)=yp_n(i)
         up(i)=up_n(i)
         vp(i)=vp_n(i)
         uo(i)=uo_n(i)
         vo(i)=vo_n(i)
         pm(i)=pm_n(i)
         h_var(i)=h_var_n(i)
         h_var0(i)=h_var0_n(i)
         rhop(i)=rhop_n(i)
         dw(i)=dw_n(i)
         dw0(i)=dw0_n(i)
         alphap(i)=alphap_n(i)
         cs(i)=cs_n(i)
         iflag(i)=iflag_n(i)
         pm0(i)=pm0_n(i)
         ar(i)=ar_n(i)
            
      enddo

c      
c --- open bc particles ---
c           
      
c      k=0
c      do i=1,np_ob
c         if (iflag_ob(i).ge.1) then
         
c             k=k+1
             
c             xp_n(k)    =xp_ob(i)
c             yp_n(k)    =yp_ob(i)
c             up_n(k)    =up_ob(i)
c             vp_n(k)    =vp_ob(i)
c             uo_n(k)    =uo_ob(i)
c             vo_n(k)    =vo_ob(i)
c             pm_n(k)    =pm_ob(i)
c             h_var_n(k) =h_var_ob(i)
c             rhop_n(k)  =rhop_ob(i)
c             dw_n(k)    =dw_ob(i)
c             alphap_n(k)=alphap_ob(i)
c             cs_n(k)    =cs_ob(i)
c             alphapo_n(k)=alphapo_ob(i)
c             dwo_n(k)    =dwo_ob(i)
c             dw0_n(k)    =dw0_ob(i)
c             iflag_n(k)=iflag_ob(i)
             
c          else
          
c              write(*,*) 'open boundary condition particle cancelled:'
c              write(*,*) i,xp_ob(i), up_ob(i), iflag_ob(i)
         
c         end if
c      enddo
      
c      np_ob=k
      
c      do i=1,np_ob
      
c         xp_ob(i)=xp_n(i)
c         yp_ob(i)=yp_n(i)
c         up_ob(i)=up_n(i)
c         vp_ob(i)=vp_n(i)
c         uo_ob(i)=uo_n(i)
c         vo_ob(i)=vo_n(i)
c         pm_ob(i)=pm_n(i)
c         h_var_ob(i)=h_var_n(i)
c         rhop_ob(i)=rhop_n(i)
c         dw_ob(i)=dw_n(i)
c         alphap_ob(i)=alphap_n(i)
c         cs_ob(i)=cs_n(i)
c         alphapo_ob(i)=alphapo_n(i)
c         dwo_ob(i)=dwo_n(i)
c         dw0_ob(i)    =dw0_n(i)
c         iflag_ob(i)=iflag_n(i)
            
c      enddo
      
         
      end
