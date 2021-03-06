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

c it update the position of open boundary particles      
            
      subroutine open_bc_pos
      
      use global_2D
      
      integer i
      real uoi_p, voi_p, uoi_o, voi_o 
      
      do i=1,np_ob
      
         !new coordinate system
         uoi_p=+uo_ob(i)*verbc(ibc(i),1)+vo_ob(i)*verbc(ibc(i),2) !velocity normal to the boundary
         voi_p=0 !velocity tangent to the boundary=0
         
         !back to old coordinate system
           uoi_o=uoi_p*verbc(ibc(i),1)-voi_p*verbc(ibc(i),2)
           voi_o=uoi_p*verbc(ibc(i),2)+voi_p*verbc(ibc(i),1)

         !update the position
           xp_ob(i) = xp_ob(i) + uoi_o*dt 
           yp_ob(i) = yp_ob(i) + voi_o*dt   
      enddo
      
      
      
      return
      
      end
