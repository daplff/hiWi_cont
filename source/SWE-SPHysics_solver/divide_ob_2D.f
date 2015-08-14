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

       subroutine divide_ob(n_start,n_end,kind_p)
       use global_2D
       
       integer n_start, n_end, kind_p, k, icell, jcell, ii 
       real dxx, dyy
       
c     ist and ien are the starting and ending values of the positions in the linear
c     array ip (corresponding to a list of particle numbers) for the particles 
c     in cell ii
c

       do k=n_start,n_end
         if (iflag_ob(k).ne.0) then
         	
           dxx = xp_ob(k) - xmin
           dyy = yp_ob(k) - ymin
           
           icell = int( dxx * one_over_2hmax ) + 1
           jcell = int( dyy * one_over_2hmax ) + 1
       
           ! ii is the linear cell position in the matrix of 2h cells
           ii    = icell + (jcell - 1)*ncx 
                                                        
           ! nc is the number of particles in cell ii
           nc(ii,kind_p) = nc(ii,kind_p)+1	
                                      
          if(nc(ii,kind_p).gt.nplinkmax_var)then
           print*
           print*,'ERROR in divide_ob_2D.f'
           print*,'nc(ii,kind_p) >= nplink_max_var'
           print*,'itime',itime
           print*,'nc(ii,kind_p)',nc(ii,kind_p)
           print*,'kind_p       ',kind_p
           print*,'nplink_max',nplink_max
           print*,'k',k
           print*,'xp_ob(k),yp_ob(k)',xp_ob(k),',',yp_ob(k)
           print*,'icell,jcell ',icell,',', jcell
           print*,'Box ii ',ii
           stop
          end if
          
          ibox(ii,kind_p,nc(ii,kind_p))=k  !Tells us that particle with array location k (i.e. xp(k) )  
                                         !is in box ii which, so far, contains "nc(ii,mkind)" particles
                                                 
         endif
       enddo 
       
       return
       end

