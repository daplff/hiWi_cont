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

	subroutine variable_time_step
        use global_2D
        
        integer i, i_fa
        real Fa, sm_l, hsm_b_min, dt_try

C	Calculate time step 

        dt_new=dtmax !maximum timestep allowed
        
        !minimum smoothing length for bottom particles
        hsm_b_min=coef*sqrt(Vol_b_min)
               
	do i=nstart,np
	   if (iflag(i).eq.1) then

               Fa=sqrt( up(i)*up(i)+vp(i)*vp(i) )+cs(i) 
               sm_l=min(h_var(i),hsm_b_min)
              dt_try=CFL*(sm_l/Fa)
	     if (dt_try.lt.dt_new) then
                i_fa=i
	        dt_new=dt_try
	     endif
	   endif
	enddo 

c ------time step condition to avoid wall penetration	
c        dt_vir is defined in ac_alpha
	dt_new=min(dt_new,dt_vir*CFL)

	return

	end
