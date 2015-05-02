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

	subroutine interp_openbc
	
        use global_2D
        
        integer i
        
        do i=1,nopen_bou !loop over boundaries
        
           call interp( ndata_obc(i), time_obc(i,1:ndata_obc(i)), 
     +      dwbc_time(i,1:ndata_obc(i)), upbc_time(i,1:ndata_obc(i)),
     +  time, dwbc(i), upbc(i) ) 

c        write(*,*) 'time= ', time, 'dwbc(i)= ', dwbc(i), 'upbc(i)= ', 
c     + upbc(i)
        
        enddo
	
	end subroutine
	
	
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^	
        subroutine interp(n,x,y1,y2,xi,yi1,yi2)
	
	implicit none
	
	integer n,i
	real x(n), y1(n), y2(n)
	real xi, yi1, yi2, m1, m2
	
	do i=1,n-1
	   if (xi.le.x(i+1)) then
	       m1=( y1(i+1)-y1(i) )/( x(i+1)-x(i) )
	       m2=( y2(i+1)-y2(i) )/( x(i+1)-x(i) )
	       yi1=y1(i)+m1*( xi-x(i) )
	       yi2=y2(i)+m2*( xi-x(i) )

	       return

	   endif
	
	enddo
	
	end
