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
c Note: this source has been modified to be wrapped in a C++ interface class.
c Modifications by Erik Wannerberg.

      subroutine sph_loopstep
       
      use global_2D

c ... TODO: Suppress output. Import variables from C, output status. Cut output routines.
c ... actually, this file might be obsolete: might just call step itself.


c  .............................. MAIN LOOP ...................................
c

              
         	         
         !-- Call Main Subroutines --  
         call step
                   
         !-- File Output Routines --
c         if(grab_P.ge.out) then 
c       
c           if (time.ge.trec_ini) then
c              ngrab=ngrab+1
c              write(*,*) ngrab
c              write(supp,'(i4.4)') ngrab
c              name='PART_'//supp
c              name2='GRD_d'//supp
c              name3='GRD_u'//supp
c              name4='GRD_v'//supp
c              write(*,*) name
c              open(23,file=name,status='replace')
c           
c              open(24,file=name2,status='replace')
c              open(25,file=name3,status='replace')
c              open(26,file=name4,status='replace')
c    
c              call poute(22)
c              call poute_grid(24)
c              close(23)
c              close(24)
c              close(25)
c              close(26)
c           
c              open(44,file='restart',status='replace')
c              write(44,100) itime,time,ngrab, npc
c	      close(44)
c	      
c	   endif
c       
c           grab_P=0.
c       
c         endif
c            
         if(itime.eq.itime*10/10)then
           write(*,*) 'time, dt hmax ',time,dt, hmax, '  ok'     
         endif
         itime = itime+1
         time = time + dt 
         grab_P=grab_P+dt
         
      end subroutine

