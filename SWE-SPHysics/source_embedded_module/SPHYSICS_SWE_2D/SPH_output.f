        subroutine SPH_output
            use global_2D

            implicit none

      character supp*4,name*40, name2*40, name3*40, name4*40

      integer i

            !-- File Output Routines --
         if(grab_P.ge.out) then

           if (time.ge.trec_ini) then
              ngrab=ngrab+1
              write(*,*) ngrab
              write(supp,'(i4.4)') ngrab
              name='PART_'//supp
              name2='GRD_d'//supp
              name3='GRD_u'//supp
              name4='GRD_v'//supp
              write(*,*) name
              open(23,file=name,status='replace')

              open(24,file=name2,status='replace')
              open(25,file=name3,status='replace')
              open(26,file=name4,status='replace')

              call poute(22)
              call poute_grid(24)
              close(23)
              close(24)
              close(25)
              close(26)

          close(44)

       endif

           grab_P=0.

         endif

         if(itime.eq.itime*10/10)then
           write(*,*) 'time, dt hmax ',time,dt, hmax, '  ok'
         endif
         itime = itime+1
         time = time + dt
         grab_P=grab_P+dt






        end
