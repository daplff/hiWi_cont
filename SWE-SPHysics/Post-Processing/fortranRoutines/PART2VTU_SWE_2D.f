c    "Copyright 2012 Prof. Robert Dalrymple, Prof. M. Gomez Gesteira, Dr Benedict Rogers, 
c     Dr. Alejandro Crespo, Dr. Muthukumar Narayanaswamy, Dr Shan Zou, Dr Andrea Panizzo "
c
c    This file is part of SPHYSICS.
c
c    SPHYSICS is free software; you can redistribute it and/or modify
c    it under the terms of the GNU General Public License as published by
c    the Free Software Foundation; either version 3 of the License, or
c    (at your option) any later version.
c
c    SPHYSICS is distributed in the hope that it will be useful,
c    but WITHOUT ANY WARRANTY; without even the implied warranty of
c    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c    GNU General Public License for more details.
c
c    You should have received a copy of the GNU General Public License
c    along with this program.  If not, see <http://www.gnu.org/licenses/>.


c       initial data for SPH models                                                                     72

      program PART2VTU_SWE_2D

      parameter(np_max = 270000,i_PART_counter_max=2000)
      parameter(num_phase_max=9)
      character chartemp*40, name_orig*40
      character name_vtu*40, name_vtu2*12, name_vtu3*9
      character(LEN=40) name_bed_orig,name_bed_vtu
      character np_string3*3, np_string4*4, np_string5*5
      character np_string6*6, np_string7*7, np_string8*8
      character frame_string1*1, frame_string2*2, frame_string3*3
      character frame_string4*4, frame_string5*5, frame_string6*6
      character supp*4, zero_string
      character string1*100,string2*100,string3*100,string4*100
      character chartemp2*100
      CHARACTER(LEN=10) :: FMT,FMT1
      CHARACTER(LEN=1)  :: TAB,DQ
      
      real xp(np_max),yp(np_max),up(np_max),vp(np_max)
      real eta(np_max),h_var(np_max),pm(np_max)
      integer iflag(np_max)
      real xbed(np_max),ybed(np_max),hb_SPH(np_max)
      real time(i_PART_counter_max), DT(i_PART_counter_max)
      real  DT1(i_PART_counter_max),DT2(i_PART_counter_max)  
      
      real np_phase_start(num_phase_max),np_phase(num_phase_max)
      real rho0_phase(num_phase_max),P0_phase(num_phase_max) 
      real gamma_phase(num_phase_max),viscos_phase(num_phase_max)
      real ST_coeff(num_phase_max),backgroundPressure
          
      integer i, iallo, ii, j, k, nplinkmax_coa, np_ob_y, i_openbc
      real area_min_coa, xp_ob_p, yp_ob_p

      TAB=CHAR(9)     
      FMT="(A)"
      FMT1="(2A)"
      DQ=CHAR(34)
      
       print*
       write(*,*) '<SWE-SPHYSICS>  Copyright (C) <2013>'
       write(*,*) 
     & '<Dr Renato Vacondio, Dr Benedict Rogers, '
       write(*,*) 
     & 'Prof. Peter Stansby and Prof. Paolo Mignosa, '
       write(*,*) 'This program comes with ABSOLUTELY NO WARRANTY;    '
       write(*,*) 'This is free software, and you are welcome to      '
       write(*,*) 'redistribute it under conditions stated in         '
       write(*,*) 'the FreeBSD License;                               '

       print*
       print*,' ---       SPHYSICS_SWE_1D.F       ---'
       print*,' ---       Distributed under       ---'
       print*,' ---      the FreeBSD License      ---'
       print*

c	---- READ PARAMETERS FROM indat -----


       open(11,file='indat')
   
       read(11,44) rho0
       write(*,*) 'rho0=',rho0, 'water density'
       read(11,44) viscos_val
       write(*,*) 'viscos_val=',viscos_val, 'artif viscosity'
       read(11,44) dw_min_fric
       write(*,*) 'dw_min_fric= ', dw_min_fric
       read(11,44) coef
       write(*,*) 'coef=', coef
       read(11,44) vlx
       write(*,*)  'vlx=',vlx
       read(11,44) vly
       write(*,*)  'vly=',vly
       read(11,43) np
       write(*,*)  'np=',np
       read(11,43) np_b
       write(*,*)  'np_b=',np_b
       read(11,43) npv
       write(*,*)  'npv=',npv
       read(11,43) i_openbc
       write(*,*) 'i_openbc=', i_openbc
       read(11,44) distmin
       write(*,*)  'distmin=',distmin    
       read(11,44) tol
       write(*,*)  'tol=',tol
       read(11,43) ivar_dt
       write(*,*)  'ivar_dt=',ivar_dt
!        read(11,44) dt
!        write(*,*)  'dt=',dt
!        read(11,44) CFL
!        write(*,*)  'CFL=',CFL
!        read(11,44) tmax
!        write(*,*)  'tmax=',tmax
!        read(11,44) out
!        write(*,*)  'out=',out
!        read(11,44) trec_ini
!        write(*,*)  'trec_ini=',trec_ini
!        read(11,43) i_restartRun 
!        write(*,*)  'i_restartRun=',i_restartRun 
!        read(11,44) hsm_b_max
!        write(*,*)  'hsm_b_max=',hsm_b_max
!        read(11,43) n0
!        write(*,*)  'n0=',n0
!        read(11,43) idebug
!        write(*,*)  'idebug=',idebug
!        read(11,43) iMUSCL
!        write(*,*)  'iMUSCL=',iMUSCL
!        read(11,43) i_dw_iter
!        write(*,*)  'i_dw_iter=',i_dw_iter
!        read(11,43) i_max_iter
!        write(*,*)  'i_max_iter=',i_max_iter
! 
!        !refining
!        ! read(11,44) area_lim
!        read(11,44) ref_p    
!        write(*,*)  'ref_p =',ref_p  
!        read(11,44) ref_h 
!        write(*,*)  'ref_h =',ref_h  
!        read(11,44) dw_min_ref 
!        write(*,*)  'dw_min_ref=', dw_min_ref 
!        read(11,44) xmin_ref
!        write(*,*)  'xmin_ref=', xmin_ref
!        read(11,44) ymin_ref
!        write(*,*)  'ymin_ref=', ymin_ref
!        read(11,44) dxx_ref
!        write(*,*)  'dxx_ref=', dxx_ref
!        read(11,44) dyy_ref
!        write(*,*)  'dyy_ref=', dyy_ref
!        read(11,43) ncx_ref
!        write(*,*)  'ncx_ref=', ncx_ref
!        read(11,43) ncy_ref
!        write(*,*)  'ncy_ref=', ncy_ref
!        !reading data to export on regular grid
!        read(11,44) dx_grd
!        write(*,*)  'dx_grd=', dx_grd
!        read(11,44) dy_grd
!        write(*,*)  'dy_grd=', dy_grd
       
       close(11)
      
43     format(i6)
44     format(f16.6)

      if(np.gt.np_max)then
        print*,'Number of entries in file PART exceeds max value'
        print*,'np.gt.np_max'
        print*,'Adjust np_max = ',np_max
        stop
      endif
      
c     %LOAD AND READ TIME,DT FROM FILE DT. THE FIRST HEADERLINE IN THE FILE IS
c     %SKIPPED
      print*,'Is the run complete? (1=Yes,0=No)'
      read(*,*)irun
      if(irun.eq.1)then
c       %LOAD AND READ TIME,DT FROM FILE DT. THE FIRST HEADLINE IN THE FILE IS
c       %SKIPPED         
c       %THE NUMBER OF PARTFILES, Nframes, IS ONE LESS THAN THE LENGTH OF THE time VECTOR.
c       %THIS IS BECAUSE THE LAST VALUE OF THE TIME VECTOR IS ASSOCIATED WITH EPART
c       Nframes=length(time)-1
        open(unit=70,file='DT',status='old')
        read(70,*) chartemp
        i_loop_finish = 0
c        i_ini = nb_Simple + 1
        i_PART_counter = 0
        do while(i_loop_finish.eq.0)
          i_PART_counter = i_PART_counter + 1
          if(i_PART_counter.gt.i_PART_counter_max)then
            print*,'Number of entries in file DT exceeds max value'
            print*,'i_PART_counter.gt.i_PART_counter_max'
            print*,'Adjust i_PART_counter_max, i_PART_counter_max = ',
     &              i_PART_counter_max
            stop
          endif
          read(70,*,END = 76)time(i_PART_counter),DT(i_PART_counter)
           
          !Determine whether to exit loop
          if(i_loop_finish.eq.0)then
            i_loop_finish = i_loop_finish - 1
          endif
76          i_loop_finish = i_loop_finish + 1
          !print*
        enddo
        N_start = 0
        Nframes = i_PART_counter-2
      else
        print*,'(To view IPART enter 0 and then 0) '
        print*,'Number of START of PART files to be visualized= '
        read(*,*)N_start
        print*,'Number of FINISH of PART files to be visualized= '
        read(*,*)Nframes
        if(Nframes.gt.i_PART_counter_max)then
          print*,'Number of entries in file DT exceeds max value'
          print*,'Nframes.gt.i_PART_counter_max'
          print*,'Adjust i_PART_counter_max, i_PART_counter_max = ',
     &            i_PART_counter_max
          stop
        endif
      endif
       

       
c     %THIS LOOP DETERMINES THE NUMBER OF DESIRED SUBDIVISIONS OF THE PARTICLE DATA FOR COLORING. THEN A UNIQUE SCALAR VALUE TITLED Scalarplot
c     % IS ASSIGNED FOR EACH DIVISION. THE SCALAR VALUE FOR DIFFERENT DIVISIONS ARE DIFFERENT. THIS IS ARBITRARILY CHOSEN
c     % SO AS TO INCLUDE THE ABILITY TO VIEW THE PARTICLES IN DIFFERENT DIVISIONS WITH DIFFERENT COLORS IN PARAVIEW

      name_bed_orig='ibottom'
      name_bed_vtu ='ParaviewFiles/VTU/ibottom'//'.vtu'
      print*, 'name_bed_orig, name_bed_vtu ',
     &         name_bed_orig, name_bed_vtu 
      !-- Load in BED PROFILE --
      open(25,file=name_bed_orig,status='old')
      open(26,file=name_bed_vtu,status='unknown')
      npp_bed=0 ! keeps track of number of particles

      print*
      print*,'Bottom Profile'
      do i=1,np_b
        read(25,*,end=299) xbed(i),ybed(i),hb_SPH(i)
        npp_bed=npp_bed+1
      enddo
299   np_bed=npp_bed
      print*,'np_bed ',np_bed
      print*
      close (25)
      !-------------------------
       
c     %LOOP OVER FRAMES
      ngrab = 0 + N_start - 1
      if(Nframes.eq.0)then
        ngrab = ngrab - 1
      endif    
      do iframe=N_start,Nframes
              
c       % READ IN THE PART FILE FOR EACH FRAME
        ngrab=ngrab+1
        write(supp,'(i4.4)') ngrab
        !print*,'supp =',supp
        if(iframe.eq.0)then
          name_orig='ipart'
          name_vtu ='ParaviewFiles/VTU/IPART.vtu'
        else
          name_orig='PART_'//supp
          name_vtu ='ParaviewFiles/VTU/PART'//supp//'.vtu'
        endif

        print*, 'iframe, name_orig, name_vtu ',
     &              iframe, ' ',name_orig, name_vtu 
        
        open(23,file=name_orig,status='old')
        open(24,file=name_vtu,status='unknown')
           
              
c       % READ POSITION, VELOCITY, ELEVATION, MASS AND SMOOTHING LENGTH DATA FOR ALL PARTICLES                      
         !- Muthu Correction - 
         npp=0 ! keeps track of number of particles

!          do i=1,np
         i_continue = 1
         i = 0
         do while(i_continue.eq.1)
             i = i +1
             read(23,*,end=300) xp(i),yp(i),up(i),vp(i),eta(i),
     +    pm(i), h_var(i), iflag(i)
            npp=npp+1
         enddo
300    np=npp


        close (23)
        print*,'np ',np
                                                                
          idiv=2
          nbeg1=1
          nend1=np_bed
          nbeg2=np_bed+1
          nend2=np
              
       
201     format(a40)
202     format(a100)
203     format(a25,i7,a17,i7,a2)
211     format(a21)
c     % OUTPUT TO FILE IN VTU FORMAT 
        if(np.lt.1000)then       
          write(np_string3,'(i3.3)') np
        string4 = '  <Piece NumberOfPoints='//DQ//np_string3//DQ//' Numb
     &erOfCells='//DQ//np_string3//DQ//'>'
        elseif(np.lt.10000)then       
          write(np_string4,'(i4.4)') np
        string4 = '  <Piece NumberOfPoints='//DQ//np_string4//DQ//' Numb
     &erOfCells='//DQ//np_string4//DQ//'>'
        elseif(np.lt.100000)then       
          write(np_string5,'(i5.5)') np
        string4 = '  <Piece NumberOfPoints='//DQ//np_string5//DQ//' Numb
     &erOfCells='//DQ//np_string5//DQ//'>'
        elseif(np.lt.1000000)then       
          write(np_string6,'(i6.6)') np
        string4 = '  <Piece NumberOfPoints='//DQ//np_string6//DQ//' Numb
     &erOfCells='//DQ//np_string6//DQ//'>'
        elseif(np.lt.10000000)then       
          write(np_string7,'(i7.7)') np
        string4 = '  <Piece NumberOfPoints='//DQ//np_string7//DQ//' Numb
     &erOfCells='//DQ//np_string7//DQ//'>'
        elseif(np.lt.100000000)then       
          write(np_string8,'(i8.8)') np
        string4 = '  <Piece NumberOfPoints='//DQ//np_string8//DQ//' Numb
     &erOfCells='//DQ//np_string8//DQ//'>'
        else
          print*,'Too many particles for np_string'
          stop  
        endif
        !print*,'np_string, np ',np_string, np 
        string1 = '<?xml version='//DQ//'1.0'//DQ//'?>'
        string2 = '<VTKFile type= '//DQ//'UnstructuredGrid'//DQ//'  vers
     &ion= '//DQ//'0.1'//DQ//'  byte_order= '//DQ//'BigEndian'//DQ//'>'
        string3 = ' <UnstructuredGrid>'
        write(24,211)string1
        write(24,202)string2
        write(24,202)string3
        write(24,202)string4
              
c       % WRITE IN PRESSURE DATA
        string1 = '   <PointData Scalars='//DQ//'Elevation'//DQ//' Vecto
     &rs='//DQ//'Velocity'//DQ//'>'        
        string2 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'Elevation'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202)string1
        write(24,202)string2
        do ii=1,np
          write(24,*)eta(ii)
        enddo
        string3 = '    </DataArray>'
        write(24,202)string3

c       % WRITE DENSITY DATA        
        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'SmoothingLength'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202)string1
        do ii=1,np
          write(24,*)h_var(ii)
        enddo
        string3 = '    </DataArray>'
        write(24,202) string3

c       % WRITE U-VELOCITY DATA        
        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'u-Velocity'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202)string1
        do ii=1,np
          write(24,*)up(ii)
        enddo
        string3 = '    </DataArray>'
        write(24,202) string3

c       % WRITE W_VELOCITY DATA        
        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'v-Velocity'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202)string1
        do ii=1,np
          write(24,*)vp(ii)
        enddo
        string3 = '    </DataArray>'
        write(24,202) string3


c       % THIS SECTION IS USED TO COLOR DIFFERENT PARTICLES BASED THE INPUT IDIV SPECIFIED ABOVE.
        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'Scalarplot'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202) string1
        do ii=1,idiv
          if(ii.eq.1)then
            nbeg = nbeg1
            nend = nend1
          elseif(ii.eq.2)then
            nbeg = nbeg2
            nend = nend2
          elseif(ii.eq.3)then
            nbeg = nbeg3
            nend = nend3
          endif
          do jj=nbeg,nend
            write(24,*)ii
          enddo
        enddo
        string3 = '    </DataArray>'
        write(24,202)string3
              
cc       % WRITE VELOCITY DATA
c        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
c     &Q//'Velocity'//DQ//' NumberOfComponents='//DQ//'3'//DQ//' format='
c     &//DQ//'ascii'//DQ//'>'
c        write(24,202) string1
c        do ii=1,np
c          write(24,*)up(ii),0.0,wp(ii)
c        enddo
c        string3 = '    </DataArray>'
c        write(24,202) string3
        string4 = '   </PointData>'
        write(24,202) string4
              
c       % WRITE PARTICLE POSITION DATA
        string2 = '   <Points>'
        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' NumberOfCo
     &omponents='//DQ//'3'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202) string2
        write(24,202) string1
        do ii=1,np
          write(24,*)xp(ii),yp(ii),0
        enddo
        string3 = '    </DataArray>'
        string2 = '   </Points>'
        write(24,202) string3
        write(24,202) string2
             
c       % WRITE CELL DATA. CELL IS OF TYPE VERTEX.        
        string2 = '   <Cells>'
        string1 = '    <DataArray type='//DQ//'Int32'//DQ//' Name='//DQ/
     &/'connectivity'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202) string2
        write(24,202) string1
        do ii=1,np
          write(24,*)ii-1
        enddo
        string3 = '    </DataArray>'
        write(24,202) string3
        
        
        string1 = '    <DataArray type='//DQ//'Int32'//DQ//' Name='//DQ/
     &/'offsets'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202) string1
        do ii=1,np
          write(24,*)ii
        enddo
        string3 = '    </DataArray>'
        write(24,202) string3
        
        
        string1 = '    <DataArray type='//DQ//'Int32'//DQ//' Name='//DQ/
     &/'types'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(24,202) string1
        do ii=1,np
          write(24,*)1
        enddo
        string3 = '    </DataArray>'
        write(24,202) string3
        
        
        string1 = '   </Cells>' 
        string2 = '  </Piece>'
        string3 = ' </UnstructuredGrid>'
        string4 = '</VTKFile>'
        write(24,202) string1
        write(24,202) string2
        write(24,202) string3
        write(24,202) string4
      enddo   !End of:  do iframe=N_start,Nframes
      close(24);


C     Write the BED.vtu files      
        if(np_bed.lt.1000)then       
          write(np_string3,'(i3.3)') np_bed
        string4 = '  <Piece NumberOfPoints='//DQ//np_string3//DQ//' Numb
     &erOfCells='//DQ//np_string3//DQ//'>'
        elseif(np_bed.lt.10000)then       
          write(np_string4,'(i4.4)') np_bed
        string4 = '  <Piece NumberOfPoints='//DQ//np_string4//DQ//' Numb
     &erOfCells='//DQ//np_string4//DQ//'>'
        elseif(np_bed.lt.100000)then       
          write(np_string5,'(i5.5)') np_bed
        string4 = '  <Piece NumberOfPoints='//DQ//np_string5//DQ//' Numb
     &erOfCells='//DQ//np_string5//DQ//'>'
        elseif(np_bed.lt.1000000)then       
          write(np_string6,'(i6.6)') np_bed
        string4 = '  <Piece NumberOfPoints='//DQ//np_string6//DQ//' Numb
     &erOfCells='//DQ//np_string6//DQ//'>'
        elseif(np_bed.lt.10000000)then       
          write(np_string7,'(i7.7)') np_bed
        string4 = '  <Piece NumberOfPoints='//DQ//np_string7//DQ//' Numb
     &erOfCells='//DQ//np_string7//DQ//'>'
        elseif(np_bed.lt.100000000)then       
          write(np_string8,'(i8.8)') np_bed
        string4 = '  <Piece NumberOfPoints='//DQ//np_string8//DQ//' Numb
     &erOfCells='//DQ//np_string8//DQ//'>'
        else
          print*,'Too many particles for np_string'
          stop  
        endif
        !print*,'np_string, np ',np_string, np 
        string1 = '<?xml version='//DQ//'1.0'//DQ//'?>'
        string2 = '<VTKFile type= '//DQ//'UnstructuredGrid'//DQ//'  vers
     &ion= '//DQ//'0.1'//DQ//'  byte_order= '//DQ//'BigEndian'//DQ//'>'
        string3 = ' <UnstructuredGrid>'
        write(26,211)string1
        write(26,202)string2
        write(26,202)string3
        write(26,202)string4
              

c       % THIS SECTION IS USED TO COLOR DIFFERENT PARTICLES BASED THE INPUT IDIV SPECIFIED ABOVE.
        string1 = '   <PointData Scalars='//DQ//'Bed Elevation'//DQ//' V
     &ectors='//DQ//'Velocity'//DQ//'>'        
        string2 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'Bed Elevation'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(26,202)string1
        write(26,202)string2
        do ii=1,np_bed
          write(26,*)hb_SPH(ii)
        enddo
        string3 = '    </DataArray>'
        write(26,202)string3

        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' Name='//D
     &Q//'Scalarplot'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(26,202)string1
!         do ii=1,idiv
!           if(ii.eq.1)then
!             nbeg = nbeg1
!             nend = nend1
!           elseif(ii.eq.2)then
!             nbeg = nbeg2
!             nend = nend2
!           elseif(ii.eq.3)then
!             nbeg = nbeg3
!             nend = nend3
!           endif
            ii = 1
            nbeg = 1
            nend = np_bed
          do jj=nbeg,nend
            write(26,*)ii
          enddo
!         enddo
        string3 = '    </DataArray>'
        write(26,202)string3
              
        string4 = '   </PointData>'
        write(26,202) string4
              
c       % WRITE PARTICLE POSITION DATA
        string2 = '   <Points>'
        string1 = '    <DataArray type='//DQ//'Float32'//DQ//' NumberOfCo
     &omponents='//DQ//'3'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(26,202) string2
        write(26,202) string1
        do ii=1,np_bed
          write(26,*)xbed(ii),ybed(ii),hb_SPH(ii)
        enddo
        string3 = '    </DataArray>'
        string2 = '   </Points>'
        write(26,202) string3
        write(26,202) string2
             
c       % WRITE CELL DATA. CELL IS OF TYPE VERTEX.        
        string2 = '   <Cells>'
        string1 = '    <DataArray type='//DQ//'Int32'//DQ//' Name='//DQ/
     &/'connectivity'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(26,202) string2
        write(26,202) string1
        do ii=1,np_bed
          write(26,*)ii-1
        enddo
        string3 = '    </DataArray>'
        write(26,202) string3
        
        
        string1 = '    <DataArray type='//DQ//'Int32'//DQ//' Name='//DQ/
     &/'offsets'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(26,202) string1
        do ii=1,np_bed
          write(26,*)ii
        enddo
        string3 = '    </DataArray>'
        write(26,202) string3
        
        
        string1 = '    <DataArray type='//DQ//'Int32'//DQ//' Name='//DQ/
     &/'types'//DQ//' format='//DQ//'ascii'//DQ//'>'
        write(26,202) string1
        do ii=1,np_bed
          write(26,*)1
        enddo
        string3 = '    </DataArray>'
        write(26,202) string3
        
        
        string1 = '   </Cells>' 
        string2 = '  </Piece>'
        string3 = ' </UnstructuredGrid>'
        string4 = '</VTKFile>'
        write(26,202) string1
        write(26,202) string2
        write(26,202) string3
        write(26,202) string4
!       enddo   !End of:  do iframe=N_start,Nframes
      close(26);
      
c     %THIS CODE WRITES OUT A FILE CALLED VTUinp.pvd. THIS FILE IS NECESSARY TO ANIMATE THE VTU FILES (OBTAINED BY RUNNING PART2VTU.m) IN
c     % PARAVIEW 3. 
c     %  THIS FILE VTUinp.pvd SHOULD BE PLACED IN THE SAME DIRECTORY AS THE VTU FILES. THIS .pvd FILE BASICALLY CONTAINS A LIST OF VTU
c     % FILES THAT ARE TO BE ANIMATED. LOADING THIS FILE IN PARAVIEW 3.0 IS SUFFICIENT TO PERFORM THE ANIMATION
       
       
c     %OPEN THE VTUinp.pvd FILE FOR WRITING 
      i_rewriteHeader = 0
      if(Nframes.eq.0)then
        open(24,file='ParaviewFiles/VTU/IPART_VTUinp.pvd',
     &       status='unknown')
        i_rewriteHeader = 1
      else
        !- Check to see if file PVD file exists
        i_newFile=0
        open(24,file='ParaviewFiles/VTU/VTUinp.pvd',
     &       err=302,status='old')
        close(24)
        i_newFile = i_newFile - 1
302     i_newFile = i_newFile + 1  
        if(i_newFile.eq.0.and.Nframes.ne.0)then
          print*,'Using existing PVD file '
        else
          print*,'Creating new PVD file '
        endif    
        if(N_start.gt.0.and.i_newFile.eq.0)then
          open(24,file='ParaviewFiles/VTU/VTUinp.pvd',
     &       status='old')
          !- Position File correctly -
          if(N_start.gt.1)then
            do i_temp = 1,N_start+2
              read(24,*,end=301)chartemp2
            enddo
            i_rewriteHeader = i_rewriteHeader - 1
          endif
301       i_rewriteHeader = i_rewriteHeader + 1
        else
          open(24,file='ParaviewFiles/VTU/VTUinp.pvd',
     &       status='unknown')
          i_rewriteHeader = 1
        endif
      endif

       
c     % WRITE THE STANDARD HEADER FOR THE FILE    
      if(N_start.lt.2.or.i_rewriteHeader.eq.1)then
      string1 = '<?xml version='//DQ//'1.0'//DQ//'?>'
      string2 = ' <VTKFile type='//DQ//'Collection'//DQ//' version='//DQ
     &//'0.1'//DQ//'>'
      string3 = '  <Collection>'
      write(24,211)string1
      write(24,202)string2
      write(24,202)string3
      endif
       
c     % WRITE THE FILE NAME FOR EACH TIME STEP
      zero_string=CHAR(48)
      ngrab = 0 + N_start - 1
      if(Nframes.eq.0)then
        ngrab = ngrab - 1
      endif    
      do ii=N_start,Nframes
        ngrab=ngrab+1
        write(supp,'(i4.4)') ngrab
        if(Nframes.eq.0)then
          name_vtu3='IPART.vtu'
          write(frame_string1,'(i1.1)') ii
          string4 = '<DataSet timestep='//DQ//frame_string1//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu3
     &//DQ//'/>'
        else
          name_vtu2='PART'//supp//'.vtu'
        endif
        if(ii.gt.0)then
         if(ii.lt.10)then       
          write(frame_string1,'(i1.1)') ii
          string4 = '<DataSet timestep='//DQ//frame_string1//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu2
     &//DQ//'/>'
         elseif(ii.lt.100)then       
          write(frame_string2,'(i2.2)') ii
          string4 = '<DataSet timestep='//DQ//frame_string2//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu2
     &//DQ//'/>'
         elseif(ii.lt.1000)then       
          write(frame_string3,'(i3.3)') ii
          string4 = '<DataSet timestep='//DQ//frame_string3//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu2
     &//DQ//'/>'
         elseif(ii.lt.10000)then       
          write(frame_string4,'(i4.4)') ii
          string4 = '<DataSet timestep='//DQ//frame_string4//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu2
     &//DQ//'/>'
         elseif(ii.lt.100000)then       
          write(frame_string5,'(i5.5)') ii
          string4 = '<DataSet timestep='//DQ//frame_string5//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu2
     &//DQ//'/>'
         elseif(ii.lt.1000000)then       
          write(frame_string6,'(i6.6)') ii
          string4 = '<DataSet timestep='//DQ//frame_string6//DQ//' group
     &='//DQ//DQ//' part='//DQ//zero_string//DQ//' file='//DQ//name_vtu2
     &//DQ//'/>'
         else
          print*,'Too many frames'
          stop
         endif
        endif 
        if(ii.gt.0)write(24,202) string4
       enddo
       
      string3 = '   </Collection>'
      string4 = '  </VTKFile>'
      write(24,202) string3
      write(24,202) string4
       
      close(24)
       
      stop
      end
