
% POST PROCESSING ROUTINE FOR SPHYSICS_SWE_2D

clear all
close all
clc

%READ PARAMETERS FROM MATLABIN
%  load matlabin;
%  np=matlabin(1);
%  vlx=matlabin(2);
%  vly=matlabin(3);

%  nplot=1.1*sqrt(np);

%IF THE SIMULATION IS COMPLETE LOAD THE TIME INFORMATION FROM FILE DT. IF
%SIMULATION IS ONGOING, JUST PLOT USER SPECIFIED FRAMES
irun=input('Is the run complete? (1=Yes,0=No)  ');
if(irun==1)
    %LOAD AND READ TIME,DT FROM FILE DT. THE FIRST HEADLINE IN THE FILE IS
    %SKIPPED
    [time,DT1,DT2,DT]=textread('DT','%f %f %f %f','headerlines',1);
    %THE NUMBER OF PARTFILES, Nframes, IS ONE LESS THAN THE LENGTH OF THE time VECTOR.
    %THIS IS BECAUSE THE LAST VALUE OF THE TIME VECTOR IS ASSOCIATED WITH EPART
    Nframes=length(time)-1;
    first_frame=1;
else

    first_frame=input('first file to be visualized= ');
    Nframes=input('last file to be visualized= ');
    dt=input('insert dt ');
    time=zeros(Nframes,1);
    time(first_frame:Nframes)=(first_frame-1:1:Nframes-1)*dt;
end


%INITIALIZE PARTICLE SIZE, VARIABLE M FOR MOVIES, PLOT AXES AND
%POSITION OF TEXT
ParticleSize=2;
%  xmin=-0.01;
%  xmax=1.05*vlx;
%  zmin=0;
%  zmax=1.1;
sframes=input('Do you want to save frames? (1=Yes,0=No)  ');

ll=0;

%load('winter2','mycmap')
%load('winter3','mycmap2')

flag_bint=1;
for ii=first_frame:Nframes
    
    %READ IN EACH FRAME INTO PART1,PART2...PART'EFRAME'
    if ((ii>0)& (ii< 10)) 
        
        eval(['fn =','''GRD_d000', int2str(ii),'''',';'])
        [h_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_u000', int2str(ii),'''',';'])
        [vx_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_v000', int2str(ii),'''',';'])
        [vy_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn2 =','''PART_000', int2str(ii),'''',';'])
        PART=load(fn2);
    elseif ((ii>=10) & (ii<100)) 
        eval(['fn =','''GRD_d00', int2str(ii),'''',';'])
        [h_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_u00', int2str(ii),'''',';'])
        [vx_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_v00', int2str(ii),'''',';'])
        [vy_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn2 =','''PART_00', int2str(ii),'''',';'])
        PART=load(fn2);
    elseif ((ii>=100) & (ii<1000)) 
        eval(['fn =','''GRD_d0', int2str(ii),'''',';'])
        [h_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_u0', int2str(ii),'''',';'])
        [vx_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_v0', int2str(ii),'''',';'])
        [vy_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn2 =','''PART_0', int2str(ii),'''',';'])
        PART=load(fn2);
    elseif ((ii>=1000) & (ii<10000)) 
        eval(['fn =','''GRD_d', int2str(ii),'''',';'])
        [h_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_u', int2str(ii),'''',';'])
        [vx_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn =','''GRD_v', int2str(ii),'''',';'])
        [vy_int nx ny xmin xmax ymin ymax]=grd_read_v2(fn);
        eval(['fn2 =','''PART_', int2str(ii),'''',';'])
        PART=load(fn2);
    end 
    
    if (flag_bint==1)
       XI=linspace(xmin,xmax,nx);
       YI=linspace(ymin,ymax,ny);
       [XX,YY]=meshgrid(XI,YI);   
       load ibottom
       xbmin=min(ibottom(:,1));
       xbmax=max(ibottom(:,1));
       ybmin=min(ibottom(:,2));
       ybmax=max(ibottom(:,2));
       lb = length(ibottom(:,1))
       ibottom(lb+1,1)=xbmax*100;
       ibottom(lb+1,2)=ybmax*100;
       ibottom(lb+1,3)=-0.01;
       XIb=linspace(xbmin,xbmax,nx);
       YIb=linspace(ybmin,ybmax,ny);
       [XXb,YYb]=meshgrid(XIb,YIb);   
       b_int=griddata(ibottom(:,1),ibottom(:,2),ibottom(:,3),XX,YY);  
       depth_factor = 0.4*(xbmax - xbmin) / max(PART(:,5))
       flag_bint=0;
    end

   
    figure(1) %water level
    surf(XX,YY,depth_factor*(h_int+b_int),h_int,'EdgeColor','none','FaceColor','interp','FaceLighting','phong','facealpha',0.6); grid on, shading interp, xlabel('X(m)'), ylabel('Y(m)'), colormap (gray), axis equal, axis off 
    freezeColors
    hold on
    surf(XX,YY,b_int,'EdgeColor','none','FaceColor','interp','FaceLighting','phong'); colormap gray, axis off
    hold off

    title(['Water Elevation, Time=',num2str(time(ii)),'s : ','  FRAME=',num2str(ii)])

    hold off
    drawnow
    if(sframes==1)
        
       if ((ii>0)&& (ii< 10)) 
           fnames=['frame_surf_wse_00',num2str(ii),'.jpg'];
       elseif ((ii>=10) && (ii<100)) 
           fnames=['frame_surf_wse_0',num2str(ii),'.jpg'];
       elseif ((ii>=100) && (ii<1000)) 
           fnames=['frame_surf_wse',num2str(ii),'.jpg'];
       end 
       print ('-djpeg', '-r300','-zbuffer',fnames)
           
   end
    
    figure(2) 
    %cbfreeze('off') 
    contourf(XX,YY,h_int,'EdgeColor','none'), grid on, xlabel('X(m)'), ylabel('Y(m)'), axis equal
    colormap (gray)
    colorbar
    freezeColors
    %cbfreeze
    hold on
    contour(XX,YY,b_int,'LineColor', 'black', 'ShowText', 'off');
    colormap gray  
    
    title(['Water Elevation, Time=',num2str(time(ii)),'s : ','  FRAME=',num2str(ii)])

    hold off
    
    if(sframes==1)

        if ((ii>0)&& (ii< 10))
            fnames=['frame_wse_00',num2str(ii),'.jpg'];
        elseif ((ii>=10) && (ii<100))
            fnames=['frame_wse_0',num2str(ii),'.jpg'];
        elseif ((ii>=100) && (ii<1000))
            fnames=['frame_wse',num2str(ii),'.jpg'];
        end
        print ('-djpeg', '-r300','-zbuffer',fnames)
    end
    
    figure(3) 
    %cbfreeze('off') 
    contourf(XX,YY,sqrt(vx_int.*vx_int+vy_int.*vy_int),'EdgeColor','none'), grid on, xlabel('X(m)'), ylabel('Y(m)'), axis equal
    colormap (gray)
    colorbar
    freezeColors
    %cbfreeze
    hold on
    contour(XX,YY,b_int,'LineColor', 'black', 'ShowText', 'off');
    colormap gray  
    %quiver(XX,YY,vx_int,vy_int,2,'k')
    quiver(PART(:,1),PART(:,2),PART(:,3),PART(:,4),'k')
    %plot(PART(:,1),PART(:,2),'k+','MarkerSize',ParticleSize)
    
    title(['Velocity magnitude, Time=',num2str(time(ii)),'s : ','  FRAME=',num2str(ii)])

    hold off
    
    if(sframes==1)

        if ((ii>0)&& (ii< 10))
            fnames=['frame_vel_00',num2str(ii),'.jpg'];
        elseif ((ii>=10) && (ii<100))
            fnames=['frame_vel_0',num2str(ii),'.jpg'];
        elseif ((ii>=100) && (ii<1000))
            fnames=['frame_vel',num2str(ii),'.jpg'];
        end
        print ('-djpeg', '-r300','-zbuffer',fnames)
    end
end


