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
      module global_2D
     
      integer, parameter :: npar=500000,nb_max=400
      integer, parameter :: nct_max=300000,nplink_max=1000

      real xp(npar),yp(npar),up(npar),vp(npar),rhop(npar),pm(npar),
     + cs(npar), areap(npar)
      integer np
      
      real time,tmax,out,trec_ini 
      integer ivar_dt,itime, itime_ini
      
      real dtrec_det,t_sta_det,t_end_det
                          
      real   r0
      
      real xmin,xmax,ymin,ymax, dw_min_fric
      integer ncx,ncy,nct,nc(nct_max,3),iloop(nct_max)
      
      real  h,hh(3),c1(3),d1(3),c2(3),a1,a2(3),a24(3),f1(3),
     1               b1(3),e1(3),aa,pi,twopi,h2(3),
     2               fourh2(3),hh_x(3),hh_z(3),
     3               twoh_x(3),twoh_z(3),Awen(3),Bwen(3),
     4               one_over_2h,one_over_h,two_h,four_h,six_h
      
      real  alphad, const_ker(2)
      real  viscos_val,eta,eta2,visc_wall
      real gamma,rho0,cs0,h_SWL,
     1               one_over_beta_coef2,one_over_beta_coef, coef
      integer i_EoS, i_gamma
      
      real  gg,damp
c      common /xsph/  xcor(npar),zcor(npar),eps
      real  grx,grz,thetam,omega,gx,stheta,orig
      
      real   mw1,mw2,xamp,xfreq,h0,xorig(npar)               
      integer iwmcase
      
      real ax(npar),ay(npar),ar(npar),
     1               ux(npar),wx(npar),zx(npar)
      real sum_wab(npar),Wab,frx,fry,fac
      
      integer ncall1,ncall2,ncall3,ncall4,ncall5,ncall6,
     1                  ncall7,ncall8,ncall9,ncall10

      integer nstart,nstart_minus1

      integer nbf,nbfp1,iopt_movingObject
      integer keep_nc(nct_max)
      integer i_viscos,i_kernel,i_algorithm
      real dt,dt_new,dt_old,dtmax
      real xmax_container,xmin_container,
     1     ymax_container,ymin_container,xtrash,ytrash,zmax_ini,
     2     ncz_ini,zcontrol
      integer iflag(npar), nct_ini
      real vlx1,vlx2,vlx,vly 
      
      integer i_geometry
      
      real lattice,vnorm_mass
      real grab_P,grab_E
      integer i_restartRun, ngrab

      integer nplinkmax_var

      real dw(npar), dw0(npar), CFL
            
      real xlim_left, xlim_right, ylim_left, ylim_right, 
     1 h_var(npar),h_var0(npar), hmax, hmax0, one_over_2hmax, dm
     
      real alphap(npar), tol, rhop_sum, rhop_sum_j(npar),
     1 grav, Res_adm(npar), uo(npar),vo(npar),
     2 udot(npar), vdot(npar), sum_f(npar,4)
      integer ighost(npar), n_assign
      
      real h_t(npar),dh_tx(npar),dh_ty(npar),ddh_txx(npar),
     1 ddh_txy(npar),ddh_tyy(npar),t_stx(npar),t_sty(npar)
     
      real dH_SPH_x(npar),dH_SPH_y(npar),ddH_SPH_xx(npar),
     1  ddH_SPH_yy(npar), ddH_SPH_xy(npar), diff_udot(npar)
      integer itest
      
c -- virtual particles --     
      real xv(npar), yv(npar), vnxv(npar), vnyv(npar), dxv(npar), 
     + od_Wdeltap, dt_vir
      integer npv
      
c -- bottom particles --

      real xb(npar), yb(npar), hb(npar), fr(npar), hsm_b(npar), 
     1 Vol_b(npar), dxb, xmaxb,xminb,ymaxb,yminb, 
     2 hsm_b_max, one_over_2hsm_b, Vol_b_min
     
      integer np_b,nct_b,ncx_b,ncy_b
      integer, allocatable :: iboxb(:,:), ncb(:)
     
      real sum_h_t(npar), sum_frx(npar), sum_b_f(npar,4), sum_b(npar), 
     1  fr_SPH(npar)
      
      integer n0 !max number of particles in one cell
      
      integer, allocatable :: ibox(:,:,:)  
      
      !gradient correction for virtual particles
      !grad_dw_x and grad_dw_y are just for debugging purposes
      real L_c(4), dw_x, dw_y,  grad_dw_x(npar),  grad_dw_y(npar)
      
c  -- refinement --
      
      real area_max, ref_p, ref_h, dw_min_ref, pm_u(npar) ,pmold(npar) 
      real pm0(npar), Vol(npar)
      real area_min, sum_v
      real  xmin_ref, ymin_ref, dxx_ref, dyy_ref, area_lim_min
      integer n_nei, ntotref, li_ref(npar), irli(npar,7), npold
      integer ncx_ref, ncy_ref, nct_ref   
      
      integer, allocatable :: ipvic(:), nc_coa(:), ibox_coa(:,:)
      real, allocatable :: area_lim(:), area_lim_coa(:) 
      
c -- MUSCL reconstruction
     
      real  grad_up(npar,2), grad_vp(npar,2), grad_dw(npar,2)
      
c -- open boundaries
      
      real dwi(npar), upi(npar), vpi(npar), uoi(npar), voi(npar)

      real, allocatable :: xpbc(:), ypbc(:), lbc(:), verbc(:,:),
     + verybc(:), dxobc(:)     
      integer, allocatable ::  ndata_obc(:), iflagbc(:)
          
      integer ibc(npar), nopen_bou, np_ob
      
      real xp_ob(npar), yp_ob(npar), up_ob(npar), vp_ob(npar), 
     + uo_ob(npar), vo_ob(npar), h_var_ob(npar), dw_ob(npar), 
     + pm_ob(npar), h_var0_ob(npar), dw0_ob(npar),
     + rhop_ob(npar), cs_ob(npar), alphap_ob(npar)
      integer iflag_ob(npar)
      
      real, allocatable :: time_obc(:,:), dwbc_time(:,:), 
     + upbc_time(:,:), dwbc(:), upbc(:)
     
      real distmin
      
c --- poute_grid

      real, allocatable :: dw_grd(:,:), up_grd(:,:), vp_grd(:,:), 
     + sum_grd(:,:), dx_fs_grd(:,:), dy_fs_grd(:,:)
      
      real dx_grd, dy_grd, dv_plot
      
      integer nplot_x, nplot_y

c --- additional constant

      integer  i_dw_iter, i_max_iter, idebug, iMUSCL
       
      end module
