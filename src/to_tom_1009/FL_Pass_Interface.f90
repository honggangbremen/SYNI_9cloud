MODULE FL_Pass_Interface
!
!******************************************************************************
!
! !F90
!
! !Name:
!   FL_Pass_Interface
!
! !Description:
!   Routine ID -
!
!   Purpose - To provide a module of routines to interface with the Fu-Liou 
!             model
!
!
! !Input Parameters:
!   NONE
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Internal Routines:
!    FL_Call         - Calls the Fu-Liou radiative transfer model
!    FL_Pass_Drv     - Drive the calls for each requested sky condition for  
!                      the current pass through the Fu-Liou model
!
!  Modules Used:
!    FL_IO_Params       - (SARBlib) Used by Sbr. FL_Call, FL_Pass_Drv, 
!    FULIOUMUTI         - (SARBlib) Used by Sbr. FL_Call 
!    MonMODISAer_Params - (SARBlib) Used by Sbr. FL_Call 
!    Profile_Params     - (SARBlib) Used by Sbr. FL_Pass_Drv
!    SARBInput_Params   - (SARBlib) Used by Sbr. FL_Call 
!    SARB_Var           - (SARBlib) Used by Sbr. FL_Call 
!
!  External Routines Called:
!    None
!
! !Revision History:
!  Version 4.0   June 17, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Modified FL_Call and USE statements 
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Removal of ClearSky_Drv, CloudySky_Drv, and
!                   PristineSky_Drv subroutines made obsolete with new 
!                   FuLiou model 
!                2. Modifications to FL_Call and FL_Pass_Drv to interface with
!                   new FuLiou model 
!  Version 3.3   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code - broken out of Module FL_SetUp
!
! !END
!
!******************************************************************************
!
!
      USE FULIOUMULTI, ONLY: fi, fo, focc, fsfc, fouv, rad_multi_fu, &
                             set_default_options_fu,ftoa, fos
      USE FL_IO_Params
!      USE MATCH_PROFILES, ONLY: Match_Vert_Read
      USE MATCH_ED4v13 ,ONLY: mpro ,get_mpro_gridboxhr, print_mpro_gridboxhr

      USE nclouds_params  !HG-2025.0722
      
      USE Profile_Params
      USE SARB_VAR
      USE SARBAer_Var, ONLY: sh_aer_rgc
      USE SARBInput_Params, ONLY: CERSYN, Match_Vert_Available
      USE MonMODISAer_Params, ONLY: wlout

      USE EXTRAS , ONLY : aer_scale_hgt	
      USE SYNI_OVERLAP, ONLY: so ,overlap_four_syni ,qsortd
      USE FUINPUT ,ONLY: check_inputs_fu
      USE COMMON_ACCESS ,ONLY : TSI, MOA,HrBox, RegionNum_CERES
      USE TLUXS ,ONLY : TLU, T_Q_profile_adjust, print_tlu
	
      IMPLICIT NONE
!
      CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE FL_Pass_Drv ( IPass,                                    & ! I
                                      IRT_Error)                           ! O
!
!******************************************************************************
!
! !F90
!
! !Name:
!   FL_Pass_Drv
!
! !Description:
!    Routine ID- 
!
!    Purpose - Drive the calls for each requested sky condition for the 
!              current pass through the Fu-Liou model  
!             
!
! !Input Parameters:
!   IPass - Index for the current pass through the Fu-Liou Model
!
! !Output Parameters:
!   IRT_Error - Status code returned from Fu-Liou model
!                 = 0 ; Successful processing through model
!                 > 0 ; Error condition encountered 
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params   - (SARBlib) Parameters: LWProf*, SWProf*, WNProf*
!    Profile_Params - (SARBlib) Parameters: MaxLev_SARB, NumSkyCond, NumDir
!
!  Internal Routines Used:
!    FL_Call 
!
! !Revision History:
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Removed assignments and subroutine calls concerning different
!                cloud conditions
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code - broken out of Module FL_SetUp
!
! !END
!
!******************************************************************************
!
!
      IMPLICIT NONE
	
      INTEGER, INTENT (IN)  :: IPass
      INTEGER, INTENT (OUT) :: IRT_Error
!
      IRT_Error = 0
      LWProf ( IPass, 1:NumSkyCond, 1:NumDir, 1:MaxLev_SARB) = 0.0
      SWProf ( IPass, 1:NumSkyCond, 1:NumDir, 1:MaxLev_SARB) = 0.0
      WNProf ( IPass, 1:NumSkyCond, 1:NumDir, 1:MaxLev_SARB) = 0.0
!
      TotLWrad ( IPass, 1:NumSkyCond)     = 0.0
      WNrad_RAW ( IPass, 1:NumSkyCond)    = 0.0
      WNrad_SatEm ( IPass, 1:NumSkyCond)  = 0.0
      WNflx_SatEm ( IPass, 1:NumSkyCond)  = 0.0
      WNurad_SatEm ( IPass, 1:NumSkyCond) = 0.0
!
      !

      !      print *, 'IPass ', IPass

      
      CALL FL_Call (AerOptDepth ( IPass), IPass,                        & ! I
                    SpSfcAlb ( IPass, 1:Num_BndSA, 0:0),                & ! I
                                                             IRT_Error)   ! O
!
!
      END SUBROUTINE FL_Pass_Drv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE FL_Call ( AOT, IPass, SfcAlb,                          & ! I
                                                         IRT_Error)       ! O
!
!******************************************************************************
!
! !F90
!
! !Name:
!
! !Description:
!   Routine ID -
!
!   Purpose - Calls the Fu-Liou radiative transfer model
!
!
! !Input Parameters:
!    AOT        - Total column aerosol optical thickness
!    IPass      - Index into the vertical flux profile arrays for the current
!                 pass through the Fu-Liou Model
!    ISky       - Index into the vertical flux profile arrays for the current
!                 sky condition
!    SfcAlb     - Array of spectral surface albedos
!
! !Output Parameters:
!    IRT_Error - Status code returned from Fu-Liou model
!                 = 0 ; Successful processing through model
!                 > 0 ; Error condition encountered 
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    None
! 
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params  - (SARBlib) Parameters:  AerOptDepth, AerRatio, 
!                                           a_taus, a_taus_temp, 
!                                           Diffuse*, DirDiff*, Direct*, 
!                                           FL_TOA_LW*, FL_TOA_SW*,
!                                           FL_TOA_WN*, 
!                                           Instr_Idx, itps, itp_scene,
!                                           LW_Prof*, ModLev,
!                                           ModLev_Cnt, ModLev_ProO3,  
!                                           ModLev_SpHum, ModLev_Temp,
!                                           nac, n_atau, PAR*, 
!                                           pts, SfcEmiss, sh_aer,
!                                           SpSfcAlb, ss,
!                                           SW_Prof*, TotLWrad*, u0, 
!                                           ur, WN_Prof*, WNrad_RAW*, 
!                                           WNflx_SatEm*, WNrad_SatEm*, 
!                                           WNurad_SatEm*
!    FULIOUMULTI        - (SARBlib) Subroutine: rad_multi_fu, 
!                                               set_default_options_fu
!                                   Structures: fi, fo, focc, fsfc, fouv 
!    MonMODISAer_Params - (SARBlib) Parameters: wlout
!    Profile_Params     - (SARBlib) Parameters: Cld_Id1, Cld_Id2, 
!                                               Cldlyr_Aer, Cldlyr_NoAer, 
!                                               ClrSky,ConstrPass1, 
!                                               CS_NoAer, 
!                                               NoAer_Cld1, NoAer_Cld2, 
!                                               DnDir, InitPass, 
!                                               MaxLev_Cld, 
!                                               Num_BndSA, 
!                                               NumSkyCond_NewFu, 
!                                               NumSkyCond, 
!                                               Num_BndSfcEm, PrsSky, 
!                                               TOALev_Idx, TotSky, UpDir
!    SARBInput_Params   - (SARBlib) Parameters: CERSYN, 
!                                               Match_Vert_Available
!    SARB_VAR           - (SARBlib) Parameters: Match_Vert_AvailFlag, 
!                                               OptDepth_mn, 
!                                               OptDepthLin_mn, Phase_mn
!
!  External Routines Called:
!    None
!
! !Revision History:
!  Version 5.1   May 27, 2020 - SCCR #1657
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Added changes Entropy, pristine, and tune flags
!  Version 5.0   June 23, 2014 - SCCR #1017  
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Edition 4 updates to cloud properties and overlap 
!  Version 4.1   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Assign fi%ur = 1.0 when running Synoptic 
!                   to assure initalization of variables 
!  Version 4.0   June 10, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Updated code to interface new SARB UV variables with 
!                   new UV outputs from FuLiou model
!                2. Corrected assignment of aerosol constituent wavelength 
!                   inputs 
!                3. Added assignment of uncorrected SW fluxes for SYN 
!                   correction routine 
!                4. Simplified output assignment for cloud layers
!                5. Added use of vertical MATCH aerosols for SYN
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Modified code to interface SARB inputs and outputs with 
!                appropriate parameters in new FuLiou model
!  Version 3.3   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code - broken out of Module FL_SetUp
!
! !END
!
!******************************************************************************
!
        !

        USE SYNSPECTRAL, ONLY: sg   !! HG-20250917

      IMPLICIT NONE
	
      REAL,    INTENT (IN)  :: AOT
      INTEGER, INTENT (IN)  :: IPass
      INTEGER, INTENT (OUT) :: IRT_Error
      REAL,    INTENT (IN)  :: SfcAlb ( Num_BndSA)
!
      INTEGER :: NLev
      INTEGER :: ISky, I, J, iac, iwv, Cldlyr, Aerflg
      INTEGER :: ii ! 20131101
!      REAL :: Adjlntau ( MaxLev_Cld)
      REAL :: Adjlntau (nclouds)  ! HG-2025.0722
      REAL :: SWTOAUP_UN, SWSFCDN_UN
      REAL :: co2, ch4, n2o 

      INTEGER isp(nclouds),isp1,isp0,iov0,iov1
      INTEGER ispI(nclouds)   ! HG-2025.0722
      
!      INTEGER isp(4),isp1,isp0,iov0,iov1
!      INTEGER ispI(4)
      REAL :: AR1 ,RADJ1 !20130927 

      !print *, 'MaxLev_Cld, ', MaxLev_Cld
      IRT_Error = 0
      NLev = ModLev_Cnt ( IPass)
!
      CALL set_default_options_fu
      CALL Sarb_Tracegas_Value(co2,ch4,n2o)
      
      IF ( .NOT. Pristine)  fi%lscm(3:4) = .false. 
      
      fi%entropy = Entropy
      fi%pristine = Pristine
      fi%tuning = Tuning
      
      fi%umco2 = co2
      fi%umch4 = ch4
      fi%umn2o = n2o
!
! *** Map our variables into Freds input structure (fi)
!
      fi%nv    = NLev - 1
      fi%u0    = u0
      fi%ur    = ur
     
     fi%ur    = 1.0
      
      fi%ee(1:Num_BndSfcEm) = SfcEmiss ( IPass, 1:Num_BndSfcEm)

      fi%ss    = ssrna
      fi%pts   = pts ( IPass)

      fi%pp (1:NLev)    = ModLev ( IPass, 1:NLev)
      fi%pt (1:NLev)    = ModLev_Temp ( IPass, 1:NLev)
      fi%ph (1:NLev)    = ModLev_SpHum ( IPass, 1:NLev)
      fi%po (1:NLev)    = ModLev_ProO3 ( IPass, 1:NLev)


      fi%hsfc = MOA % SfcGeoPt

!--- Match Aerosol /Profile/Constituent
      mpro%rlon = RLong !!! Longitude ( 0 : 360)
      mpro%nv1  = ModLev_Cnt (IPass)  !!  # of LEVELS in model profile ( fi%nv +1 ) 
      mpro%pp(1:mpro%nv1) = ModLev ( IPass, 1:ModLev_Cnt (IPass))

      CALL get_mpro_gridboxhr !!! get AOTs and Scaled fractional Profile data for selected (lat.lon)
  
      fi%n_atau             = 1         !  Match 550nm     
      fi%nac    	    = mpro%npc          
      fi%itps(1:fi%nac)     = mpro%ityp_comb(1:mpro%npc) 
        
      fi%a_wli(1:fi%n_atau) = 0.550 
	
! *** Define column aerosol properties

!      a_taux = AerOptDepth ( IPass)
!      IF (AerOptDepth ( IPass) .gt. 10.0) a_taux = 0.0       
      
      fi%a_taus(1,1:fi%nac) = mpro%aots_comb(1:fi%nac) * AerRatio(IPass)
      WHERE(fi%a_taus <= 1E-10 ) fi%a_taus = 1E-10

      fi%aprofs(1:fi%nv, 1:fi%nac) = mpro%mvp_comb(1:fi%nv,1:mpro%npc)


! *** Set instrument id number

      fi%instrument = Instr_Idx - 1

! *** Solver set ups

      fi%HYBRID_SW_SOLVER = .true.
      fi%fourssl = .FALSE. ! SW Fourstream
      fi%foursir = .FALSE. ! IR Four_Stream
      fi%isksolve = 1 !0=Original Fu Solver 1=Kato Solver

!
! *** Define cloud properties
!
!      WHERE ( CF(IPass,1:4) > 0 .AND. OptDepth_mn(IPass,1:4)	> 512. ) OptDepth_mn(IPass,1:4) = 512.
!      WHERE ( CF(IPass,1:4) > 0 .AND. OptDepthLin_mn(IPass,1:4) > 512. ) OptDepthLin_mn(IPass,1:4) = 512.
      WHERE ( CF(IPass,1:nclouds) > 0 .AND. OptDepth_mn(IPass,1:nclouds)	> 512. ) OptDepth_mn(IPass,1:nclouds) = 512.
      WHERE ( CF(IPass,1:nclouds) > 0 .AND. OptDepthLin_mn(IPass,1:nclouds) > 512. ) OptDepthLin_mn(IPass,1:nclouds) = 512.

      
      !   Ed4 Cloud Overlap

      !print *, 'fi%pp'
      !print *, fi%pp
      !print *, 'fi%pt'
      !print *, fi%pt
      
      
      so%cfi =0.0
      so%cfi(0) =  CF(IPass, 0)
!      DO i =1,4
      DO i =1,nclouds ! HG-2025.0722
        so%ptop(i)=1E36
        IF ( CF(IPass, i) > 0.0 .AND. CF(IPass, i) < 1.01) THEN
	  so%cfi(i) =  CF(IPass, i)  ! Cloud Fraction (0-1)
	  so%ptop(i) = fi%pp( CCLyr_Idx ( IPass, i, 1) )	
          so%pbot(i) = fi%pp( CCLyr_Idx ( IPass, i, 2) )
   
        !print *, 'so%cfi(i), ', so%cfi(i)
        !print *, 'so%ptop(i), ', so%ptop(i)  !need to check
        !print *, 'so%pbot(i), ', so%pbot(i)
        
	  IF ( so%pbot(i) <= so%ptop(i) ) THEN  !!! CORRECT CLOUD BOT PRESSURE
	    so%pbot(i) = so%ptop(i) + 2.0 !! Pbot = Ptop +2
	  ENDIF
        ENDIF
      ENDDO	

      !print *, 'so%ptop, ', so%ptop
      !print *, 'so%pbot, ', so%pbot
      
      !stop
!    Begin Cloud WG CLapse Rate Method inputs used to adjust lower level T,Q Profile 

      !print *, 'here-500 CF(Ipass,0) ', CF(Ipass,0)   ! check clear sky fraction, avoid negatie value
      IF  ( Ipass == 1 .AND. CF(Ipass,0) >= 0.0 .AND. CF(Ipass,0) < 0.95 ) THEN ! Clear fraction of Untuned is less than 0.95 ic cld frac> 0.05 ) 
        tlu%ierr = -10 ! initalize
        tlu%pamin = 650.
!        tlu%pm(1:4) = 1E+30
!        tlu%tm(1:4) = 1E+30
        tlu%pm(1:nclouds) = 1E+30   !! HG-2025.0722
        tlu%tm(1:nclouds) = 1E+30

        
        DO i=1,nclouds 
           !        DO i=1,4
                    !print *, 'here-200 i= ', i

           !print *, 'tsi%cld_layer(i)%areafrac_total ', tsi%cld_layer(i)%areafrac_total
           !print *, 'tsi%cld_layer(i)%optdepth_mn_lnr ', tsi%cld_layer(i)%optdepth_mn_lnr
           !print *, 'tsi%cld_layer(i)toppres_mn ', tsi%cld_layer(i)%toppres_mn

           !HG-20250826, modify 5% cloud to 0 for testing 9 cloud set
          !IF (  tsi%cld_layer(i)%areafrac_total > 5.0 .AND. tsi%cld_layer(i)%areafrac_total <= 100.00 .AND. &
          IF (  tsi%cld_layer(i)%areafrac_total >= 0.0 .AND. tsi%cld_layer(i)%areafrac_total <= 100.00 .AND. &
          ! tsi%cld_layer(i)%optdepth_mn_lnr > 0.5 .AND. tsi%cld_layer(i)%optdepth_mn_lnr < 512.0 .AND. &
           tsi%cld_layer(i)%optdepth_mn_lnr >= 0.0 .AND. tsi%cld_layer(i)%optdepth_mn_lnr < 5120.0 .AND. &  !HG-20250908
	        tsi%cld_layer(i)%toppres_mn > tlu%pamin .AND. tsi%cld_layer(i)%toppres_mn < 1100 ) THEN
            tlu%pm(i) = tsi%cld_layer(i)%toppres_mn ! Set Cloud Top Pressures
            tlu%tm(i) = tsi%cld_layer(i)%toptemp_mn ! Set Cloud Top Temperatures
            tlu%ierr = 0
            !print *, 'here-100, tsi%cld_layer(i)%toppres_mn, ', tsi%cld_layer(i)%toppres_mn
            !print *, 'tsi%cld_layer(i)%toptemp_mn, ', tsi%cld_layer(i)%toptemp_mn
         ENDIF
         !print '(A14, I3)', 'here-200 HrBox', HrBox
         !print *, 'tlu%pm, ', tlu%pm(i)
         !print *, 'tlu%tm, ', tlu%tm(i)
!         print *, 'tlu%ierr, ', tlu%ierr
!       stop  
      ENDDO

      !stop
      
     
        !print *, '111. tlu%ierr, ', tlu%ierr

        IF ( tlu%ierr == 0 ) THEN 
          tlu%nlev = fi%nv+1
          tlu%pp(1:tlu%nlev) = fi%pp(1:fi%nv+1)
          tlu%pt(1:tlu%nlev) = fi%pt(1:fi%nv+1)
          tlu%pq(1:tlu%nlev) = fi%ph(1:fi%nv+1)
          tlu%padjrange = 100. ! Set pressure range(hPa) above cloud top for temperature adjustment
          CALL T_Q_profile_adjust  ! Execute the Adjustment procedure. may return non-zero tlu%ierr
         ! print *, 'check tlu%ierr after T_Q', tlu%ierr
         !            print *, 'HrBox = ', HrBox

          !CALL print_tlu  ! Print Before and After profiles.

        ENDIF


        !print *, '222, tlu%ierr, ', tlu%ierr
!        stop
        
        IF (tlu%ierr == 0 ) THEN
!!!!! HG 20250819 ************************** when there are split clouds, this part does not work.
!!!!! check T_Q_profile_adjust part, after, tlu%ierr changes
!!!!! this part is to update original atmospheric profile
           
         !  print *, 'print_tlu ????? '
          ! print *, 'HrBox = ', HrBox
        ! CALL print_tlu  ! Print Before and After profiles.
          fi%pt(1:fi%nv+1) = tlu%pt(1:tlu%nlev)
          fi%ph(1:fi%nv+1) = tlu%pq(1:tlu%nlev)
          ModLev_Temp ( IPass, 1:NLev)   =   fi%pt (1:NLev)   
          ModLev_SpHum ( IPass, 1:NLev)  =   fi%ph (1:NLev)
        ENDIF
      ENDIF !! Inital Clear fraction check
   
      !    END Cloud WG CLapse Rate Method inputs used to adjust lower level T,Q Profile


      !print *, 'so%ptop, ', so%ptop

      
      IF ( Ipass == 1 ) THEN 

!       CALL qsortd(so%ptop,so%isp,4) ! GET SORTING INDEX to sort by cloud top!
       CALL qsortd(so%ptop,so%isp,nclouds) ! HG-2025.0722
       
       isp=so%isp

      !print *, 'so%isp, ', so%isp

      !stop
      
!       so%cfi(1:4)   = so%cfi(isp)
!       so%ptop(1:4)  = so%ptop(isp)
!       so%pbot(1:4)  = so%pbot(isp)
!       so%tau(1:4,1) = OptDepth_mn(IPass,isp)
!       so%tau(1:4,2) = OptDepthLin_mn(IPass,isp)

       so%cfi(1:nclouds)   = so%cfi(isp)
       so%ptop(1:nclouds)  = so%ptop(isp)
       so%pbot(1:nclouds)  = so%pbot(isp)
       !print *, ' '
       !print *, 'check505 ', 'so%tau(1:nclouds,1) = OptDepth_mn(IPass,isp)', &
       !     'so%tau(1:nclouds,2) = OptDepthLin_mn(IPass,isp)'
       !print *, 'isp', isp
       !print *, so%tau(1:nclouds,1)
       !print *, OptDepth_mn(IPass,isp)
       !print *, so%tau(1:nclouds,2)
       !print *, OptDepthLin_mn(IPass,isp)
       !print *, ' '
       
       so%tau(1:nclouds,1) = OptDepth_mn(IPass,isp)
       so%tau(1:nclouds,2) = OptDepthLin_mn(IPass,isp)    !! HG-2025.0722

       !! HG-20250823, tau changed before here
       !print *, 'check503'
       !print *, 'so%cfi(1:nclouds) ', so%cfi(1:nclouds) ! so%cfi(0) -- clear sky %?
       !print *, 'so%ptop(1:nclouds) ', so%ptop
       !print *, 'so%pbot(1:nclouds) ', so%pbot
       !print *, 'so%tau(1:nclouds,1) ', so%tau(:,1)
       !print *, 'so%tau(1:nclouds,2) ', so%tau(:,2)

       
       !stop
       
       CALL overlap_four_syni

       !print *, ' '
       !print *, 'check504'
       !print *, 'after overlap_four_syni'
       !print *, 'so%cfi(1:nclouds) ', so%cfi(1:nclouds) ! so%cfi(0) -- clear sky %?
       !print *, 'so%ptop(1:nclouds) ', so%ptop
       !print *, 'so%pbot(1:nclouds) ', so%pbot
       !print *, 'so%tau(1:nclouds,1) ', so%tau(:,1)
       !print *, 'so%tau(1:nclouds,2) ', so%tau(:,2)
       
       
      ELSEIF ( Ipass == 2 ) THEN
      
         isp=so%isp
!         so%tau(1:4,1) = OptDepth_mn(IPass,1:4)
!         so%tau(1:4,2) = OptDepthLin_mn(IPass,1:4)

         so%tau(1:nclouds,1) = OptDepth_mn(IPass,1:nclouds)  !! HG-2025.0722
         so%tau(1:nclouds,2) = OptDepthLin_mn(IPass,1:nclouds)
        
      ENDIF


!      ispI= 5- isp ! Flip order for SYNI outputs

      ispI= (nclouds+1)- isp ! HG-2025.0722
      !print *, 'isp, ', isp      
      !print *, 'ispI, ', ispI

      fi%sfcalb(1:Num_BndSA,1,0) = SpSfcAlb_Clear ( IPass, 1:Num_BndSA, 0) 
      fi%sfcalb(1:Num_BndSA,2,0) = SpSfcAlb_Pris  ( IPass, 1:Num_BndSA, 0) 

      fi%fc%cldfrac       = 0.0 !20131101
      
!      DO i=1,4
      DO i=1, nclouds !! HG-2025.0722
       fi%fc(i)%tau_vis       = 0.0 !20141114
       fi%fc(i)%sc%mn_lin_tau = 0.0 !20141114     
      ENDDO
      
      DO isp0 = 1, so%nnz !!! MaxLev_Cld  ! 20131101   
       
         fi%fc(isp0)%cldfrac   = so%Fcnd(so%IPcnd(isp0))            
       
         IF ( Ipass == 2 ) fi%fc(isp0)%cldfrac = CF(Ipass,isp0 )
       
         IF ( fi%fc(isp0)%cldfrac > 0.0 ) THEN ! Valid Cloud Fraction

	 fi%fc(isp0)%novl = so%novl( so%IPcnd(isp0) ) !! count(so%BT(so%IPcnd(isp0),1:4) )

!         isp1 =isp(isp0) !!! SORTING INDEX
!	 IF ( Ipass == 2 ) isp1 = isp0
	 
  	 iov1=0
	
	 AR1   = -9999.0 !20130927 
	 RADJ1 = 1.0 !20130927 
	
	 OVERLAPS: DO iov0 = 1, nclouds ! 4, HG-2025.0722	
	   
	   IF ( so%BT( so%IPcnd(isp0),iov0)  ) THEN
	     iov1=iov1+1
	     isp1 =isp(iov0) !!! SORTING INDEX on Overlap	

!  Surface Albedo	

             IF (iov1 == 1   ) THEN	      
              fi%sfcalb(1:Num_BndSA,1,isp0) = SpSfcAlb_Clear ( IPass, 1:Num_BndSA, isp1) 
              fi%sfcalb(1:Num_BndSA,2,isp0) = SpSfcAlb_Pris  ( IPass, 1:Num_BndSA, isp1) 
             ENDIF


!  Cloud Top & Bottom

	     IF ( ipass == 1 ) THEN 
              fi%fc(isp0)%icld_top(iov1) = CCLyr_Idx ( IPass, isp1, 1)   
              fi%fc(isp0)%icld_bot(iov1) = CCLyr_Idx ( IPass, isp1, 2)
              !print *, 'fi%fc(isp0)%icld_top(iov1)', fi%fc(isp0)%icld_top(iov1)  ! 7, 8, 9 cloud temperature adjusted till here
              !print *, 'fi%fc(isp0)%icld_bot(iov1)', fi%fc(isp0)%icld_bot(iov1) 
              !print *, 'fi%pt', fi%pt(fi%fc(1:nclouds)%icld_top(1))
              !print *, 'fi%pt', fi%pt(fi%fc(1:nclouds)%icld_top(iov1))
              !print *, 'fi%pt', fi%pt(fi%fc(isp0)%icld_top(1))
              !print *, 'fi%pt', fi%pt(fi%fc(isp0)%icld_top(iov1))

              !!!!!! HG-20250819, cloud temperatures changed here.
              
	     ELSEIF (ipass == 2 .AND. iov1 == 1 ) THEN                  ! For Tuned use cloud index of tuned overlap sorted top layer
              fi%fc(isp0)%icld_top(iov1) = CCLyr_Idx ( Ipass, isp0, 1)   
              fi%fc(isp0)%icld_bot(iov1) = CCLyr_Idx ( Ipass, isp0, 2) 
	     ELSE                                                       ! Find Lowest cloud for remaining tuned overlap under layers.
!	      fi%fc(isp0)%icld_top(iov1) = maxval(CCLyr_Idx ( ipass, 1:4, 1) , mask = CCLyr_Idx ( ipass, 1:4, 1) < 100)
!	      fi%fc(isp0)%icld_bot(iov1) = maxval(CCLyr_Idx ( ipass, 1:4, 2) , mask = CCLyr_Idx ( ipass, 1:4, 2) < 100) 
	      fi%fc(isp0)%icld_top(iov1) = maxval(CCLyr_Idx ( ipass, 1:nclouds, 1) , mask = CCLyr_Idx ( ipass, 1:nclouds, 1) < 100)
	      fi%fc(isp0)%icld_bot(iov1) = maxval(CCLyr_Idx ( ipass, 1:nclouds, 2) , mask = CCLyr_Idx ( ipass, 1:nclouds, 2) < 100) 
	     ENDIF

      
!  PartSize & Phase

             IF ( Phase_mn (isp1) <= 1.5 .AND. Phase_mn (isp1) /= Default_REAL4_FL) THEN       
              fi%fc(isp0)%rphase(iov1)  = 1                            !   Cloud Phase 1=Water 2=Ice
              fi%fc(isp0)%re(iov1)      = LiquidPart_size ( 1, isp1)
	      IF ( AR1 > 0 ) radj1 = Radj(AR1, fi%fc(isp0)%re(iov1) )  ! 20130927
             ELSEIF ( Phase_mn (isp1) > 1.5 .AND. Phase_mn (isp1) /= Default_REAL4_FL) THEN 
              fi%fc(isp0)%rphase(iov1)  = 2                            ! Cloud Phase 1=Water 2=Ice
              fi%fc(isp0)%de(iov1) = IcePart_size ( 1, isp1)
              fi%txt=2                                                 ! Fu2007 Rough
              fi%fc(isp0)%asp(iov1) =de_asp(IcePart_size ( 1, isp1))
              radj1 = 1.00                                             ! 20130927
              if (AR1 < 0 )AR1 = fi%fc(isp0)%asp(iov1)! 20130927
             ENDIF



             IF (IPass == InitPass) THEN   ! Untuned
!               fi%fc(isp0)%tau_vis(iov1)       =   so%oprf(so%IPcnd(isp0),iov0, 1) *Radj1 !20130927 ! exp of SSF 85
!               fi%fc(isp0)%sc(iov1)%mn_lin_tau =   so%oprf(so%IPcnd(isp0),iov0, 2) *Radj1 !20130927 ! SSF 83

               ! HG-2025-0723
               fi%fc(isp0)%tau_vis(iov1)       =   so%oprf(so%IPcnd(isp0), 1) *Radj1 !20130927 ! exp of SSF 85
               fi%fc(isp0)%sc(iov1)%mn_lin_tau =   so%oprf(so%IPcnd(isp0), 2) *Radj1 !20130927 ! SSF 83

               !print *, 'check501-----------------------'
               !print *, 'fi%fc(isp0)%tau_vis(iov1)       =   so%oprf(so%IPcnd(isp0), 1) *Radj1 ', &
               !     fi%fc(isp0)%tau_vis(iov1), so%oprf(so%IPcnd(isp0), 1), Radj1
               !print *, 'fi%fc(isp0)%sc(iov1)%mn_lin_tau =   so%oprf(so%IPcnd(isp0), 2) *Radj1 ', &
               !     fi%fc(isp0)%sc(iov1)%mn_lin_tau, so%oprf(so%IPcnd(isp0), 2), Radj1

	       if ( fi%fc(isp0)%sc(iov1)%mn_lin_tau < fi%fc(isp0)%tau_vis(iov1) )&
	                 fi%fc(isp0)%sc(iov1)%mn_lin_tau = fi%fc(isp0)%tau_vis(iov1)
             ENDIF  ! Untuned


             IF (IPass == ConstrPass1) THEN  ! Tuned
               IF (OptDepth_mn( IPass, isp0) == 0.0 .OR. &
                   OptDepth_mn( InitPass, isp1) == 0.0) THEN
                 Adjlntau(isp0) = Default_REAL4_FL
               ELSE
                 Adjlntau(isp0)  = log(OptDepth_mn( IPass, isp0)) -  log(OptDepth_mn( InitPass, isp0))
               ENDIF  
!         
               IF (OptDepthLin_mn ( InitPass, isp1) == 0.0 .OR. &
                   Adjlntau(isp0) == Default_REAL4_FL) THEN
                 OptDepthLin_mn ( IPass, isp0) = 0.0
               ELSE
                 OptDepthLin_mn ( IPass, isp0) = &
                 EXP( log (OptDepthLin_mn ( InitPass, isp1)) +  Adjlntau(isp0) )
               ENDIF
	 
!               fi%fc(isp0)%tau_vis(iov1)       = OptDepth_mn( IPass, isp0)* RADJ1 * & ! 20131101
!                                                 so%oprf(so%IPcnd(isp0),iov0, 1)/ sum(so%oprf(so%IPcnd(isp0),1:nclouds, 1))
!               fi%fc(isp0)%sc(iov1)%mn_lin_tau = OptDepthLin_mn ( IPass, isp0)* RADJ1 * & ! 20131101
!                                                 so%oprf(so%IPcnd(isp0),iov0, 2)/ sum(so%oprf(so%IPcnd(isp0),1:nclouds, 2))

               !HG-2025-0723
               fi%fc(isp0)%tau_vis(iov1)       = OptDepth_mn( IPass, isp0)* RADJ1
               fi%fc(isp0)%sc(iov1)%mn_lin_tau = OptDepthLin_mn ( IPass, isp0)* RADJ1 

!               fi%fc(isp0)%tau_vis(iov1)       = OptDepth_mn( IPass, isp0)* RADJ1 * & ! 20131101
!                                                 so%oprf(so%IPcnd(isp0),iov0, 1)/ sum(so%oprf(so%IPcnd(isp0),1:4, 1))
!               fi%fc(isp0)%sc(iov1)%mn_lin_tau = OptDepthLin_mn ( IPass, isp0)* RADJ1 * & ! 20131101
!                                                 so%oprf(so%IPcnd(isp0),iov0, 2)/ sum(so%oprf(so%IPcnd(isp0),1:4, 2))

	       IF ( fi%fc(isp0)%sc(iov1)%mn_lin_tau < fi%fc(isp0)%tau_vis(iov1) )&
	           fi%fc(isp0)%sc(iov1)%mn_lin_tau = fi%fc(isp0)%tau_vis(iov1)
             ENDIF  ! Tuned

           ENDIF
         ENDDO OVERLAPS
    
         ENDIF ! Valid Cloud Fraction

               !print *, 'fi%fc(isp0)%tau_vis ', fi%fc(isp0)%tau_vis

      END DO   !!! MaxLev_Cld 
!
!print *, '-------------------testing---------------'
      
!if (HrBox == 636) then
!    stop "HrBox reached 636"
!end if

      
      CALL check_inputs_fu
      
!      CALL print_in_fu

      
! *** Call Fu-Liou radiative transfer model

      IF (fi%ierror == 0 ) CALL rad_multi_fu

!     CALL print_out_fu
		
      ! *** Pull off outputs
      ! HG-20250917
!print *, "100 DEBUG: sg%bba_source_flag (before output) =", sg%bba_source_flag
!IF (sg%bba_source_flag > 1000) print *, "606, sg%bba_source_flag = ", sg%bba_source_flag

      
      CALL Prep_Output_SYNI_ED4(IPass,ispI)
!print *, "100 DEBUG: sg%bba_source_flag (after output) =", sg%bba_source_flag
!IF (sg%bba_source_flag > 1000) print *, "607, sg%bba_source_flag = ", sg%bba_source_flag


! *** Error handling
      IF (fi%ierror /= 0) THEN
       IRT_Error = fi%ierror
       RETURN
      ENDIF

!      CALL print_out_fu

!
! *** NOTE: For Edition4 SYNI, this section no longer sets the SYNI output variables.
!           However, these variables are used in verifying that outputs are
!           within acceptable limits, so this section will remain. 
!
      DO ISky = 1, NumSkyCond
	
       IF (ISky == PrsSky .OR. ISky == ClrSky .OR. &
           ISky == TotSky .OR. ISky == CS_NoAer) THEN  ! - use fo data structure
!
          I = NumSkyCond_NewFu (ISky)  ! Map to one of four values of nscm
       
          SWProf ( IPass, ISky, UpDir, 1:NLev) = fo(I)%fus (1:NLev)                
          SWProf ( IPass, ISky, DnDir, 1:NLev) = fo(I)%fds (1:NLev)               
          LWProf ( IPass, ISky, UpDir, 1:NLev) = fo(I)%fuir (1:NLev)               
          LWProf ( IPass, ISky, DnDir, 1:NLev) = fo(I)%fdir (1:NLev)               
          WNProf ( IPass, ISky, UpDir, 1:NLev) = fo(I)%fuwn (1:NLev)               
          WNProf ( IPass, ISky, DnDir, 1:NLev) = fo(I)%fdwn (1:NLev)               
!
          TotLWrad ( IPass, ISky)     = fo(I)%fiurt ( TOALev_Idx)
          WNrad_RAW ( IPass, ISky)    = fo(I)%fiurw ( TOALev_Idx)
          WNrad_SatEm ( IPass, ISky)  = fo(I)%trwn_flt_r                          
          WNurad_SatEm ( IPass, ISky) = fo(I)%trwn_unf_r
          WNflx_SatEm ( IPass, ISky)  = fo(I)%trwn_f                         
          Dir ( IPass, ISky)          = fo(I)%fdsdr(fi%nv+1)
          Diff ( IPass, ISky)         = fo(I)%fdsdf(fi%nv+1)            
          PAR ( IPass, ISky)          = fsfc(I)%swpar
          Dir_SW_Sfc ( IPass, ISky)   = fsfc(I)%dirsfc
          Diff_SW_Sfc ( IPass, ISky)  = fsfc(I)%difsfc
!
          DnUVB_TOA( IPass, ISky) = fouv(I)%toa_uvb 
          DnUVA_TOA( IPass, ISky) = fouv(I)%toa_uva 
          DnPAR_TOA( IPass, ISky) = fouv(I)%toa_par 
!
          DnUVB_Sfc ( IPass, ISky)       = fouv(I)%uvb       
          DirDiff_UVB_Sfc ( IPass, ISky) = fouv(I)%uvb_rdirdif
          DnUVA_Sfc ( IPass, ISky)       = fouv(I)%uva       
          DirDiff_UVA_Sfc ( IPass, ISky) = fouv(I)%uva_rdirdif 
          PAR_Sfc ( IPass, ISky)         = fouv(I)%par        
          DirDiff_PAR_Sfc ( IPass, ISky) = fouv(I)%par_rdirdif
          Par_Purve_SW ( IPass, ISky)    = fouv(I)%par_purv
          DirDiff_Par_Purv( IPass, ISky) = fouv(I)%par_purv_rdirdif
          Par_ChlorA_SW ( IPass, ISky)   = fouv(I)%par_chla
          DirDiff_Par_ChlorA(IPass,ISky) = fouv(I)%par_chla_rdirdif 
!          ActFlx_Sfc ( IPass, ISky) = fouv(I)%actinic 
!          fouv(I)%actinic no longer contained in model
          ActFlx_Sfc ( IPass, ISky) = Default_REAL4_FL     
!
          IdxUV_Sfc ( IPass, ISky) = fouv(I)%uvindex 
!
          IF (IRT_Error == 0) THEN
!
           FL_TOA_LW ( IPass, ISky) = LWProf ( IPass, ISky, UpDir, TOALev_Idx) 
           FL_TOA_SW ( IPass, ISky) = SWProf ( IPass, ISky, UpDir, TOALev_Idx)  
           FL_TOA_WN ( IPass, ISky) = WNflx_SatEm ( IPass, ISky)     
!
           IF (ABS (Diff ( IPass, ISky)) > 0.0) THEN
!         *** Inputs to the Direct/Diffuse Ratio are valid
!
            DirDiff ( IPass, ISky) =   Dir ( IPass, ISky)    &
                                    / Diff ( IPass, ISky)
!  
           END IF
!  
          ENDIF
!
       ELSE  ! Cloud layer parameters - use focc data structure
!
          IF (ISky < NoAer_Cld1) THEN
           Aerflg = 1
           Cldlyr = Cldlyr_Aer(ISky)
          ELSE 
           Aerflg = 2
           Cldlyr = Cldlyr_NoAer(ISky)
          ENDIF

	
          SWProf ( IPass, ISky, UpDir, 1:NLev) = focc(Cldlyr,Aerflg)%fus (1:NLev)                
          SWProf ( IPass, ISky, DnDir, 1:NLev) = focc(Cldlyr,Aerflg)%fds (1:NLev)               
          LWProf ( IPass, ISky, UpDir, 1:NLev) = focc(Cldlyr,Aerflg)%fuir (1:NLev)               
          LWProf ( IPass, ISky, DnDir, 1:NLev) = focc(Cldlyr,Aerflg)%fdir (1:NLev)               
          WNProf ( IPass, ISky, UpDir, 1:NLev) = focc(Cldlyr,Aerflg)%fuwn (1:NLev)               
          WNProf ( IPass, ISky, DnDir, 1:NLev) = focc(Cldlyr,Aerflg)%fdwn (1:NLev)               
!
          TotLWrad ( IPass, ISky)     = focc(Cldlyr,Aerflg)%fiurt ( TOALev_Idx)
          WNrad_RAW ( IPass, ISky)    = focc(Cldlyr,Aerflg)%fiurw ( TOALev_Idx)
          WNrad_SatEm ( IPass, ISky)  = focc(Cldlyr,Aerflg)%trwn_flt_r                          
          WNurad_SatEm ( IPass, ISky) = focc(Cldlyr,Aerflg)%trwn_unf_r
          WNflx_SatEm ( IPass, ISky)  = focc(Cldlyr,Aerflg)%trwn_f                         
          Dir ( IPass, ISky)          = focc(Cldlyr,Aerflg)%fdsdr(fi%nv+1)
          Diff ( IPass, ISky)         = focc(Cldlyr,Aerflg)%fdsdf(fi%nv+1)
          Dir_SW_Sfc ( IPass, ISky) = Default_REAL4_FL
          Diff_SW_Sfc (IPass,ISky)  = Default_REAL4_FL            
!
          DnUVB_TOA( IPass, ISky)   = Default_REAL4_FL 
          DnUVA_TOA( IPass, ISky)   = Default_REAL4_FL 
          DnPAR_TOA( IPass, ISky)   = Default_REAL4_FL 
!
          DnUVB_Sfc ( IPass, ISky)       = Default_REAL4_FL       
          DirDiff_UVB_Sfc ( IPass, ISky) = Default_REAL4_FL
          DnUVA_Sfc ( IPass, ISky)       = Default_REAL4_FL       
          DirDiff_UVA_Sfc ( IPass, ISky) = Default_REAL4_FL 
          PAR_Sfc ( IPass, ISky)         = Default_REAL4_FL        
          DirDiff_PAR_Sfc ( IPass, ISky) = Default_REAL4_FL 
          ActFlx_Sfc( IPass, ISky)       = Default_REAL4_FL
          Par_Purve_SW( IPass, ISky)     = Default_REAL4_FL
          DirDiff_Par_Purv ( IPass, ISky)  = Default_REAL4_FL
          Par_ChlorA_SW( IPass, ISky)      = Default_REAL4_FL
          DirDiff_Par_ChlorA( IPass, ISky) = Default_REAL4_FL       
!
          IdxUV_Sfc ( IPass, ISky) = Default_REAL4_FL 
!
          IF (IRT_Error == 0) THEN
!
           FL_TOA_LW ( IPass, ISky) = LWProf ( IPass, ISky, UpDir, TOALev_Idx) 
           FL_TOA_SW ( IPass, ISky) = SWProf ( IPass, ISky, UpDir, TOALev_Idx)  
           FL_TOA_WN ( IPass, ISky) = WNflx_SatEm ( IPass, ISky)     
!
           IF (ABS (Diff ( IPass, ISky)) > 0.0) THEN
!         *** Inputs to the Direct/Diffuse Ratio are valid
!
            DirDiff ( IPass, ISky) =   Dir ( IPass, ISky)    &
                                     / Diff ( IPass, ISky)
!  
           END IF
!  
          ENDIF   !  IRT_Error == 0
!
       ENDIF
!
      ENDDO  ! END of Sky conditions
!
      PAR ( IPass, TotSky) = fouv(TotSky)%par

! *** Assign inputs for SW correction for SYN 
!
      SW_TOAUP_UNCOR = SWProf ( IPass, PrsSky, UpDir, TOALev_Idx) 
      SW_SFCDN_UNCOR = SWProf ( IPass, PrsSky, DnDir, NLev)
!
! *** End of Edition2 output assignments.  Now used only for data validation in Edition4 SYNI 
!

   END SUBROUTINE FL_Call

!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   SUBROUTINE spoof_pack_sky   
!
!******************************************************************************
!
! !F90
!
! !Name:
!   spoof_pack_sky 
!
! !Description:
!    Routine ID- 
!
!    Purpose -  
!             
!
! !Input Parameters:
!   
!
! !Output Parameters:
!   
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)

!
!  Internal Routines Used:
!     
!
! !Revision History:
!  Version 1.0   June 23, 2014 - SCCR #1017 
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   USE CALIPSO_OUTPUT, ONLY : pack_sky,print_pack_sky,skyp,SKYP_TYPE

   REAL psel(6)
   INTEGER i,is

   fi%vd%nrep = 0
   fi%vo%ireport =0

   fi%vo%ireport(1)=1
   psel=(/0.1,70.,200.,500.,850.,fi%pp(fi%nv+1)/)
   IF ( fi%pp(fi%nv+1) < 850 ) psel=(/0.1,70.,200.,500.,fi%pp(fi%nv+1),9999./)
   IF ( fi%pp(fi%nv+1) < 500 ) psel=(/0.1,70.,200.,fi%pp(fi%nv+1),9999.,9999./)

   fi%vo%ireport(1)=1
   is=2
     DO i= 1,fi%nv+1
!      IF ( ABS(fi%pp(i) - psel(is) )  < 2.0 )  THEN
!Fred Rose 20150918
      IF ( ABS(fi%pp(i) - psel(is) )  < 5.0 .or. fi%pp(i) >= psel(is) + 5.0 )  THEN
       fi%vo%ireport(is) = i
       fi%vd%nrep =is
       if ( is == 6 ) exit   !!! Fred Rose 20150929 Prevent more than 6 reporting levels 
       is=is+1
      ENDIF
     ENDDO

   fi%vo%ireport( fi%vd%nrep  ) = fi%nv+1   !! Explicitly  extract THE SURFACE ! Fred Rose 20150925
 
   CALL pack_sky
!   CALL print_pack_sky
   
   END SUBROUTINE spoof_pack_sky
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   REAL FUNCTION Radj(AR,Re)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   Radj 
!
! !Description:
!    Routine ID- 
!
!    Purpose -  
!             
!
! !Input Parameters:
!   
!
! !Output Parameters:
!   
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)

!
!  Internal Routines Used:
!     
!
! !Revision History:
!  Version 1.0   June 23, 2014 - SCCR #1017 
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   REAL ,INTENT(IN):: AR, RE
   REAL pr0,pr1,pr2 ,fx
   REAL gx,gi
   REAL gw
   REAL huwp(4)

   DATA pr2 /1.659297E-02/
   DATA pr1 /7.070839E-02/
   DATA pr0 /4.653916E-01/
   DATA fx / 0.500005/
   DATA huwp /8.155450e-01,  7.271765e-03, -3.186095e-04,  4.807992e-06/ 

    gx = pr0 +pr1*log(AR) +pr2* (log(AR))**2 
    gi = fx + ( 1.0 - fx ) * gx
 
    gw  =  huwp(1) +  re*huwp(2)+ re*re*huwp(3) +  re*re*re*huwp(4)

    Radj = (1.0-gi)/(1.0-gw)

   END FUNCTION Radj
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   SUBROUTINE Prep_Output_SYNI_ED4 (IPass,isp)
!
!******************************************************************************
!
! !F90
!
! !Name:
!    Prep_Output_SYNI_ED4 
!
! !Description:
!    Routine ID- 
!
!    Purpose - Assigns Fu-Liou outputs to SYNI output variables 
!             
!
! !Input Parameters:
!   
!
! !Output Parameters:
!   
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)

!
!  Internal Routines Used:
!     
!
! !Revision History:
!  Version 1.0   June 23, 2014 - SCCR #1017 
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   USE ed4_syni_structure , ONLY : syni4 ,pfp ,syni4_print,SYNI4_NULL,nswb,nlwb
   USE CALIPSO_OUTPUT, ONLY : skyp
   USE SYNSPECTRAL ,ONLY :sg
    
   REAL aot_by_band
    
   COMMON /aotbyband/ aot_by_band(18)
   
   INTEGER ipass, kb ,k
!   INTEGER isp(4)
   INTEGER isp(nclouds)  !HG-2025.0722

   INTEGER :: ic_nclouds, iii

! HG-20250917   
!print *, "101 DEBUG: sg%bba_source_flag (at output) =", sg%bba_source_flag
!IF (sg%bba_source_flag > 1000) print *, "608, sg%bba_source_flag = ", sg%bba_source_flag

   
   CALL spoof_pack_sky
 
!------------------------------------------------------------------
   IF (IPass == 1) CALL SYNI4_NULL !Initalize on Untuned pass


!  Misc	
   SYNI4%misc%hr_month         = HrBox
   SYNI4%misc%regnum           = RegionNum_CERES

 
   SYNI4%misc%error(ipass)     = fi%ierror
   SYNI4%misc%sfc_lev          = skyp%sfclev
   SYNI4%misc%sfc_pres         = fi%pp(fi%nv+1)
   SYNI4%misc%sfc_hgt          = fi%zz(fi%nv+1)
   IF ( fi%ierror == 0 ) SYNI4%misc%toaswinsolation  = pfp(skyp%clr%sw%d(1))

!  change made 12/9/14 by Fred Rose to fix SW flux problem for cossza < 0.01   
   IF ( skyp%clr%sw%d(1) <= 0.0  .AND. fi%u0 > 0.0  ) fi%ierror = 99 ! Unknown !20141209 

! change made 9/11/14 requested by Pam M. and Raja
   IF ( fi%ierror .NE. 0 )syni4%misc%toaswinsolation  = pfp( fi%ss* fi%u0 )
   SYNI4%misc%sfcalbedo_source = sg%bba_source_flag
!   print *, "102 DEBUG: sg%bba_source_flag (SYNI4%misc%sfcalbedo_source = sg%bba_source_flag) =", sg%bba_source_flag
! HG-20250918
!IF (sg%bba_source_flag > 1000) print *, "609, sg%bba_source_flag = ", sg%bba_source_flag

!  UNTUNED DATA FILL

! change made 9/11/14 requested by Pam M. and Raja

!   IF (IPass == 1 ) CALL output_IWP_LWP(syni4%ut%var%Iwp(1:4) ,syni4%ut%var%Lwp(1:4),isp)
   IF (IPass == 1 ) CALL output_IWP_LWP(syni4%ut%var%Iwp(1:nclouds) ,syni4%ut%var%Lwp(1:nclouds),isp)

   IF (IPass == 1 .AND. fi%ierror == 0) THEN
!!      CALL output_IWP_LWP(SYNI4%ut%var%Iwp(1:4) ,SYNI4%ut%var%Lwp(1:4),isp) 
      kb = skyp%sfclev
      SYNI4%ut%clr%sw%d(2) = pfp(skyp%clr%sw%d(kb))
      SYNI4%ut%tot%sw%d(2) = pfp(skyp%tot%sw%d(kb))
      SYNI4%ut%prs%sw%d(2) = pfp(skyp%prs%sw%d(kb))
      SYNI4%ut%tna%sw%d(2) = pfp(skyp%tna%sw%d(kb))

      SYNI4%ut%clr%sw%u(1) = pfp(skyp%clr%sw%u(1) )
      SYNI4%ut%tot%sw%u(1) = pfp(skyp%tot%sw%u(1) )
      SYNI4%ut%prs%sw%u(1) = pfp(skyp%prs%sw%u(1) )
      SYNI4%ut%tna%sw%u(1) = pfp(skyp%tna%sw%u(1) )

      SYNI4%ut%clr%sw%u(2) = pfp(skyp%clr%sw%u(kb))
      SYNI4%ut%tot%sw%u(2) = pfp(skyp%tot%sw%u(kb))
      SYNI4%ut%prs%sw%u(2) = pfp(skyp%prs%sw%u(kb))
      SYNI4%ut%tna%sw%u(2) = pfp(skyp%tna%sw%u(kb))


      SYNI4%ut%clr%lw%d(2) = pfp(skyp%clr%lw%d(kb))
      SYNI4%ut%tot%lw%d(2) = pfp(skyp%tot%lw%d(kb))
      SYNI4%ut%prs%lw%d(2) = pfp(skyp%prs%lw%d(kb))
      SYNI4%ut%tna%lw%d(2) = pfp(skyp%tna%lw%d(kb))

      SYNI4%ut%clr%lw%u(1) = pfp(skyp%clr%lw%u(1) )
      SYNI4%ut%tot%lw%u(1) = pfp(skyp%tot%lw%u(1) )
      SYNI4%ut%prs%lw%u(1) = pfp(skyp%prs%lw%u(1) )
      SYNI4%ut%tna%lw%u(1) = pfp(skyp%tna%lw%u(1) )

      SYNI4%ut%clr%lw%u(2) = pfp(skyp%clr%lw%u(kb))
      SYNI4%ut%tot%lw%u(2) = pfp(skyp%tot%lw%u(kb))
      SYNI4%ut%prs%lw%u(2) = pfp(skyp%prs%lw%u(kb))
      SYNI4%ut%tna%lw%u(2) = pfp(skyp%tna%lw%u(kb))
      SYNI4%ut%wnolr%tot = pfp( ftoa(2)%trwn_f )
      SYNI4%ut%wnolr%clr = pfp( ftoa(1)%trwn_f )

   ENDIF

!  UNTUNED Variables

   IF (IPass == 1 ) THEN !...... !Regardless of Error Status
     SYNI4%ut%var%pw  =  wv(ipass)         
     SYNI4%ut%var%uth =  uth(ipass)
     if ( skyp%tot%sw%d(skyp%sfclev)  > 0 ) &     
       SYNI4%ut%var%sfcalb = skyp%tot%sw%u(skyp%sfclev)/skyp%tot%sw%d(skyp%sfclev)
  
     SYNI4%ut%var%aot      = SUM(fi%a_taus(1,1:fi%nac)) 
     SYNI4%ut%var%skint    = fi%pts
  
!     SYNI4%ut%var%cldfrac(isp(1:4)) = fi%fc(1:4)%cldfrac
!     SYNI4%ut%var%cldtemp(isp(1:4)) = fi%pt(fi%fc(1:4)%icld_top(1))
!     SYNI4%ut%var%cldtau(isp(1:4))  =  fi%fc(1:4)%tau_vis(1)+&
!                                       fi%fc(1:4)%tau_vis(2)+&
!			               fi%fc(1:4)%tau_vis(3)+&
!			               fi%fc(1:4)%tau_vis(4)

     SYNI4%ut%var%cldfrac(isp(1:nclouds)) = fi%fc(1:nclouds)%cldfrac
     SYNI4%ut%var%cldtemp(isp(1:nclouds)) = fi%pt(fi%fc(1:nclouds)%icld_top(1)) !need to check HG 20250820

!     print *, 'Top indices:', fi%fc(1:nclouds)%icld_top(1)
!   
!     print *, 'SYNI4%ut%var%cldtemp(isp(1:nclouds)) = fi%pt(fi%fc(1:nclouds)%icld_top(1))'
!     print *, SYNI4%ut%var%cldtemp(isp(1:nclouds))
!     print *, fi%pt(fi%fc(1:nclouds)%icld_top(1))
!     print *, ' '

     
     do ic_nclouds = 1, nclouds
        !SYNI4%ut%var%cldtau(isp(ic_nclouds)) = sum(fi%fc(ic_nclouds)%tau_vis(1:nclouds))  !HG-20250802
        !print *, 'sum(fi%fc(ic_nclouds)%tau_vis(1:nclouds)) ', SYNI4%ut%var%cldtau(isp(ic_nclouds)) 
        !!SYNI4%ut%var%cldtau(isp(ic_nclouds)) = sum(fi%fc(ic_nclouds)%tau_vis(1:fi%fc(ic_nclouds)%novl))
        !print *, 'sum(fi%fc(ic_nclouds)%tau_vis(1:fi%fc(ic_nclouds)%novl)) ', &
        !     sum(fi%fc(ic_nclouds)%tau_vis(1:fi%fc(ic_nclouds)%novl))
        SYNI4%ut%var%cldtau(isp(ic_nclouds)) = fi%fc(ic_nclouds)%tau_vis(1)
        !print *, 'fi%fc(ic_nclouds)%tau_vis(1) ',  SYNI4%ut%var%cldtau(isp(ic_nclouds))
        !print *, ' '
       
     end do
!     SYNI4%ut%var%cldtau(isp(1:nclouds))  =  fi%fc(1:nclouds)%tau_vis(1)+&
!                                       fi%fc(1:nclouds)%tau_vis(2)+&
!			               fi%fc(1:nclouds)%tau_vis(3)+&
!			               fi%fc(1:nclouds)%tau_vis(4)   !HG-2025.0722

     WHERE ( SYNI4%ut%var%cldfrac <= 0 ) 
!       SYNI4%ut%var%cldtemp(1:4) = -9999.
!       SYNI4%ut%var%cldtau(1:4) = -9999.
       SYNI4%ut%var%cldtemp(1:nclouds) = -9999.
       SYNI4%ut%var%cldtau(1:nclouds) = -9999.
     ENDWHERE

     SYNI4%misc%ut_aot_840 = NINT(aot_by_band(11) *10000) 
     IF ( aot_by_band(11) > 3.0 ) SYNI4%misc%ut_aot_840 = 30000.
     SYNI4%misc%o3_du = pfp(sg%o3)
     ENDIF

!     print *, ' -- SYNI4%ut%var%cldtemp(isp(1:nclouds)) = fi%pt(fi%fc(1:nclouds)%icld_top(1))'
!     print *, 'Top indices:', fi%fc(1:nclouds)%icld_top(1)
!     print *, SYNI4%ut%var%cldtemp(isp(1:nclouds))
!     print *, fi%pt(fi%fc(1:nclouds)%icld_top(1))
!     print *, ' '


!  TUNED DATA FILL 

   IF (IPass == 2 .AND. fi%ierror == 0 ) THEN

     kb = skyp%sfclev

     DO k= 2,kb ! omit redundant TOA
      SYNI4%tu%clr%sw%d(k) = pfp(skyp%clr%sw%d(k))
      SYNI4%tu%tot%sw%d(k) = pfp(skyp%tot%sw%d(k))
      SYNI4%tu%prs%sw%d(k) = pfp(skyp%prs%sw%d(k))
      SYNI4%tu%tna%sw%d(k) = pfp(skyp%tna%sw%d(k))

      SYNI4%tu%clr%lw%d(k) = pfp(skyp%clr%lw%d(k))
      SYNI4%tu%tot%lw%d(k) = pfp(skyp%tot%lw%d(k))
      SYNI4%tu%prs%lw%d(k) = pfp(skyp%prs%lw%d(k))
      SYNI4%tu%tna%lw%d(k) = pfp(skyp%tna%lw%d(k))
     ENDDO

     DO k= 1,kb
      SYNI4%tu%clr%sw%u(k) = pfp(skyp%clr%sw%u(k) )
      SYNI4%tu%tot%sw%u(k) = pfp(skyp%tot%sw%u(k) )
      SYNI4%tu%prs%sw%u(k) = pfp(skyp%prs%sw%u(k) )
      SYNI4%tu%tna%sw%u(k) = pfp(skyp%tna%sw%u(k) )

      SYNI4%tu%clr%lw%u(k) = pfp(skyp%clr%lw%u(k))
      SYNI4%tu%tot%lw%u(k) = pfp(skyp%tot%lw%u(k))
      SYNI4%tu%prs%lw%u(k) = pfp(skyp%prs%lw%u(k))
      SYNI4%tu%tna%lw%u(k) = pfp(skyp%tna%lw%u(k))
     ENDDO

     SYNI4%tu%wnolr%tot = pfp( ftoa(2)%trwn_f )
     SYNI4%tu%wnolr%clr = pfp( ftoa(1)%trwn_f )

!  Tuned Dir Dif

     SYNI4%tu%clr%sfc%direct  = pfp( fsfc(1)%swdir )
     SYNI4%tu%clr%sfc%diffuse = pfp( fsfc(1)%swdif )
     SYNI4%tu%tot%sfc%direct  = pfp( fsfc(2)%swdir )
     SYNI4%tu%tot%sfc%diffuse = pfp( fsfc(2)%swdif )
     SYNI4%tu%prs%sfc%direct  = pfp( fsfc(3)%swdir )
     SYNI4%tu%prs%sfc%diffuse = pfp( fsfc(3)%swdif )
     SYNI4%tu%tna%sfc%direct  = pfp( fsfc(4)%swdir )
     SYNI4%tu%tna%sfc%diffuse = pfp( fsfc(4)%swdif )

!  Tuned Action Spectra

     SYNI4%tu%as%toa_uva     = pfp(fouv(1)%toa_uva*10.) !! Wm-2  to DeciWm-2
     SYNI4%tu%as%toa_uvb     = pfp(fouv(1)%toa_uvb*10.) !! Wm-2  to DeciWm-2
     SYNI4%tu%as%toa_par     = pfp(fouv(1)%toa_par)

     SYNI4%tu%as%sfc_tot_uvi = pfp(fouv(2)%uvindex)


     SYNI4%tu%as%sfc_tot_uva      = pfp(fouv(2)%uva*10 )!! Wm-2  to DeciWm-2)
     SYNI4%tu%as%sfc_tot_uvb      = pfp(fouv(2)%uvb*10 )!! Wm-2  to DeciWm-2

!!    SYNI4%tu%as%sfc_tot_par      = pfp(fouv(2)%par)

     IF ( fouv(2)%par > 0 ) THEN
      SYNI4%tu%as%sfc_tot_par_dif  = pfp(fouv(2)%par/ (1+fouv(2)%par_rdirdif)      )
      SYNI4%tu%as%sfc_tot_par_dir  = pfp(fouv(2)%par -(fouv(2)%par/ (1+fouv(2)%par_rdirdif))  )
     ELSE
      SYNI4%tu%as%sfc_tot_par_dif  = pfp(0.0)
      SYNI4%tu%as%sfc_tot_par_dir  = pfp(0.0)
     ENDIF

!!    SYNI4%tu%as%sfc_clr_par      = pfp(fouv(1)%par)

     IF ( fouv(1)%par > 0 ) THEN
      SYNI4%tu%as%sfc_clr_par_dif  = pfp(fouv(1)%par/ (1+fouv(1)%par_rdirdif)      )
      SYNI4%tu%as%sfc_clr_par_dir  = pfp(fouv(1)%par -(fouv(1)%par/ (1+fouv(1)%par_rdirdif))  )
     ELSE
      SYNI4%tu%as%sfc_clr_par_dif  = pfp(0.0)
      SYNI4%tu%as%sfc_clr_par_dir  = pfp(0.0)
     ENDIF

     IF ( fouv(3)%par > 0 ) THEN
      SYNI4%tu%as%sfc_prs_par_dif  = pfp(fouv(3)%par/ (1+fouv(3)%par_rdirdif)      )
      SYNI4%tu%as%sfc_prs_par_dir  = pfp(fouv(3)%par -(fouv(3)%par/ (1+fouv(3)%par_rdirdif))  )
     ELSE
      SYNI4%tu%as%sfc_prs_par_dif  = pfp(0.0)
      SYNI4%tu%as%sfc_prs_par_dir  = pfp(0.0)
     ENDIF


!  ENTROPY 20130306
  
     SYNI4%tu%ent%toa_lwe_out = fo(2)%lw_entropy(1)
     SYNI4%tu%ent%atm_lwe_out = fo(2)%lw_entropy(2)
     SYNI4%tu%ent%sfc_lwe_out = fo(2)%lw_entropy(3)
     SYNI4%tu%ent%sfc_lwe_up  = fo(2)%lw_entropy(4)
     SYNI4%tu%ent%sfc_lwe_dn  = fo(2)%lw_entropy(5)
     SYNI4%tu%ent%atm_lwe_gen = fo(2)%lw_entropy(6)
     SYNI4%tu%ent%sfc_lwe_gen = fo(2)%lw_entropy(7)
  
     SYNI4%tu%ent%toa_swe_inc = fo(2)%sw_entropy(1)
     SYNI4%tu%ent%atm_swe_inc = fo(2)%sw_entropy(2)
     SYNI4%tu%ent%sfc_swe_inc = fo(2)%sw_entropy(3)
     SYNI4%tu%ent%atm_swe_gen = fo(2)%sw_entropy(4)
     SYNI4%tu%ent%sfc_swe_gen = fo(2)%sw_entropy(5)



!  Tuned TOA & Sfc Total Sky Spectal Fluxes------------------------------------------------------------------

!     SYNI4%tu%spec%tot%toa%sw%d(k) = pfp( SUM(fos(2)%rswfd(	  1 ,1:7)) )  
!     SYNI4%tu%spec%tot%toa%sw%u(k) = pfp( SUM(fos(2)%rswfu(	  1 ,1:7)) )
!     SYNI4%tu%spec%tot%sfc%sw%d(k) = pfp( SUM(fos(2)%rswfd(fi%nv+1 ,1:7)) )
!     SYNI4%tu%spec%tot%sfc%sw%u(k) = pfp( SUM(fos(2)%rswfu(fi%nv+1 ,1:7)) )


     SYNI4%tu%spec%tot%toa%sw%d(1) = pfp( SUM( fos(2)%rswfd(	  1 ,1:7)) )
     SYNI4%tu%spec%tot%toa%sw%u(1) = pfp( SUM( fos(2)%rswfu(	  1 ,1:7)) )
     SYNI4%tu%spec%tot%sfc%sw%d(1) = pfp( SUM( fos(2)%rswfd(fi%nv+1 ,1:7)) )
     SYNI4%tu%spec%tot%sfc%sw%u(1) = pfp( SUM( fos(2)%rswfu(fi%nv+1 ,1:7)) )
 
     SYNI4%tu%spec%tot%toa%sw%d(2) = pfp( SUM( fos(2)%rswfd(	  1 ,8:10)) )
     SYNI4%tu%spec%tot%toa%sw%u(2) = pfp( SUM( fos(2)%rswfu(	  1 ,8:10)) )
     SYNI4%tu%spec%tot%sfc%sw%d(2) = pfp( SUM( fos(2)%rswfd(fi%nv+1 ,8:10)) )
     SYNI4%tu%spec%tot%sfc%sw%u(2) = pfp( SUM( fos(2)%rswfu(fi%nv+1 ,8:10)) )
 
     SYNI4%tu%spec%tot%toa%sw%d(3) = pfp( SUM( fos(2)%rswfd(	  1 ,11:13)) )
     SYNI4%tu%spec%tot%toa%sw%u(3) = pfp( SUM( fos(2)%rswfu(	  1 ,11:13)) )
     SYNI4%tu%spec%tot%sfc%sw%d(3) = pfp( SUM( fos(2)%rswfd(fi%nv+1 ,11:13)) )
     SYNI4%tu%spec%tot%sfc%sw%u(3) = pfp( SUM( fos(2)%rswfu(fi%nv+1 ,11:13)) )

     SYNI4%tu%spec%tot%toa%sw%d(4) = pfp( SUM( fos(2)%rswfd(	  1 ,14:18)) )
     SYNI4%tu%spec%tot%toa%sw%u(4) = pfp( SUM( fos(2)%rswfu(	  1 ,14:18)) )
     SYNI4%tu%spec%tot%sfc%sw%d(4) = pfp( SUM( fos(2)%rswfd(fi%nv+1 ,14:18)) )
     SYNI4%tu%spec%tot%sfc%sw%u(4) = pfp( SUM( fos(2)%rswfu(fi%nv+1 ,14:18)) )
 
 
     SYNI4%tu%spec%tot%toa%lw%u(1) = pfp( SUM(fos(2)%rlwfu(	 1 ,10:13)) + SUM(fos(2)%rlwfu(      1 ,22:23)) ) 
     SYNI4%tu%spec%tot%sfc%lw%d(1) = pfp( SUM(fos(2)%rlwfd(fi%nv+1 ,10:13)) + SUM(fos(2)%rlwfd(fi%nv+1 ,22:23)) )
     SYNI4%tu%spec%tot%sfc%lw%u(1) = pfp( SUM(fos(2)%rlwfu(fi%nv+1 ,10:13)) + SUM(fos(2)%rlwfu(fi%nv+1 ,22:23)) )
 
     SYNI4%tu%spec%tot%toa%lw%u(2) = pfp( SUM(fos(2)%rlwfu(	 1 ,14:16)) ) 
     SYNI4%tu%spec%tot%sfc%lw%d(2) = pfp( SUM(fos(2)%rlwfd(fi%nv+1 ,14:16)) )
     SYNI4%tu%spec%tot%sfc%lw%u(2) = pfp( SUM(fos(2)%rlwfu(fi%nv+1 ,14:16)) )

     SYNI4%tu%spec%tot%toa%lw%u(3) = pfp( SUM(fos(2)%rlwfu(	 1 ,17:18)) ) 
     SYNI4%tu%spec%tot%sfc%lw%d(3) = pfp( SUM(fos(2)%rlwfd(fi%nv+1 ,17:18)) )
     SYNI4%tu%spec%tot%sfc%lw%u(3) = pfp( SUM(fos(2)%rlwfu(fi%nv+1 ,17:18)) )

     SYNI4%tu%spec%tot%toa%lw%u(4) = pfp( SUM(fos(2)%rlwfu(	 1 ,19:20)) ) 
     SYNI4%tu%spec%tot%sfc%lw%d(4) = pfp( SUM(fos(2)%rlwfd(fi%nv+1 ,19:20)) )
     SYNI4%tu%spec%tot%sfc%lw%u(4) = pfp( SUM(fos(2)%rlwfu(fi%nv+1 ,19:20)) )

     SYNI4%tu%spec%tot%toa%lw%u(5) = pfp( fos(2)%rlwfu(	 1 ,21) ) 
     SYNI4%tu%spec%tot%sfc%lw%d(5) = pfp( fos(2)%rlwfd(fi%nv+1 ,21) )
     SYNI4%tu%spec%tot%sfc%lw%u(5) = pfp( fos(2)%rlwfu(fi%nv+1 ,21) )

   ENDIF

   IF (IPass == 2 ) THEN !...... !Regardless of Error Status
 
!    CALL output_IWP_LWP(SYNI4%tu%var%Iwp(1:4) ,SYNI4%tu%var%Lwp(1:4),isp)
    CALL output_IWP_LWP(SYNI4%tu%var%Iwp(1:nclouds) ,SYNI4%tu%var%Lwp(1:nclouds),isp)  !HG-2025.0722
 
!  Tuned Variables
  
    SYNI4%tu%var%pw  =  wv(ipass)         
    SYNI4%tu%var%uth =  uth(ipass)
    IF ( skyp%tot%sw%d(skyp%sfclev)  > 0 ) &     
      SYNI4%tu%var%sfcalb  = skyp%tot%sw%u(skyp%sfclev)/skyp%tot%sw%d(skyp%sfclev)
    SYNI4%tu%var%aot      = SUM(fi%a_taus(1,1:fi%nac)) 
    SYNI4%tu%var%skint    = fi%pts
!    SYNI4%tu%var%cldfrac(isp(1:4)) = fi%fc(1:4)%cldfrac
!    SYNI4%tu%var%cldtemp(isp(1:4)) = fi%pt(fi%fc(1:4)%icld_top(1))
!    SYNI4%tu%var%cldtau(isp(1:4))  =  fi%fc(1:4)%tau_vis(1)+&
!                                      fi%fc(1:4)%tau_vis(2)+&
!  			              fi%fc(1:4)%tau_vis(3)+&
!  			              fi%fc(1:4)%tau_vis(4)

    SYNI4%tu%var%cldfrac(isp(1:nclouds)) = fi%fc(1:nclouds)%cldfrac
    SYNI4%tu%var%cldtemp(isp(1:nclouds)) = fi%pt(fi%fc(1:nclouds)%icld_top(1))
     do ic_nclouds = 1, nclouds
        SYNI4%tu%var%cldtau(isp(ic_nclouds)) = sum(fi%fc(ic_nclouds)%tau_vis(1:nclouds))  !HG-2025-0802
        !SYNI4%tu%var%cldtau(isp(ic_nclouds)) = sum(fi%fc(ic_nclouds)%tau_vis(1:fi%fc(ic_nclouds)%novl))
        !SYNI4%tu%var%cldtau(isp(ic_nclouds)) = fi%fc(ic_nclouds)%tau_vis(1)

     end do
!    SYNI4%tu%var%cldtau(isp(1:nclouds))  =  fi%fc(1:nclouds)%tau_vis(1)+&
!                                      fi%fc(1:nclouds)%tau_vis(2)+&
!  			              fi%fc(1:nclouds)%tau_vis(3)+&
!  			              fi%fc(1:nclouds)%tau_vis(4)   !! HG-2025.0722

    WHERE ( SYNI4%tu%var%cldfrac <= 0 ) 
!     SYNI4%tu%var%cldtemp(1:4) = -9999.
!     SYNI4%tu%var%cldtau(1:4) = -9999.
     SYNI4%tu%var%cldtemp(1:nclouds) = -9999.
     SYNI4%tu%var%cldtau(1:nclouds) = -9999.
    ENDWHERE
  
   ENDIF			  

   END SUBROUTINE
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   SUBROUTINE Sarb_Tracegas_Value(co2,ch4,n2o)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   Sarb_Tracegas_Value 
!
! !Description:
!    Routine ID- 
!
!    Purpose -  
!             
!
! !Input Parameters:
!   
!
! !Output Parameters:
!   
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)

!
!  Internal Routines Used:
!     
!
! !Revision History:
!  Version 1.0   June 23, 2014 - SCCR #1017 
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   REAL ,INTENT(OUT) :: co2,ch4,n2o

   REAL fyr_1959
   REAL jd_1959 
   REAL co2_1959, co2yr
   DATA jd_1959 /2436569.5/
   DATA co2_1959 , co2yr/315.98 ,1.364444/

   REAL ch4_1959, ch4yr
   DATA ch4_1959, ch4yr/1.56 ,0.004/

   REAL n2o_1959, n2oyr
   DATA n2o_1959, n2oyr/0.305 ,0.0002/

   IF ( Input_Time > 2469962 .OR. Input_Time < 2378651  ) THEN !YR < 1800 or YR >2050
      co2 = 360
      ch4=1.75
      n2o=0.31
      RETURN
   ENDIF 

   fyr_1959= (Input_Time-jd_1959) / 365.25

   co2 =   co2_1959 + (fyr_1959*co2yr)
   IF ( co2 < 280. ) co2 = 280.

   ch4 =   ch4_1959 + (fyr_1959*ch4yr)
   IF ( ch4 < 0.75 ) ch4 = 0.75

   n2o =   n2o_1959 + (fyr_1959*n2oyr)
   IF ( n2o< 0.27 ) n2o = 0.27

! Co2
! 315.98 ppmv in 1959
! 377.38 ppmv in 2004.

   END SUBROUTINE Sarb_Tracegas_Value
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   REAL FUNCTION de_asp(De)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   de_asp 
!
! !Description:
!    Routine ID- 
!
!    Purpose -  
!             
!
! !Input Parameters:
!   
!
! !Output Parameters:
!   
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)

!
!  Internal Routines Used:
!     
!
! !Revision History:
!  Version 1.0   June 23, 2014 - SCCR #1017 
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   REAL Re,De,Dge,ARfu

!  De= 0.451+2.162*Re   -7.068e-03*Re**2.0 +   1.774e-05*Re**3.0 
!  Dge =-0.706+1.526*Re    1.495e-03*Re**2.0    -2.066e-05*Re**3.0 

    Dge =  2.504655 /(  -1.819921e-04  + 1.0/(  2.453e-1  *  De   + 1.2196e-3* De**2  - 3.4745e-6*  De**3 ))
    ARfu = 2.4265+ 0.0174*Dge
    de_asp = ARfu
!    fi%fc(1)%asp(1)= ARfu  !!!!iasp
   
   END FUNCTION de_asp
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   SUBROUTINE output_IWP_LWP(IWP_P, LWP_P,isp)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   output_IWP_LWP 
!
! !Description:
!    Routine ID- 
!
!    Purpose -  
!             
!
! !Input Parameters:
!   
!
! !Output Parameters:
!   
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)

!
!  Internal Routines Used:
!     
!
! !Revision History:
!  Version 1.0   June 23, 2014 - SCCR #1017 
!                Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   USE ed4_syni_structure , ONLY :  pfp
   USE extras ,ONLY : tau_wp
  
   INTEGER, PARAMETER :: idir =1 ! Tau -->WP
   INTEGER ic,iovl
   REAL IWP,LWP
!   INTEGER*2 IWP_P(4),LWP_P(4)
!   INTEGER isp(4)
   INTEGER*2 IWP_P(nclouds),LWP_P(nclouds)
   INTEGER isp(nclouds)  ! HG-2025.0722

   CldCnd_loop:  DO ic=1,nclouds !HG
     IWP=0
     LWP=0
     DO iovl = 1,fi%fc(ic)%novl
      IF (fi%fc(ic)%cldfrac > 0 .AND. fi%fc(ic)%tau_vis(iovl) >0 ) THEN        

       IF (fi%fc(ic)%rphase(iovl)>= 1.5)&
       IWP=IWP+tau_wp(IDIR,fi%fc(ic)%tau_vis(iovl), fi%fc(ic)%de(iovl) ,2)

       IF (fi%fc(ic)%rphase(iovl)< 1.5 )&
       LWP=LWP+tau_wp(IDIR,fi%fc(ic)%tau_vis(iovl), fi%fc(ic)%re(iovl) ,1)

      ENDIF   
     ENDDO
  
     IWP_P(isp(ic)) = pfp( IWP )
     LWP_P(isp(ic)) = pfp( LWP )
  
     ENDDO CldCnd_loop


   END SUBROUTINE output_IWP_LWP

END MODULE FL_Pass_Interface
