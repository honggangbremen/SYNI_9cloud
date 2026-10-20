MODULE FL_SetUp
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    FL_SetUp
!
! !Description:
!   Routine ID - M5.2.2
!
!   Purpose - To provide routines necessary to define the variables to 
!             be passed to the Fu-Liou radiative transfer model
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
!    ConstrainDelta_Calc - Calculates deltas between values of selected   
!                          variables defined during two different passes  
!                          through the radiative transfer model
!    FluxCalc_Drv        - Drives the calculation of the vertical flux profiles 
!    FluxRange_Check     - Checks flux values for NaNs and other out of range 
!                          values
!    InitTune_Copy       - Copies values from the initial pass through the 
!                          radiative transfer into variables meant to hold  
!                          the results of the constrained pass 
!    InitTune_Drv        - Defines values for variables used by the Fu-Liou 
!                          radiative transfer model for both the initial and
!                          tuned flux profile calculations
!
!  Modules Used:
!    Convert_OptDepth   - (SARBlib)  Used by Sbr. InitTune_Drv
!    FL_IO_Params       - (SARBlib)  Used by Sbr. FluxCalc_Drv, FluxRange_Check,
!                                                 InitTune_Copy
!    FL_Pass_Interface  - (SARBlib)  Used by Sbr. FluxCalc_Drv
!    MOA_IO             - (CERESlib) Used by Sbr. InitTune_Drv 
!    Profile_Params     - (SARBlib)  Used by Sbr. FluxRange_Check,
!                                                 InitTune_Copy, InitTune_Drv 
!    SARB_Error_Process - (SARBlib)  Used by Sbr. FluxCalc_Drv
!    SARB_OutVar        - (SARBlib)  Used by Sbr. ConstrainDelta_Calc,
!                                                 FluxCalc_Drv, FluxRange_Check,
!                                                 InitTune_Copy
!    SARB_QC_Params     - (SARBlib)  Used by Sbr. FluxCalc_Drv, FluxRange_Check,
!                                                 InitTune_Drv 
!    SARB_Var           - (SARBlib)  Used by Sbr. FluxCalc_Drv, InitTune_Drv
!    TuneDrive          - (SARBlib)  Used by Sbr. InitTune_Drv
!    UpTropHum          - (SARBlib)  Used by Sbr. InitTune_Drv
!    With_Cloud         - (SARBlib)  Used by Sbr. InitTune_Drv
!
! !Revision History:
!  Version 5.0   May 27, 2020 - SCCR #1657
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Added changes Entropy, pristine, and tune flags
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   Modified Sbr. InitTune_Copy
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   1. Modified Sbr. FluxCalc_Drv
!  Version 3.5   August 1, 2003 - SCCR #458  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   1. Modified Sbr. FluxRange_Check
!                   2. Modified Sbr. FluxCalc_Drv
!                   3. Modified Sbr. ConstrainDelta_Calc
!                   4. Modified Sbr. InitTune_Copy
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   1. Sbr. FL_Call - Broken down into smaller routines and
!                      moved to new Modules FL_Drv and CLdAreaWt_Results
!                   2. Modified Sbr. InitTune_Drv
!                   3. Modified Sbr. FluxRange_Check
!                   4. Modified Sbr. FluxCalc_Drv
!                   5. Modified Sbr. ConstrainDelta_Calc
!                   6. Modified Sbr. InitTune_Copy
!                   7. Removed Sbr. CldProps
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Modified Sbr. InitTune_Drv
!                  2. Modified Sbr. CldProps
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Tom Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                      Modified all routines
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Incorporation of new version of Fu-Liou model that
!                    calculates flux profiles for the window channel
!                      Modified Sbr. FL_Call
!                      Modified Sbr. FluxRange_Check
!                      Modified Sbr. FluxCalc_Drv
!                      Modified Sbr. InitTune_Drv
!                      Added Sbr. ConstrainDelta_Calc
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                    Modified Sbr. InitTune_Drv 
!                    Modified Sbr. FluxCalc_Drv
!                    Relocated Sbr. MOA_Inter to Module SARB_General
!                    Relocated Sbr. CloudPressure and WithCloud_Profile_Build
!                      into new module, With_Cloud
!                    Breakout of Sbr. CldProps from Sbr. InitTune_Drv
!                    Modified Sbr. FL_Call
!                    Modified Sbr. FluxRange_Check
!  Version 2.2   April 3, 1998 - SCCR #72
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. QC Report modifications
!                    Modified Sbr. InitTune_Drv 
!  Version 2.0   November 21, 1997 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov 
!                Initial Version of Release 2 Code
!  Version 1.0   March 21, 1996
!                Tammy Olaisen, SAIC, t.l.olaisen@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      USE Convert_OptDepth
      USE FL_Pass_Interface
      USE MOA_IO_NESTED

      USE nclouds_params  ! <<<< ADDED: use nclouds instead of MaxLev_Cld HG-2025.0722

      USE Profile_Params
      USE SARB_Error_Process
      USE SARB_OutVar
      USE SARB_QC_Params
      USE SARB_Var
      USE TuneDrive
      USE UpTropHum
      USE With_Cloud
!
  IMPLICIT NONE
!
  CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE InitTune_Drv (MOA, RecNum,                         & ! I
                                        NumTune, Flag_RTError)        ! O
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   InitTune_Drv
!
! !Description:
!   Routine ID - 5.2.2
!
!   Purpose - To define values for variables used by the Fu-Liou radiative 
!             transfer model for both the initial and tuned flux profile
!             calculations
!
!
! !Input Parameters:
!    MOA    - MOA record contents
!    RecNum - Current FOV number
!
! !Output Parameters:
!   Flag_RTError - Flag indicating whether or not an error was encountered
!                  in the intial pass through the radiative transfer model
!                   = 0; No error encountered
!                   > 0; Error encountered. Fill SARB output parameters
!                        with the CERES default value
!   NumTune      - Number of tuning iterations
!
! !Team-Unique Header:
!
!  Modules Used:
!    Convert_OptDepth  - (SARBlib)  Sbr. OptDepth_LWC
!    MOA_IO            - (CERESlib) Structures: MOA_TYPE
!    Profile_Params    - (SARBlib)  Parameters: ConstrPass1, InitPass,
!                                               MaxLev_Cld, Num_BndSA
!    FL_IO_Params      - (SARBlib)  Parameters: IW_content, LW_content, ModLev,
!                                               ModLev_Cnt, ModLev_SpHum,
!                                               ModLev_Temp 
!    SARB_QC_Params    - (SARBlib)  Parameters: ICnt_NumPass
!    SARB_Var          - (SARBlib)  Parameters: CldBot, CldTop, DayNite, 
!                                               GeoPt_Pro, NumCld_FOV,
!                                               PW_Ratio_LowTrop, 
!                                               PW_Ratio_UpTrop, SfcGeoPt, uth, 
!                                               wv_LowerTrop, wv_UpperTrop
!    TuneDrive         - (SARBlib)  Sbr. Tune_Drv
!    UpTropHum         - (SARBlib)  Sbr. HumAdj_Apply, UpperLower_Hum_Separate,
!                                        UTH_CalcDrv
!    With_Cloud        - (SARBlib)  Sbr. CloudPressure
!
!  Internal Routines Used: 
!    ConstrainDelta_Calc
!    FluxCalc_Drv
!    InitTune_Copy
!
! !Revision History:
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Modified calls to Sbr. HumAdj_Apply, 
!                     UpperLower_Hum_Separate, and UTH_CalcDrv to match  
!                     their argument lists also updated with this version 
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Removed Flag_DetCld input variable
!                  2. Modified call to CldProps
!  Version 3.1   June 22, 2001 - SCCR #267
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Added constrained/initial Aerosol ratio to calling
!                    argument list for Sbr. FluxCalc_Drv
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Thomas E. Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 1. Use of consolodated vertical flux profile arrays
!                 2. Inclusion of the spectral surface albedo data
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Added window channel flux profiles
!                 2. Modified humidity profile adjustments to separate the 
!                    upper and lower troposphere--variable PW_Ratio replaced 
!                    with PW_Ratio_LowTrop and PW_Ratio_UpTrop
!                 3. Added the incrementation of the ICnt_NumPass counters,
!                    previously in Sbr. Tune_Drv
!                 4. Removed call to Sbr. Tune_Error_Process, as it was also
!                    in Sbr. Tune_Drv.
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Added skin temperature to calling argument list for 
!                    Sbr. FluxCalc_Drv
!  Version 2.2   April 3, 1998 - SCCR #72
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. QC Report modifications
!                    Added counters for errors indicated during radiative
!                    transfer model.
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                  Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      INTEGER,         INTENT (OUT) :: Flag_RTError
      TYPE (MOA_TYPE), INTENT (IN)  :: MOA
      INTEGER,         INTENT (OUT) :: NumTune
      INTEGER,         INTENT (IN)  :: RecNum
!
!
      INTEGER :: BadTune
      INTEGER :: I 
      INTEGER :: IPass
      INTEGER :: NLev
!
!
      IPass = InitPass
!
! *** Calculate any initial cloud properties not already defined
!
!      CALL OptDepth_LWC (IPass,                                         & ! I
!                                CldBot ( IPass, 1:MaxLev_Cld))            ! O
! HG-2025.0722
      CALL OptDepth_LWC (IPass,                                         & ! I
                                CldBot ( IPass, 1:nclouds))            ! O
!
!
! *** Add initial cloud pressures to profile
!
!      CALL CloudPressure ( MOA % TropHt, IPass,                         & ! I
!                                  CldTop ( IPass, 1:MaxLev_Cld),        & ! I/O
!                                  CldBot ( IPass, 1:MaxLev_Cld),        & ! I/O
!                                                     NumCld_FOV)          ! O

      ! HG-2025.0722
      CALL CloudPressure ( MOA % TropHt, IPass,                         & ! I
                                  CldTop ( IPass, 1:nclouds),        & ! I/O
                                  CldBot ( IPass, 1:nclouds),        & ! I/O
                                                     NumCld_FOV)          ! O
!
!
! *** Calculate initial upper and lower tropospheric column humidity
!
      NLev = ModLev_Cnt ( IPass)
!      print*,'Ipass ',IPass,NLev

      CALL UpperLower_Hum_Separate (NLev, ModLev ( IPass, 1:NLev),      & ! I
                                    ModLev_SpHum ( IPass, 1:NLev),      & ! I
                                            wv_LowerTrop ( IPass),      & ! O
                                            wv_UpperTrop ( IPass))        ! O
 


 
!         Calculate the upper tropospheric relative humidity and execute constrainment algorithm.
!
          CALL UTH_CalcDrv (NLev, ModLev ( IPass, 1:NLev),              & ! I
                            ModLev_SpHum ( IPass, 1:NLev),              & ! I
                            ModLev_Temp ( IPass, 1:NLev), SfcGeoPt,     & ! I
                            GeoPt_Pro ( IPass, 1:NLev), uth ( IPass))     ! O
!
! *** Calculate initial flux profiles.
!
      Flag_RTError = 0
      CALL FluxCalc_Drv (IPass, RecNum,                                & ! I
                                        Flag_RTError)                    ! O
!
      IF (.NOT. Tuning) RETURN


      IF (Flag_RTError == 0) THEN !!     *** Initial pass through model successful.

!
          CALL Tune_Drv (InitPass, ConstrPass1,                         & ! I
                                                BadTune)                  ! O
          IPass = ConstrPass1
!
          IF (BadTune == 0) THEN
!         *** Constrainment process successful. Rebuild vertical profiles with
!             constrained cloud heights and recalculate the flux profiles.
!
!
!              CALL CloudPressure (MOA % TropHt, IPass,                   & ! I
!                                        CldTop ( IPass, 1:MaxLev_Cld),   & ! I/O
!                                        CldBot ( IPass, 1:MaxLev_Cld),   & ! I/O
!                                                           NumCld_FOV)     ! O

             ! HG-2025.0722
             CALL CloudPressure (MOA % TropHt, IPass,                   & ! I
                                        CldTop ( IPass, 1:nclouds),   & ! I/O
                                        CldBot ( IPass, 1:nclouds),   & ! I/O
                                                           NumCld_FOV)     ! O
!
              NLev = ModLev_Cnt ( IPass)
!
!
!         *** Adjust humidity profile for constrained calculations
!
              CALL HumAdj_Apply (NLev, ModLev ( IPass, 1:NLev),         & ! I
                                 PW_Ratio_LowTrop, PW_Ratio_UpTrop,     & ! I
                                      ModLev_SpHum ( IPass, 1:NLev))      ! I/O
!
!
!         *** Calculate constrained upper and lower tropospheric column humidity
!
              CALL UpperLower_Hum_Separate (NLev, ModLev ( IPass, 1:NLev), & ! I
                                            ModLev_SpHum ( IPass, 1:NLev), & ! I
                                            wv_LowerTrop ( IPass),         & ! O
                                            wv_UpperTrop ( IPass))           ! O
!
!
!         *** Calculate constrained flux profiles.
              CALL FluxCalc_Drv (IPass, RecNum,                           & ! I
                                                Flag_RTError)               ! O
!
          END IF
		 
		  IF (BadTune /= 0 .OR. Flag_RTError /= 0) THEN
!         *** Either constrainment process unsuccessful or bad constrained flux
!             value encountered.  Increment QC report counter and load initial 
!             values into archival variables.
!
              ICnt_NumPass ( DayNite, InitPass, NumCld_FOV) =                 &
                               ICnt_NumPass ( DayNite, InitPass, NumCld_FOV) + 1
!
              NumTune = 0

              CALL InitTune_Copy (InitPass, ConstrPass1)                    ! I
!
          ELSE
!         *** Calculate adjustment deltas to be written to the output product
!
              NumTune = 1
			  CALL ConstrainDelta_Calc (InitPass, ConstrPass1)             ! I
!
              ICnt_NumPass ( DayNite, ConstrPass1, NumCld_FOV) =              &
                          ICnt_NumPass ( DayNite, ConstrPass1, NumCld_FOV) + 1
!
          END IF
      END IF
!
!
      END SUBROUTINE InitTune_Drv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 
      SUBROUTINE FluxCalc_Drv (IPass, RecNum,                           & ! I
                                              Flag_RTError)               ! O
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   FluxCalc_Drv
!
! !Description:
!   Routine ID - 5.2.2.4
!
!   Purpose - Drives the calculation of the vertical flux profiles  
!
!
! !Input Parameters:
!   IPass  - Flag indicating the pass (initial or a constrained pass)  
!            through the radiative transfer model
!   RecNum - FOV number
!
! !Output Parameters:
!   Flag_RTError - Flag indicating whether or not an error was encountered
!                  in the radiative transfer model
!                   = 0; No error encountered
!                   > 0; Error encountered
!
! !Team-Unique Header:
!
!  Modules Used:
!    FL_IO_Params       - (SARBlib) Parameters: Default_INT4_FL, 
!                                               Default_REAL4_FL,   
!                                               LW_Prof, Model_Lev, 
!                                               ModLev_Cnt, SW_Prof, WN_Prof
!    FL_Pass_Interface  - (SARBlib) Sbr. FL_Pass_Drv
!    SARB_Error_Process - (SARBlib) Sbr. FLMod_Error
!    SARB_OutVar        - (SARBlib) Parameters: LWProf, SWProf, WNProf
!    SARB_QC_Params     - (SARBlib) Parameters: ICnt_Tridag
!    SARB_Var           - (SARBlib) Parameters: Tol_FracAtm
!
!  Internal Routines Used: 
!    FluxRange_Check
!
! !Revision History:
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Removed calls to CldAreaWt_Flux and CldAreaWt_Rad.  The 
!                     weighting of the FuLiou outputs is done inside the model
!                     in the new version of the Fu code.
!  Version 3.5   August 1, 2003 - SCCR #458  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removed calls to SelFlx.  For the CERES Instantaneous 
!                     SARB Subsystem, these calls are now in Module Foot_Drv.
!  Version 3.4   December 20, 2002 - SCCR #408  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Use of Module FL_IO_Params
!                  2. Addition of integer default value to Sbr. SelFlx
!                     input arguments
!  Version 3.1   June 22, 2001 - SCCR #267
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. The addition of input variable AerRatio in the 
!                    calling arguments for this routine and Sbr. FL_Call
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Use of consolodated vertical flux profile arrays
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Added window channel data
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Added skin temperature to input calling argument list
!                 2. Clean-up effort
!                    a. Moved call to Sbr. FluxRange_Check and model error
!                       counters from Sbr. InitTune_Drv
!                    b. Removed cloud heights from input calling argument list
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: IPass 
      INTEGER, INTENT (IN)  :: RecNum
      INTEGER, INTENT (OUT) :: Flag_RTError
!
      INTEGER :: ISky
!
!
! *** Calculate flux profiles.
!
      Flag_RTError = 0
      CALL FL_Pass_Drv ( IPass,                                          & ! I
                                Flag_RTError)                              ! O
!
!
      IF (Flag_RTError == 0) THEN
!     *** Verify that flux profile values are in range
!
          CALL FluxRange_Check (IPass, RecNum,                            & ! I
                                               Flag_RTError)                ! O
!
      ELSE
!     *** Error indicated by radiative tranfer algorithm. Increment 
!         appropriate counter and issue informative message.
!
          IF ( Flag_RTError == 1  .OR.  Flag_RTError == 2) THEN
               ICnt_Tridag ( Flag_RTError, IPass) =                          &
                                      ICnt_Tridag ( Flag_RTError, IPass) + 1
          END IF
!
!          CALL FLMod_Error (IPass, Flag_RTError, RecNum)                    ! I
!
      END IF
!
      IF (Flag_RTError /= 0) THEN
       Flag_RTError = Flag_RTError + (IPass*1000)
      ENDIF
!
!
      END SUBROUTINE FluxCalc_Drv 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE FluxRange_Check (IPass, RecNum,                         & ! I
                                                 BadFlux)                  ! O
!
!*****************************************************************************
!
! !F90
! 
! !Name
!   FluxRange_Check
!
! !Description
!   Routine ID - 5.2.2.4.2
!
!   Purpose - To check whether or not SARB-calculated fluxes are in the
!             correct ranges
!
!
! !Input Parameters:
!   IPass      - Flag indicating which pass through the model produced the
!                data values being checked
!   RecNum     - Current record number
!
! !Output Parameters:
!   BadFlux - Flag indicating whether or not an invalid flux value was found
!               = 0; All values valid
!               = 200; Invalid value found
!
! !Team-Unique Header:
!
!  Modules Used:
!   FL_IO_Params   - (SARBlib)  Parameters: DirDiff, PAR, Default_REAL4_FL,
!                                           LWProf, SWProf, TotLWrad, 
!                                           WNflx_SatEm, WNProf, WNrad_SatEm
!   Profile_Params - (SARBlib)  Parameters: NumDir, NumSkyCond,
!                                           Pres_Out, TotSky
!   SARB_OutVar    - (SARBlib)  Parameters: DirDiff_High, DirDiff_Low, 
!                                           LW_High, LW_Low, LWProf_Arc,
!                                           PAR_High, PAR_Low, SW_High, SW_Low,
!                                           SWProf_Arc, WNProf_Arc, WNrad_High,
!                                           WNrad_Low
!   SARB_QC_Params - (SARBlib)  Parameters: ICnt_DirDiff, ICnt_LW_Inv, 
!                                           ICnt_PAR, ICnt_SW_Inv, 
!                                           ICnt_TotLWrad_Inv, ICnt_WN_Inv,
!                                           ICnt_WNflx_Inv, ICnt_WNrad_Inv
!
! !Revision History:
!  Version 3.6   March 25, 2025 - SCCR #2548  
!                Thomas Caldwell, ADnet, Thomas.E.Caldwell@nasa.gov
!                  1. Disabled WN channel output range checks   
!                     for NOAA20 FM6 runs 
!  Version 3.5   August 1, 2003 - SCCR #458  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removed use of parameter NumLev_Arc and arrays 
!                     LWProf_Arc, SWProf_Arc, WNProf_Arc
!  Version 3.4   December 20, 2002 - SCCR #408  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Use of Module FL_IO_Params
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Removed calls to Sbr. FluxValue_Error to reduce 
!                    the large volume of warning messages written to the 
!                    log reports.
!                 2. Use of consolodated vertical flux profile arrays
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Addition of window channel data
!                 2. Parameterization of upper and lower bounds values
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (OUT) :: BadFlux 
      INTEGER, INTENT (IN)  :: IPass    
      INTEGER, INTENT (IN)  :: RecNum  
!
!
      INTEGER :: IDir
      INTEGER :: ILev 
      INTEGER :: ISky
      INTEGER :: NoNAN !! dummy variable
!
!
! *** Initialize FOV-level flag
!
      BadFlux = 0
!
!
! *** Validate ranges
!
      IF ((PAR ( IPass, TotSky) >= PAR_Low                               &
              .AND. PAR ( IPass, TotSky) <= PAR_High)                    &
          .OR. PAR ( IPass, TotSky) == Default_REAL4_FL) THEN 
!     *** PAR value out of range
!
           NoNAN = 0
       ELSE 
           BadFlux = 200
           ICnt_PAR ( IPass) = ICnt_PAR ( IPass) + 1
!
       END IF
!
!
      IF (    ( DirDiff ( IPass, TotSky) >= DirDiff_Low                  & 
                .AND. DirDiff ( IPass, TotSky) <= DirDiff_High)          &
           .OR. DirDiff ( IPass, TotSky) == Default_REAL4_FL) THEN 
!     *** Direct/diffuse ratio value out of range
!
          NoNAN = 0
      ELSE 
          BadFlux = 200
          ICnt_DirDiff ( IPass) = ICnt_DirDiff ( IPass) + 1
!
      END IF
!
!
      DO ISky = 1, NumSkyCond
!
          DO IDir = 1, NumDir
!
              DO ILev = 1, ModLev_Cnt ( IPass)
!
!
                  IF (LWProf ( IPass, ISky, IDir, ILev) >= LW_Low            &
                                .AND.                                        &
                      LWProf ( IPass, ISky, IDir, ILev)                      &
                               <= LW_High ( IDir)) THEN 
!                 *** Value within acceptable range
!
                      NoNAN = 0
!
                  ELSE 
!                 *** Value outside acceptable range. Increment counters.
!
                      IF (BadFlux == 0) THEN
                          BadFlux = 200
                          ICnt_LW_Inv ( IPass, ISky, IDir)                    &
                                  = ICnt_LW_Inv ( IPass, ISky, IDir) + 1
                      END IF
!
                  END IF
!
! 
                  IF (SWProf  ( IPass, ISky, IDir, ILev) >= SW_Low            &
                                .AND.                                         &
                      SWProf  ( IPass, ISky, IDir, ILev) <= SW_High) THEN 
!                 *** Value within acceptable range
!
                      NoNAN = 0
!
                  ELSE 
!                 *** Value outside acceptable range. Increment counters.
!
                      IF (BadFlux == 0) THEN
                          BadFlux = 200
                          ICnt_SW_Inv ( IPass, ISky, IDir)                    &
                                  = ICnt_SW_Inv ( IPass, ISky, IDir) + 1
                      END IF
!
                  END IF
!
!

               IF (Instr_Idx /= CERES_FM6) THEN


                  IF (WNProf  ( IPass, ISky, IDir, ILev) >= WNflx_Low         &
                                .AND.                                         &
                      WNProf  ( IPass, ISky, IDir, ILev) <= WNflx_High) THEN 
!                 *** Value within acceptable range
!
                      NoNAN = 0
!
                  ELSE 
!                 *** Value outside acceptable range. Increment counters.
!
                      IF (BadFlux == 0) THEN
                          BadFlux = 200
                          ICnt_WN_Inv ( IPass, ISky, IDir)                    &
                                  = ICnt_WN_Inv ( IPass, ISky, IDir) + 1
                      END IF
!
                  END IF
!

               ENDIF  ! Not FM6
!
              END DO
!
          END DO
!
!
          IF (Instr_Idx /= CERES_FM6) THEN
           
	   IF (      WNrad_SatEm ( IPass, ISky) >= 0.0                         &
              .AND. WNrad_SatEm ( IPass, ISky) <= WNrad_High) THEN 
!         *** Window radiance value out of range
!
               NoNAN = 0
           ELSE 
               IF (BadFlux == 0) THEN

                   BadFlux = 200
                   ICnt_WNrad_Inv ( IPass, ISky) =                          &
                                           ICnt_WNrad_Inv ( IPass, ISky) + 1
               END IF
!
           END IF
!      
!  
           IF (      WNflx_SatEm ( IPass, ISky) >=  WNflx_Low                 &
               .AND. WNflx_SatEm ( IPass, ISky) <= WNflx_High) THEN 
!          *** Window flux value out of range
!
               NoNAN = 0
           ELSE 
               IF (BadFlux == 0) THEN
                   BadFlux = 200
                   ICnt_WNflx_Inv ( IPass, ISky) =                           &
                                           ICnt_WNflx_Inv ( IPass, ISky) + 1
               END IF
!
           END IF
!     
          ENDIF   ! Not FM6
!
          IF (      TotLWrad ( IPass, ISky) >= 0.0                           &
               .AND. TotLWrad ( IPass, ISky) <= LWurad_High) THEN 
!          *** Total longwave radiance value out of range
!
               NoNAN = 0
          ELSE 
               IF (BadFlux == 0) THEN
                   BadFlux = 200
                   ICnt_TotLWrad_Inv ( IPass, ISky) =                         &
                                            ICnt_TotLWrad_Inv ( IPass, ISky) + 1
               END IF
!
          END IF
!     
!
      END DO
!
!
      END SUBROUTINE FluxRange_Check
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE InitTune_Copy (P1, P2)                               ! I
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   InitTune_Copy
!
! !Description:
!   Routine ID - 5.2.2.5
!
!   Purpose - To copy results from the initial pass through the radiative
!             transfer into variables meant hold the results of the 
!             constrained pass in the event that the constrained pass 
!             encounters an error, thus producing results that cannot be
!             archived
!
!
! !Input Parameters:
!    P1 - Index to the previous pass through the Fu-Liou model
!    P2 - Index to the upcoming pass through the Fu-Liou model
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    FL_IO_Params   - (SARBlib) Parameters: Default_INT4_FL, Default_REAL4_FL,
!                                           DirDiff, PAR, TotLWrad, WNflx_SatEm,
!                                           WNrad_RAW, WNrad_SatEm  
!    Profile_Params - (SARBlib) Parameters: MaxLev_Cld, NumDir,
!                                           NumSkyCond, TotSky
!    SARB_OutVar    - (SARBlib) Parameters: a_aer, a_CldFrac, a_CldTemp,
!                                           a_OptDepth, a_Precip_H2O, a_pts,
!                                           a_TotLWrad, a_UpTrop_RelHum,
!                                           a_UpTropHum, a_WNflx_SatEm, 
!                                           a_WNrad_SatEm, LWProf_Arc,
!                                           SWProf_Arc, WNProf_Arc
!
! !Revision History:
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  Added assignment of ModLev values from initial 
!                  pass to constrained pass
!  Version 3.5   August 1, 2003 - SCCR #458  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removed use of parameter NumLev_Arc and arrays 
!                     LWProf_Arc, SWProf_Arc, WNProf_Arc
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Use of new module FL_IO_Params variables
!                  2. Use of constants newly added or moved to 
!                     Module Profile_Params
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. New, consolidated Flux Profile arrays
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: P1
      INTEGER, INTENT (IN)  :: P2
!
!
      PAR ( P2, 1:NumSkyCond)     = PAR ( P1, 1:NumSkyCond)
      DirDiff ( P2, 1:NumSkyCond) = DirDiff ( P1, 1:NumSkyCond)
!
      ModLev ( P2, :) = ModLev ( P1, :)
      ModLev_Cnt ( P2) = ModLev_Cnt ( P1)
      LWProf ( P2, 1:NumSkyCond, 1:NumDir, 1:ModLev_Cnt ( P1))        &
               =  LWProf ( P1, 1:NumSkyCond, 1:NumDir, 1:ModLev_Cnt ( P1))
!
      SWProf ( P2, 1:NumSkyCond, 1:NumDir, 1:ModLev_Cnt ( P1))        &
               =  SWProf ( P1, 1:NumSkyCond, 1:NumDir, 1:ModLev_Cnt ( P1))
!
      WNProf ( P2, 1:NumSkyCond, 1:NumDir, 1:ModLev_Cnt ( P1))        &
               =  WNProf ( P1, 1:NumSkyCond, 1:NumDir, 1:ModLev_Cnt ( P1))
!
!
      TotLWrad ( P2, TotSky)    = TotLWrad ( P1, TotSky)
      WNrad_RAW ( P2, TotSky)   = WNrad_RAW ( P1, TotSky)
      WNrad_SatEm ( P2, TotSky) = WNrad_SatEm ( P1, TotSky)
      WNflx_SatEm ( P2, TotSky) = WNflx_SatEm ( P1, TotSky)
!
!
! *** Set flux deltas to default
!
      LW_Delta ( 1:NumSkyCond, 1:NumDir, 1:2) = Default_REAL4_FL
      SW_Delta ( 1:NumSkyCond, 1:NumDir, 1:2) = Default_REAL4_FL
      WN_Delta ( 1:NumSkyCond, 1:NumDir, 1:2) = Default_REAL4_FL

!

! *** Set all tuning variables to the CERES default value
!
      a_TotLWrad    = Default_REAL4_FL
      a_WNrad_SatEm = Default_REAL4_FL
      a_WNflx_SatEm = Default_REAL4_FL
!
!
! *** Meteorological and aerosol deltas
!
      a_Precip_H2O     = Default_REAL4_FL
      a_UpTropHum      = Default_REAL4_FL
      a_UpTrop_RelHum  = Default_REAL4_FL
      a_aer            = Default_REAL4_FL
      a_pts            = Default_REAL4_FL
      a_salb           = Default_REAL4_FL
!  
!
! *** Cloud condition deltas
!
!      a_OptDepth ( 1:MaxLev_Cld) = Default_REAL4_FL
!      a_CldFrac ( 1:MaxLev_Cld)  = Default_REAL4_FL
!      a_CldTemp ( 1:MaxLev_Cld)  = Default_REAL4_FL

      ! HG-2025.0722
      a_OptDepth ( 1:nclouds) = Default_REAL4_FL
      a_CldFrac ( 1:nclouds)  = Default_REAL4_FL
      a_CldTemp ( 1:nclouds)  = Default_REAL4_FL
!
!
      ISTAT_TUNE = Default_INT4_FL
!  
!
      END SUBROUTINE InitTune_Copy
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE ConstrainDelta_Calc (P1, P2)                   ! O
!
!
!*****************************************************************************
!
! !F90
! 
! !Name
!   ConstrainDelta_Calc
!
! !Description
!   Routine ID - 5.2.2.6
!
!   Purpose - Calculates deltas between values of selected variables  
!             defined during two different passes through the 
!             radiative transfer model
!
!
! !Input Parameters:
!    P1 - Index to the earlier pass through the radiative transfer model
!    P2 - Index to the later pass through the radiative transfer model
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    SfcP1 - Index to the surface height element for the earlier pass 
!            through the radiative transfer model
!    SfcP2 - Index to the surface height element for the later pass 
!            through the radiative transfer model
!
!  Modules Used:
!    FL_IO_Params   - (SARBlib) Parameters: AerOptDepth, CF, Default_REAL4_FL,
!                                           pts, TotLWrad, WNflx_SatEm,
!                                           WNrad_SatEm 
!    Profile_Params - (SARBlib) Parameters: MaxLev_Cld, NumDir, NumSkyCond, 
!                                           SfcDelta_Idx, TOADelta_Idx, TotSky
!    SARB_OutVar    - (SARBlib) Parameters: a_aer, a_CldFrac, a_CldTemp, 
!                                           a_OptDepth, a_Precip_H2O, a_pts,   
!                                           a_salb, a_TotLWrad,
!                                           a_UpTrop_RelHum, a_UpTropHum,  
!                                           a_WNrad_SatEm, a_WNflx_SatEm, 
!                                           LW_Delta, LWProf_Arc, SW_Delta,
!                                           SWProf_Arc, WN_Delta, WNProf_Arc
!    SARB_Var       - (SARBlib) Parameters: bbout, DayNite, Daytime, EffTemp, 
!                                           uth, wv, wv_UpperTrop
!
! !Revision History:
!  Version 3.5   August 1, 2003 - SCCR #458  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removed use of parameter NumLev_Arc and arrays 
!                     LWProf_Arc, SWProf_Arc, WNProf_Arc
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Use of new module FL_IO_Params variables
!                  2. Use of constants newly added or moved to 
!                     Module Profile_Params
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Use of consolodated flux profile arrays
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: P1
      INTEGER, INTENT (IN)  :: P2
!
      INTEGER :: I
      INTEGER :: IDir
      INTEGER :: ISky
      INTEGER :: SfcP1
      INTEGER :: SfcP2
!
!
      SfcP1 = ModLev_Cnt ( P1)
      SfcP2 = ModLev_Cnt ( P2)
!
      IF (DayNite == Daytime) THEN
!     *** Compute the surface albedo adjustment
!
          a_salb = bbout ( P2) - bbout ( P1)
!
      ELSE
!     *** N/A at nighttime
!
          a_salb = Default_REAL4_FL
!
      END IF
!
!
! *** Radiance deltas
!
      a_TotLWrad    =   TotLWrad ( P2, TotSky)                   &
                      - TotLWrad ( P1, TotSky)
!
      a_WNrad_SatEm =   WNrad_SatEm ( P2, TotSky)                & 
                      - WNrad_SatEm ( P1, TotSky)
!
      a_WNflx_SatEm =   WNflx_SatEm ( P2, TotSky)                & 
                      - WNflx_SatEm ( P1, TotSky)
!
!
! *** Meteorological and aerosol deltas
!
      a_Precip_H2O    = wv ( P2)           - wv ( P1)
      a_UpTropHum     = wv_UpperTrop ( P2) - wv_UpperTrop ( P1)
      a_UpTrop_RelHum = uth ( P2)          - uth ( P1)
      a_aer           = AerOptDepth ( P2)  - AerOptDepth ( P1)
      a_pts           = pts ( P2)          - pts ( P1)
!
!
! *** Cloud condition deltas
!      DO I = 1, MaxLev_Cld
      DO I = 1, nclouds  ! HG-2025.0722
!
          IF (CF ( InitPass, I) > 0.0) THEN
!         *** There is a cloud for this layer
!
              a_OptDepth ( I) = OptDepth_mn ( P2, I) - OptDepth_mn ( P1, I)
              a_CldFrac ( I)  = CF ( P2, I) - CF ( P1, I)
              a_CldTemp ( I)  = ct_adj ( I)
!  
          ELSE
!         *** There is no cloud for this layer, hence no adjustments.
!  
              a_OptDepth ( I) = Default_REAL4_FL
              a_CldFrac ( I)  = Default_REAL4_FL
              a_CldTemp ( I)  = Default_REAL4_FL
!
          END IF
!
      END DO
!
!
      DO ISky = 1, NumSkyCond
          DO IDir = 1, NumDir
!
!         *** TOA
!
              SW_Delta ( ISky, IDir, TOADelta_Idx) =                          &
                                       SWProf ( P2, ISky, IDir, TOALev_Idx)   &     
                                     - SWProf ( P1, ISky, IDir, TOALev_Idx)
!
              LW_Delta ( ISky, IDir, TOADelta_Idx) =                          &
                                       LWProf ( P2, ISky, IDir, TOALev_Idx)   &
                                     - LWProf ( P1, ISky, IDir, TOALev_Idx)
!
              WN_Delta ( ISky, IDir, TOADelta_Idx) =                          &
                                       WNProf ( P2, ISky, IDir, TOALev_Idx)   &
                                     - WNProf ( P1, ISky, IDir, TOALev_Idx)
!
!
!         *** Surface
!
              SW_Delta ( ISky, IDir, SfcDelta_Idx) =                          &
                                             SWProf ( P2, ISky, IDir, SfcP2)  &
                                           - SWProf ( P1, ISky, IDir, SfcP1)
!
              LW_Delta ( ISky, IDir, SfcDelta_Idx) =                          &
                                             LWProf ( P2, ISky, IDir, SfcP2)  &
                                           - LWProf ( P1, ISky, IDir, SfcP1)
!
              WN_Delta ( ISky, IDir, SfcDelta_Idx) =                          &
                                             WNProf ( P2, ISky, IDir, SfcP2)  &
                                           - WNProf ( P1, ISky, IDir, SfcP1)
!
           END DO
      END DO
!
!
      END SUBROUTINE ConstrainDelta_Calc
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
END MODULE FL_SetUp
