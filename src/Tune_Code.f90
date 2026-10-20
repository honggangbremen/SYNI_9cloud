MODULE Tune_Code
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Tune_Code
!
! !Description:
!   Routine ID -
!
!   Purpose - To provide a module of software, contributed by the Science
!             Team, to tune the adjustable SARB parameters.
!
! 
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARB_Error_Process - Used by Sbr. dt_tune2, chk_limit, LUDCMP, tune_xxx
!
!  Internal Subroutines:
!    dt_access   - Retrieves derivative table values
!    dt_tune_mem - Drives constrainment process
!    fsflxs
!    LUBKSB
!    LUDCMP
!    st_get_nl   - Retrieves sigma table values
!    tune_xxx
!
! !Revision History:
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Modified Sbr. fsflxs
!  Version 4.0   June 17, 2004 - SCCR #541
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Modified Sbr. st_get_nl
!  Version 3.3   April 19, 2002 - SCCR #337
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  1. Modified Sbr. dt_tune_mem
!  Version 2.4   March 15, 1999 - SCCR #132
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Initial Version of Code
!
! !end
!
!*****************************************************************************
!
  USE Control_Mod, only: Sig_Key
  USE SARB_Error_Process, only: IVar1_Temp, IVar2_Temp, IVar3_Temp,      &
                                IVar4_Temp, RVar1_Temp, RVar2_Temp,      &
                                RVar3_Temp, RVar4_Temp
  USE SARB_Var, only: FLAG_OP
  USE SARBInput_Params, only: CERCRS, CERCRSDG, CERES_FM6
  USE FL_IO_Params, only: Instr_Idx
!
!
  IMPLICIT NONE
!
!
  CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
!!!==========================================================================
!!!
!!! NEW SUBROUTINE(s) FOR Memory Loaded Derivative Tables
!!!   dt_tune_mem  "replaces"  dt_tune2
!!!   Note:  unit # not needed anymore ( pre-loaded memory access)
!!!          mcldc added as an argument for flexibility( max # cloud conditions)
!!!
!!!==========================================================================
! ABSTRACT::::
! Routine is used to come up with sugested adjustments
! to model input variables dependent on model 'state'
! computed Satellite - Model Error, and Input variable uncertanty and
! Flux Observation Uncertanty,& Estimated Partial derivative wrt/Input variable
!
! Routine uses satellite observed TOA SW & LW fluxes 
! along with model generated TOA fluxes for
! multiple cloud conditions where the cloud properties and fractions
! are obtained by a cloud imager.
!
! Selected model input variables :
! Skin Temperature ,PW ,SfcAlbedo ,Aerosol Optical Depth
! Cloud LWP,Cloud Top Temp , & Cloud Fraction are used
! as Possible canidates to be modified to bring satellite and model
! fluxes in to a "BETTER " but not exact match. (see SARB ATBD)
!
! The routine assumes a statistical range of error for each
! model input variable as well as the observed Fluxes.
! these  'sigmas' - 1std_dev expected error "uncertanty"
! are set by NAMELIST input loaded by the Subroutine 'st_load'
! the sigmas can optionally be made a function of Cloud condition and/or
! a surface dependent scene id.
! 
! Four Pre-calculated derivative sub-tables (csLw,fsLw,cssw,fssw) are used
! The Sub-tables are contained in a single F90 Unformated sequential file. 
! The tables are loaded into memory with a call to dt_load(iunit)
! Each of the Four deivative sub-tables contain different dimentionality 
! such that variations of 'state' that pertain to each Table are used.
! cslw - SkinTemp, PW
! fslw - SkinTemp, PW, Cld LWP, CLD Top Temp, "Cld Ice eff. dia."
! cssw -PW,sfcalb,aerosol Od, "solar zenith angle"
! fssw - PW,sfcalb,CLD LWP,Cld Top Temp,"Cld Ice eff. dia.","solar zenith angle"
! Updated Derivative tables can be added as long as the state and
! tuning variables are not changed or rearanged. ie. only changes
! in resolution (added or decreased) can 'easily' be made.
!
! Fred Rose 8/13/96
! 
! INPUTS:----------------------------------------------------------
! PARAMETER:: maxtune=7 ...# OF POSSIBLE TUNING VARIABLES( HARDWIRED)
! tsSwi(0:mcldc) Inital Shortwave Fluxes for each Cloud Conditions
! tsLwi(0:mcldc) Inital Longwave  Fluxes for each Cloud Conditions
!
! mcldc =    ...MAX # of cloud conditions (0=clear) mcldc>=1
! satSw = Shortwave Flux as observed by satellite(w/m**2)
! satLw = Longwave Flux as observed by satellite(w/m**2)
! 
! cfs(0:mcldc) clear sky frac,  cfs(1:mcldc)OVERCAST sky cloud fraction(s)
! skint = Skin Temperature (k)
! pwi    = Precipitable Water (cm)
! salb  = surface albedo (0-1)
! clwpi(1:mcldc) = Cloud Liquid/Ice water path(g/m**2)
! ctemp(1:mcldc) = Cloud Top Temperature(K)
! aodi= Aerosol Optical Depth
!
! cpz(1:mcldc) = Ice Particle Size (um),  for cloud tops T<260K !!STATE
!  u0= Cosine Solar Zenith angle (0-1) !!! STATE
!  itp  = aerosol type 
!  isid =  0=ocean, 1=land   
!
! SIGMAS set in subroutine st_get_nl
! USING MODULES TO SET SIGMA VALUES
!
! FOUR Derivative table File locations hardwired in subroutine dt_get
!
!OUTPUTS::-------------( ADJUSTED Values of Tuning variables)----------------
! t_cfs(0:mcldc) clear sky frac,  cfs(1:mcldc)OVERCAST sky cloud fraction(s)
! t_skint = Skin Temperature (k)
! t_pw    = Precipitable Water (cm)
! t_salb  = surface albedo (0-1)
! t_clwp(1:mcldc) = Cloud Liquid/Ice water path(g/m**2)
! t_ctemp(1:mcldc) = Cloud Top Temperature(K)
! t_aod= Aerosol Optical Depth
! istat_tune === Describes tuning adjustment status.
! WORKING ARRAYS:----------------------------------------------------------
! df(mflx) = Sw & LW satellite model TOA flux mismatch
! dtbl(0:mcldc,maxtune,mflx)  derivative array filled from derivative table
! stbl(0:mcldc,maxtune) =SIGMA TABLE
! tol(mflx) shortwave & long wave 1sigma tolerances
!
! !Revision History:
!  Version 3.3   April 19, 2002 - SCCR #337
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  1. Set sigmas for selected variables NOT tuned for nearly 
!                     clear Conditions
!  Version 3.4   March 20, 2025 - SCCR #2548
!		 Seiji Kato
!		   1. Drop window channel, window radiance, and longwave radiance
!			from tuning (Tune_xxx).
!============================================================================== 

SUBROUTINE dt_tune_mem ( mcldc, satSW, satLW, satWN, satLWr, satWNr, &
                         tsSwi, tsLwi, tsWni, tsLwri, tsWNri,        &    
                         cfs, skint, pwi, salb, clwpi, ctemp,        &
                         aodi, uthi, cpz, u0, itp, isid, ur,         &
                         t_cfs, t_skint, t_pw, t_salb, t_clwp,       &
                         t_ctemp ,t_aod,t_uth, istat_tune, BadFlag)        
!----------------------------------------------------------------------------
!! Re_Write if Changed
        INTEGER, parameter :: maxtune=8 !! # of Tuning Variables 
        INTEGER, parameter :: mflx=5 !! MAX # of fluxes      
        REAL,    parameter :: omit=-9999.  

! - Inputs
        integer,intent(in)  :: mcldc
        real   ,intent(in)  :: satSW,satLW,satWN,satLWr,satWNr
        real   ,intent(in), dimension(0:mcldc) :: cfs,tsSwi,tsLwi,tsWni,tsLwri,tsWNri
        real   ,intent(in), dimension(mcldc) :: clwpi,ctemp,cpz
        real   ,intent(in) :: skint,pwi,salb,aodi,u0
        real   ,intent(in):: uthi,ur
        integer,intent(in) :: isid,itp

!- Outputs
        real   ,intent(out),dimension(0:mcldc) :: t_cfs
        real   ,intent(out) :: t_skint,t_pw,t_salb,t_aod,t_uth
        real,intent(out),dimension(mcldc):: t_clwp,t_ctemp
        integer ,intent(out):: istat_tune
        
!- work arrays
        real :: pw,aod,uth
        real ,dimension(mcldc) :: clwp
        real, dimension(0:mcldc) :: adjcf
        real, dimension(0:mcldc,maxtune,mflx) :: dtbl
        real, dimension(mflx) :: df,tol
        real, dimension(0:mcldc,maxtune) :: stbl,adjv
        real, dimension(0:mcldc,mflx) :: flx
        real, dimension(maxtune) :: badj
!
! >>> LHC mods
!
        INTEGER, INTENT (OUT) :: BadFlag
        REAL, PARAMETER :: rminlog = 1.0E-05
        REAL            :: fsSW, fsLW ,fsWn,fsLwr,fsWNr
        INTEGER         :: icdy, icldc, iprt, nf
!
! >>>>> END LHC mods



! >>> LHC mods
!
        t_cfs ( 0:mcldc) = 0.0
        t_clwp ( 1:mcldc) = 0.0
        t_ctemp ( 1:mcldc) = 0.0
        dtbl = omit !! FRED   8/98
        BadFlag = 0
        iprt = 1
!
!print*, ' mcldc=',mcldc,' skint=',skint,' pwi=',pwi,' uthi=',uthi
!print*, ' salb=',salb,' aodi=',aodi,' u0=',u0, 'ur=',ur
!print*, ' satSW=',satSW,' satLW=',satLW,'  satWN=',satWN
!print*, ' cfs=',cfs
!print*, ' tsSwi=',tsSwi, ' tsLwi=',tsLwi, ' tsWNi=',tsWNi
!print*, ' tsLwri=',tsLwri, ' tsWNri=',tsWNri
!print*, ' clwpi=',clwpi
!print*, ' ctemp=',ctemp
!print*, ' cpz=',cpz
!print*, ' itp=',itp, '  isid=',isid
! >>>>> END LHC mods
!
      pw   = pwi
      aod  = aodi
      clwp = clwpi
      uth=uthi
      IF (pw <= 0)  pw   = rminlog
      IF (aod <= 0) aod  = rminlog
      WHERE (clwp <= 0) clwp = rminlog

! *** Compose Total Sky Model Fluxes from Componets

      CALL fsflxs (mcldc, cfs,tsSwi,tsLwi,tsWni,tsLwri,tsWNri   &
                            ,fsSW,fsLw,fsWn,fsLwr,fsWNr,flx)

! *** Satellite - Model flux mismatch

      df ( 1) = satSW - fsSW
      df ( 2) = satLW - fsLW
      df(3) =satWN-fsWN
      df(4) =satLWr-fsLWr
      df(5) =satWNr-fsWNr
!
! *** Get Sigma Table
!
      CALL st_get_nl(mcldc, satSW, satLW,satWN,satLWr,satWNr, isid,  & ! I
                                                 BadFlag,              & ! I/O
                                                          stbl, tol)     ! O
      IF (BadFlag == 0) THEN
!     *** Succsessful retrieval of sigma table values
!
       IF ( cfs(0) < 0.95) THEN
!         *** Set sigmas for selected variables NOT tuned for Cloudy Conditions

        stbl(0:mcldc,1)   = 1.0E-6  !!! NON CLEAR NO Adjust(SKINT)
        stbl(0:mcldc,2)   = 1.0E-6  !!! NON CLEAR NO Adjust(PW)
        stbl(0:mcldc,3)   = 1.0E-6  !!! NON CLEAR NO Adjust(UTH)
!!!!      stbl(0:mcldc,4)   = 1.0E-6  !!! NON CLEAR NO Adjust(SfcAlb)

        IF ( isid == 15 .or. isid == 19 .or. isid ==20 ) THEN
         !! Keep sigma set, Do nothing
        ELSE
         stbl(0:mcldc,4)   = 1.0E-6  !!! NON CLEAR NO Adjust(SfcAlb) NON_Cryosphere
        ENDIF

        stbl(0:mcldc,7)   = 1.0E-6  !!! NON CLEAR NO Adjust(Aerosol Tau)
        tol(3:5)= 1.0E+6  !NON CLEAR NO Constrain to WNFlux ,LWrad ,Win Rad

       ELSE IF ( cfs(0) .GE. 0.95  .AND.  cfs(0) < 1.0) THEN
!      *** Set sigmas for selected variables NOT tuned for almost clear
!          conditions
!
        stbl(0:mcldc,5) = 1.0E-6  !!! Almost CLEAR NO Adjust(CLDTAU)
        stbl(0:mcldc,6) = 1.0E-6  !!! Almost CLEAR NO Adjust(CLDTEMP)
        stbl(0:mcldc,8) = 1.0E-6  !!! Almost CLEAR NO Adjust(CLDFRACTION)
!
       END IF
!
       icdy = 0 
!
!     *** Get CLEAR SKY Derivatives - ALWAYS
!
       CALL dt_access (skint, pw, salb, omit, omit, aod, omit, u0, itp,     &
                       icdy, mcldc, dtbl,uth,ur)
!
!     *** Get OVERCAST SKY Derivatives - ONLY if some fraction exists

       GETDERIV : DO icdy = 1, mcldc
        IF (cfs ( icdy) > 0.0) THEN
          CALL dt_access (skint, pw, salb, clwp ( icdy),            &
                          ctemp ( icdy), aod, cpz ( icdy), u0,      &
                          itp, icdy, mcldc, dtbl,uth,ur)
        END IF
       END DO GETDERIV

       WHERE ( dtbl == omit) dtbl = 0.0  !  A cowardly move.....
!
!
!     *** Call generalized tuning routine
!
       nf = mflx  ! non-FM6

! sk: March 20, 2025 for FM-6 without window channel
       IF (Instr_Idx == CERES_FM6) THEN
        nf = 2  ! FM6 only
       ENDIF
              
       CALL tune_xxx (mflx, nf, mcldc, mcldc, cfs, df, tol, flx,       &
                      stbl ( 0:mcldc, 8:8), adjcf, maxtune, maxtune-1,   &
                      dtbl, stbl, adjv, badj, iprt, istat_tune, BadFlag)	      
!
       IF (BadFlag == 0) THEN
!      *** Adjustments have been successfully determined 
!


        t_skint =     skint        + badj(1)
        t_pw=     exp(log(pw)  + badj(2) )
        t_uth   =     exp(log(uth) + badj(3) )
        t_salb  =     salb         + badj(4)
        t_aod   =     exp(log(aod) + badj(7) ) 

!
! +++ LHC modification - 99/02/25
        IF ( CFS ( 0) == 1.0) adjcf ( 0:MCLDC) = 0.0
! +++ END LHC modification
!
!       *** Redistribute adjustments to individual cloud conditions.
!
        ADJUSTMENTS : DO icldc = 0, mcldc    
!       *** CLOUD FRACTION & LWP & CLDT
!
          t_cfs ( icldc) =  cfs ( icldc) + adjcf ( icldc)
!##print*, ' cfs(',icldc,')=',cfs(icldc),' ad=',adjcf(icldc),' t_cfs=',t_cfs(icldc)

          IF (cfs ( icldc) > 0.0) THEN
!             *** CONDITION EXISTS

           IF (icldc /= 0) THEN 
!           *** CLOUD

             t_clwp ( icldc) =    EXP (LOG (clwp ( icldc))       &
                                       + adjv ( icldc, 5) )
!
             t_ctemp ( icldc) =  ctemp ( icldc) + adjv ( icldc, 6)
!
                IF (t_cfs ( icldc) < 0. .OR. t_cfs ( icldc) > 1.) THEN
!                   *** Error. Bad tuned cloud fraction.
!
                        BadFlag = 7
                        RVar1_Temp = t_cfs ( icldc)
                        IVar2_Temp = icldc
                        EXIT
!        
                END IF
!
           END IF !! CLOUD
!
          END IF !! GOOD Cloud Condition Fraction
!
        END DO ADJUSTMENTS
!
       END IF
!
      END IF
!
!
      END SUBROUTINE dt_tune_mem
!
! 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE st_get_nl (mcldc, satSW, satLW,satWN,satLWr,satWNr, isurf,                & ! I
                                                        BadFlag,       & ! I/O
                                                        stbl, tol )      ! O
!
!*****************************************************************************
!
! !F90
!
! !Name
!    st_get_nl
!
! !Description:
!    Routine ID -
!
!    Purpose - To retrieve the appropriate sigma table values for the 
!              current footprint/region used during the tuning process
!
!
! !Input Parameters:
!    isurf - Surface sigma type
!    mcldc - Maximum number of possible cloud layers
!    satLW - LW TOA flux estimated by the CERES Inversion Subsystem 
!    satSW - SW TOA flux estimated by the CERES Inversion Subsystem 
!
!  Input/Output Parameters:
!    BadFlag - Integer flag indicating whether or not retrieval of sigma 
!              table value was successful
!
! !Output Parameters:
!    stbl - Sigma values for each cloud layer and tunable parameter
!    tol  -
!
! !Team-Unique Header:
!
!  Modules Used:
!    Control_Mod - Parameters: Sig_Key
!    SARB_Error_Process - Parameters: IVar2_Temp, RVar1_Temp
!    SARBInput_Params   - Parameters: CERCRS, CERCRSDG
!    SARB_Var - Parameters: FLAG_OP
!    SigTab_Var         - Parameters: iav, abs_flux_tol, lut, maxtune, ncase,
!                                     nsid, par_sig, pct_flux_tol, rwv
!
! !Revision History:
!  Version 4.0   June 17, 2004 - SCCR #541
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Implemented use of variable percentage flux tolerances
!                     set by ControlFile
! 
! !END
!
!*****************************************************************************
!
!
      USE SigTab_Var
!
!
      INTEGER, INTENT (INOUT) :: BadFlag
      INTEGER, INTENT (IN)    :: isurf
      INTEGER, INTENT (IN)    :: mcldc
      REAL,    INTENT (IN)    :: satLW
      REAL,    INTENT (IN)    :: satSW
      REAL,    INTENT (IN)    :: satWN,satLWr,satWNr
      REAL,    INTENT (OUT)   :: stbl ( 0:mcldc, maxtune)
      REAL,    INTENT (OUT)   :: tol ( mflx)
!
      INTEGER :: ia
      INTEGER :: icldc
      REAL    :: tpct
!
! *** SHORTWAVE TOLERANCE (%)
!
!  Set selection key to 1 for processing CRS
!
      IF (FLAG_OP == CERCRS .OR. FLAG_OP == CERCRSDG) THEN
       Sig_Key = 1
      ENDIF
      
      tpct = satSW * 0.01 * pct_flux_tol ( 1, Sig_Key)
      tol ( 1) = MAX (abs_flux_tol ( 1), tpct)

! *** LONGWAVE TOLERANCE (%)

      tpct = satLW * 0.01 * pct_flux_tol ( 2, Sig_Key) 
      tol ( 2) = MAX (abs_flux_tol ( 2), tpct)

      tpct = satWN *0.01* pct_flux_tol(3, Sig_Key) !! Window TOLERANCE (%)
      tol(3)=max(abs_flux_tol(3) ,tpct)

      tpct = satLWr *0.01* pct_flux_tol(4, Sig_Key) !! lw rad TOLERANCE (%)
      tol(4)=max(abs_flux_tol(4) ,tpct)

      tpct = satWNr *0.01* pct_flux_tol(5, Sig_Key) !! wn rad TOLERANCE (%)
      tol(5)=max(abs_flux_tol(5) ,tpct)

      CLOUD_CND : DO icldc = 0, mcldc
          stbl ( icldc, 1:maxtune) = par_sig ( 1:maxtune, lut ( isurf)) 
      END DO CLOUD_CND
!
      SPECIAL_CASE : DO ia = 1, ncase
!
          IF (iav ( 1, ia) > maxtune) THEN
!         *** Error --- Check sigma table/maxtune 
!
              BadFlag = 1
              IVar2_Temp = ia
              RVar1_Temp = iav ( 1, ia)
              EXIT
!
          ELSE IF (iav ( 2, ia) > mcldc  ) THEN
!         *** Error ---  Check sigma table/mcldc 
!
              BadFlag = 2
              IVar2_Temp = ia
              RVar1_Temp = iav ( 2, ia)
              EXIT
! 
          ELSE IF (iav ( 3, ia) > nsid   ) THEN
!         *** Error ---  Check sigma table/nsid 
! 
              BadFlag = 3
              IVar2_Temp = ia
              RVar1_Temp = iav ( 3, ia)
              EXIT
!
          END IF
!
!
          IF (iav ( 3, ia) == lut ( isurf)) THEN
              stbl ( iav ( 2, ia), iav ( 1, ia)) = rwv ( ia)
          END IF
!
      END DO SPECIAL_CASE
!
!
      END SUBROUTINE st_get_nl
!
!
!===========================================================================
! SUBROUTINE FOR MEMORY LOAD And Access
!===========================================================================
!
! -------------------------------------------------------------------------
      SUBROUTINE dt_access(skint,pw,salb,clwp,ctemp,aod, cpz,u0,itp, &
                           icdy,mcldc,dtbl,uth,ur)

! INPUT:
!     skint = SKIN TEMPERATURE (K)
!     PW    = Precipitable water (cm) 
!     SALB  = SURFACE ALBEDO (0.0-1.0)
!     ClWP  = ln( CLOUD LIQUID WATER PATH) ln(g/m**2)
!     CTEMP = Cloud Top Temperature (K)
!     AOD   = ln(Aerosol Optical Depth)  ln(tau)
!     CPZ   = Cloud Particle size (um) 10=water 30=ice
!     u0    = Cosine Solar Zenith Angle (0-1.0)
!     itp   = aerosol type(1-3)
!     icdy  =  0:CLEAR SKY   1:TOTAL OVERCAST
!     mcldc = max # of cloud conditions 
!     uth = UpperTroposphere RH (<500hPa)
!     ur  = Cosine of View Zenith Angle
! OUTPUT:
!     dtbl(0:mcldc,maxtune,mflx) = Sw&Lw derivatives for input state for all
!                                  Cloud conditions & tuning vars 
!---------------------------------------------------------------------------
! ASSIGN WHICH TUNING/State VARIIABLES ARE WHERE IN EACH TABLE
! SUBJECT TO CHANGE WITH NEW DERIVATIVE TABLE VERSIONS
! ipo(i,j)
! i=Tuning variable in table j
! j=1 CSLW j=2 FSLW j=3 CSSW j=4 FSSW    , j=5 Window Flux , j=6 Tot Radiance , j=7 Window Radiance

     USE DrivTab_Var
!
!
      REAL,    INTENT (IN)  :: aod
      REAL,    INTENT (IN)  :: clwp
      REAL,    INTENT (IN)  :: cpz
      REAL,    INTENT (IN)  :: ctemp
      INTEGER, INTENT (IN)  :: icdy
      INTEGER, INTENT (IN)  :: itp
      INTEGER, INTENT (IN)  :: mcldc
      REAL,    INTENT (IN)  :: pw
      REAL,    INTENT (IN)  :: salb
      REAL,    INTENT (IN)  :: skint
      REAL,    INTENT (IN)  :: u0
      REAL,    INTENT (IN)  :: uth,ur
!##      REAL,    INTENT (OUT) :: dtbl(0:mcldc,maxtune,mflx)
      REAL,    INTENT (INOUT) :: dtbl(0:mcldc,maxtune,mflx)
!
      REAL          :: aod_log,uth_log
      REAL          :: clwp_log
      INTEGER       :: I
      INTEGER       :: index, isv, it, ityp, itv 
      INTEGER, SAVE :: ipo ( maxtune, maxsubtab)
      INTEGER       :: istate ( maxstate)
      INTEGER       :: j  
      REAL          :: pw_log
      REAL          :: statefull (maxstate, maxsubtab)
      REAL          :: value
!  
      character*24 lab(maxtune)
      data lab/'SKIN TEMP','lnPW(sfc:500)','lnUTH(<500hpa)','SFC ALB','ln(CLOUD LWP)', &
               'Cld Top Temperature','ln(Aerosol Optical Dept)','Cld frac'/

!     IPO>>>TUNING/state
!     CSLW:SKINTPW UTH
!     FSLW:SKINTPWCLWPCTEMPCFRAC
!     CSSW:PWSALBAODu0itp
!     FSSW:PWSALBCLWP    CTEMPcpz     u0
!     CSWN:  SKINTPW UTH
!     CSLWR:SKINTPW UTHur
!     CSWNR: SKINTPW UTHur

      data ipo / 1,2,3, 0, 0,0,0,0, &
                 1,2,5, 6, 8,0,0,0, &
                 2,4,7, 9,10,0,0,0, &
                 2,4,5, 6, 8,9,0,0, &
                 1,2,3, 0, 0,0,0,0, &
                 1,2,3,11, 0,0,0,0, &
                 1,2,3,11, 0,0,0,0  /

      pw_log=omit
      IF (pw .gt.0) pw_log=log(pw)
      uth_log=omit
      IF (uth .gt.0) uth_log=log(uth)
      clwp_log=omit
      IF (clwp.gt.0) clwp_log=log(clwp)
      aod_log=omit
      IF (aod.gt.0) aod_log=log(aod)

      DO j=1,maxsubtab
       DO i=1,maxtune
        SELECT CASE  ( ipo(i,j) )

         CASE (1) ;statefull(i,j) = skint    ! Tuning
         CASE (2) ;statefull(i,j) = pw       ! Tuning ! (This was pw_log before) a mistake
         CASE (3) ;statefull(i,j) = uth_log  ! Tuning
         CASE (4) ;statefull(i,j) = salb     ! Tuning
         CASE (5) ;statefull(i,j) = clwp_log ! Tuning
         CASE (6) ;statefull(i,j) = ctemp    ! Tuning
         CASE (7) ;statefull(i,j) = aod_log  ! Tuning
         CASE (8) ;statefull(i,j) = cpz      !! state
         CASE (9) ;statefull(i,j) = u0       !! state
         CASE (10);statefull(i,j) = itp      !! state
         CASE (11);statefull(i,j) = ur       !! state

         CASE default ;cycle

        END SELECT
       ENDDO
      ENDDO
      
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
      dtbl (icdy, 1:maxtune, 1:mflx) = 0.0

      TABLES: DO  it=1,ntbl

       TUNEVAR: DO  itv=1,ntv(it)

        IF(it.eq.3 .or. it.eq.4) ityp=1 !!SW FLUX
        IF(it.le.2) ityp=2              !!LW FLUX
        IF(it.eq.5) ityp=3              !!WN FLUX
        IF(it.eq.6) ityp=4              !! LW Radiance
        IF(it.eq.7) ityp=5              !! WN Radiance

        IF( (u0.le.0.or.u0.gt.1.0).and.( it.eq.3.or.it.eq.4)) cycle  !!NIGHT
        IF( icdy.eq.0 .and.( it.eq.2.or.it.eq.4)) cycle              !!CLEAR 
        IF( icdy.ge.1 .and. &
        (it.eq.1.or.it.eq.3.or.it.eq.5.or.it.eq.6.or.it.eq.7)) cycle !!CLOUDY


!---------------------- DERIVATIVE TABLE LOOKUP
        DO isv = 1, nsv ( it)
             istate( isv) =   1.5                                             &
                            + (nsizes ( isv, it) - 1)                         &
                               * (statefull ( isv, it) - bnds ( 1, isv, it))  &
                               / rng ( isv, it)

             IF (istate ( isv) < 1) istate ( isv) = 1
             IF (istate ( isv) > nsizes ( isv, it) ) THEN
                istate ( isv) = nsizes ( isv, it)
             END IF
        END DO

        index = (itv-1) * itdim ( it) + istate ( 1)
        DO isv = 2, nsv ( it)
            index = index + ( (istate ( isv) - 1) * os ( isv, it) )
        END DO

        index = ioffs ( it) + index 

        value = transfer ( bt ( index), value)
! ------------------------------------------------

        IF ((it.eq.3 .or. it.eq.4).and. value /= omit) value = value*0.01*u0*1365.  
!          !! TOA ALB(0-100) --> TOA FLUX FSSW

        dtbl (icdy, ipo ( itv, it), ityp) = value 
        IF (value /= omit) THEN
            dtbl (icdy, ipo ( itv, it), ityp)= value / dsig ( itv, it)
        END IF

       ENDDO TUNEVAR
      ENDDO TABLES

!##  print*,' DERIVATIVEs'
!##  DO  i=1,maxtune
!##  print '(i4,5f10.3,4x,a40)', i,dtbl(icdy,i,1:mflx),lab(i)
!##  ENDDO

      END SUBROUTINE dt_access

!=============================================================================
! ============================================================================
!!! OLD SUBROUTINES UNCHANGED From Seek I/O version. 
!!! (STILL NEEDED) (CORE OF TUNING PROCESS)
!=====================================================================

!=============================================================================

      SUBROUTINE fsflxs(mcldc, cfs, tsSwi, tsLwi, tsWni, tsLwri, tsWNri,   &
                        fsSW, fsLw, fsWn, fsLwr, fsWNr, flx)
      
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Modified IF statement concerning cfs (cloud fractions) 
!                and added two where statements to establish boundaries      
!
      INTEGER, parameter :: mflx=5 ! REWRITE if changed
      INTEGER :: mcldc
      INTEGER :: I
      REAL    :: Scf
      REAL    :: fsSW, fsLW,fsWn,fsLwr,fsWNr
      REAL, dimension(0:mcldc) ::cfs,tsSwi,tsLwi,tsWni,tsLwri,tsWNri
      REAL, dimension(0:mcldc,mflx) ::flx

! Compute Full Sky TOA fluxes From OVERCAST SKY cloud conditions.
      fsSw=0
      fsLw=0
      fsWn=0
      fsLwr=0
      fsWnr=0
      scf=0

      DO  i=0,mcldc
       flx(i,1)=tsSwi(i)
       flx(i,2)=tsLwi(i)
       flx(i,3)=tsWNi(i)
       flx(i,4)=tsLwri(i)
       flx(i,5)=tsWNri(i)
!       IF(cfs(i).gt.0 .and. cfs(i).le. 1.00)then
       IF(cfs(i).gt.-0.01 .and. cfs(i).le. 1.01)then
       WHERE(cfs.lt.0)cfs=0
       WHERE(cfs.gt.1)cfs=1
        scf=scf+cfs(i)
        fsSw=fsSw+ cfs(i)*tsSwi(i)
        fsLw=fsLw+ cfs(i)*tsLwi(i)
        fsWn=fsWn+ cfs(i)*tsWni(i)
        fsLwr=fsLwr+ cfs(i)*tsLwri(i)
        fsWnr=fsWnr+ cfs(i)*tsWnri(i)
!##print'(i3,f7.2,10f10.2)',i,cfs(i),flx(i,1:mflx)
       ENDIF
      ENDDO
      
!##IF( abs(scf-1.0) .gt. 0.01) PRINT *,' Sum Cloud Frac ERROR ',scf
 
      RETURN
      
      END SUBROUTINE fsflxs
!===========================================================================
!
      SUBROUTINE tune_xxx (mflx, nf, mcldc, ncc, org, dfx,tol, flx, sigs, adj, &
                           nvar, ntv, dfdv, sigv, adjv, badj, iprt, iok, BadFlag)
!
!
!******************************************************************************
!
! !F90
!
! !Name 
!    tune_xxx
!
! !Description:
!    Routine ID -
!
!    Purpose -
!
!
! !Input Parameters:
!   MCLDC                   - Max dimension for # of Cloud Conditions
!   NCC                     - Actual # of Cloud Conditions ond used in tuning
!   ORG(0:mcldc)            - unadjusted cloud fraction
!   DF(3)                   - Inital Flux error ( OBS - Modeled)
!   TOL(3)                  - 1_sigma tolerance for observed flux
!   FLX(0:mcldc,mflx)       - Array holding Model Fluxes for each Cloud Condition
!   sigs(0:mcldc)           - 1 sigma uncertanty for Cloud Cond. Fractions (ABSOLUTE)
!   NVAR                    - Max dimension for # of Tuning Variables
!   NTV                     - actual # of tuning variables
!   DFDV(0:mcldc,nvar,mflx) - Partial Derivatives for each tuning variable for each
!                             Cloud Condition
!   sigv(0:mcldc,nvar)      - 1 sigma uncertanty for tuning variable
!   iprt                    - turn printout of statistics 1=on 0=off
!
! !Output Parameters:
!   adj(0:mcldc)            - sugested adjustments to be applied to Cloud Fractions
!   adjv(0:mcldc,nvar)      - sugested adjustments to be applied to tuning variables
!   badj(nvar)              - adjustment over entire footprint.
!
! !END
!
!******************************************************************************
!
      INTEGER, PARAMETER :: mv=20
      REAL,    PARAMETER :: ret=0.0001
      INTEGER            :: mflx, mcldc, ncc, ntv, nvar
      
      REAL, DIMENSION(mflx) :: dfx,tol
      REAL, DIMENSION(0:mcldc,mflx) :: flx
      REAL, DIMENSION(0:mcldc,nvar,mflx) :: dfdv
      REAL, DIMENSION(0:mcldc):: org,sigs,adj
      REAL, DIMENSION(0:mv) :: su,u,wt,ce
      REAL, DIMENSION(0:mv,mv) :: at
      REAL, DIMENSION(mv) :: BBB,sc,sa,te,uv,bsig,ef,r
      REAL, DIMENSION(mv,mv) :: atv,aaa
      REAL, DIMENSION(0:mcldc,nvar) :: adjv
      REAL, DIMENSION(mv+1):: a
      REAL, DIMENSION(nvar):: badj
      REAL, DIMENSION(0:mcldc,nvar):: sigv
      INTEGER, DIMENSION(mv):: INDX
      INTEGER :: iok, iprt
      INTEGER :: kk,idf
      REAL :: fvar,ovar,osd,fsd,var_red

!
      REAL    :: DDD, Energy, S1, S2, S3, SumAdj
      REAL    :: TeAll, UUU
      INTEGER :: I, IC, IJ, IPASS, IV, J, K, NF
      INTEGER, INTENT (INOUT) :: BadFlag
!
! ### Suspect that the following is unnecessary for PRODUCTION purposes
! ###
! ###IF(mcldc.gt.mv)    stop '1 redimension tune_xxx'
! ###IF(nvar .gt.mv)    stop '2 redimension tune_xxx'
! ###IF(ntv  .gt.mv)    stop '3 redimension tune_xxx'
! ###IF(ntv .gt. nvar)  stop '4 redimension tune_xxx'
! ###IF(ncc .gt. mcldc) stop '5 redimension tune_xxx'
!
      ipass=0
      iok=0
!
!
1000 continue
!
!
! --- Initalize variables
      DO I = 1, mv
       a ( I) = 0.0
       bbb ( I) = 0.0
       indx ( I) = 0.0
       te ( I) = 0.0
       sc ( I) = 0.0
       sa ( I) = 0.0
       DO J = 1, mv
        aaa ( I, J)=0.0
       END DO
      END DO

      DO I = 0, mcldc
       adj ( I) = 0.0
       ce ( I)  = 0.0
       su ( I)  = 0.0
       u ( I)   = 0.0
       wt ( I)  = 0.0
       DO J = 1, nvar
        adjv ( I, J) = 0.0
       END DO
      END DO

      DO J = 1, nvar
       DO K = 1, nf
        atv ( J, K) = 0.0
       END DO
       bsig ( J) = 0.0
       badj ( J) = 0.0
      END DO

! --- Sum Flux @ Cloud Condition
!
      DO IC = 0, ncc
       DO K = 1, nf
        energy = org ( IC) * flx ( IC, K)
        te ( K) = te ( K) + energy
        ce ( IC) = ce ( IC) + energy
       END DO !K
      END DO !IC

      teall = 0
      DO K = 1, nf
       teall = teall + te ( K) 
      END DO
!
! --- Set up for solution

      DO K  = 1, nf
       a ( K) = -2.0 * dfx ( K)
      END DO
      a ( nf+1) = 0

      DO I = 0, ncc  !!! Cloud Conditions

       IF (ipass == 0) THEN
!      *** All absolute
!
        su ( I) = ABS (sigs ( I))     
        IF (I >= 1 .AND. org ( I) <= sigs ( I)) THEN
         su ( I)  = org ( I)
        END IF
!
       ELSE IF (ipass == 1 ) THEN
!      *** Clear-sky absolute
!
        su ( 0) = ABS (sigs ( 0))  
!                    
        IF (I >= 1) THEN
!       *** Total-sky relative
!
         su ( I) = ABS (sigs ( I) * org ( I) )     
! 
        END IF
!
       ELSE IF (ipass == 2) THEN
!      *** All Relative
!
        su ( I) =  ABS (sigs ( I) * org ( I) )     
! 
       ELSE IF (ipass >= 3 .AND. ipass <= 10 ) THEN
!      *** Reduce
!
        su ( I) = ABS (sigs ( I) * org ( I)) * 4.5/REAL (ipass**2)                    !
       ELSE 
!      *** Error --- tune_xxx error ipass
!
        BadFlag = 4
        IVar2_Temp = ipass
        EXIT
!
       END IF
!
       DO K = 1, nf
        at ( I, K) = flx ( I, K) * su ( I)
       END DO
!
       wt ( I) = ce ( I) / teall  ! ENERGY FRAC for each cloud condition...
!       wt ( I) = org ( I)
       DO J = 1, ntv   !!! Bulk Sigma
        bsig ( J) = bsig ( J) + sigv ( I, J) * wt ( I) 
       END DO ! J
!
      END DO !i
!  
      IF (BadFlag == 0) THEN
!     *** No errors encountered so far
!
       DO i=0,ncc   !!! Cloud Condition
        DO j=1,ntv   !!! Property variable
         DO k = 1,nf    !! FLUX
          atv(j,k) = atv(j,k) + dfdv(i,j,k) * bsig(j) * org(i) 
         END DO !k
        END DO !j
       END DO !i
!
!    *** SOLVE  
!
       DO ij=1,nf
        aaa(ij,ij)=  tol(ij)*tol(ij)
       END DO
!
       DO ic=0,ncc !!! CLOUD CONDITIONS

        DO j=1,nf
         DO i=1,nf
             aaa(i,j)=aaa(i,j)+ at(ic,i)*at(ic,j)
         END DO !i
        END DO !j

        DO ij= 1, nf
         aaa(nf+1,ij)=aaa(nf+1,ij)+ at(ic,ij)*su(ic)
         aaa(ij,nf+1)=aaa(nf+1,ij)
        END DO !ij

        aaa(nf+1,nf+1)= aaa(nf+1,nf+1)+su(ic)*su(ic)

       END DO !! ic ; CLOUD CONDITION
!  
       DO iv=1,ntv  !! CLOUD PROPERTY TERMS
        DO j=1,nf
         DO i=1,nf
             aaa(i,j) = aaa(i,j) + atv(iv,i)*atv(iv,j)
         END DO !i
        END DO !j
       END DO  !! CLOUD PROPERTY TERMS
!
!    *** Solve Matrix
!
       DO K = 1, nf+1
        bbb ( K) = a ( K)
       END DO
!
       CALL LUDCMP (AAA, NF+1, MV, INDX, DDD, BadFlag)
         IF (BadFlag == 0) THEN
!        *** No error encountered
!
       CALL LUBKSB (AAA, NF+1, MV, INDX, bbb)
!
!
!        *** Solve for adjustments
!
       DO J = 1,ntv !!! ADJUST PROPERTY Variables
        uuu = 0.0
        DO K = 1,nf
         uuu = uuu + atv ( J, K) * bbb ( K)
        END DO
        uv ( J)= -0.5 * uuu 
        badj ( J)= uv ( J) * bsig ( J) !! Bulk Footprint Adjustment
!
!            *** Individulal cloud condition property adjustment
!
        DO I = 0, ncc
         IF (org ( I) > 0) THEN
          adjv ( I, J) =   uv ( J) * sigv ( I, J)         &
                             * wt ( I) / org ( I)
         ELSE
          adjv ( I, J) = 0.0
         END IF
        END DO !I Cloud Condition
       END DO !J Property variable
!
!
!        *** ADJUST CLOUD FRACTIONS
!
       sumadj = 0
       DO I = 0, ncc 
!
        uuu = 0
        DO K = 1, nf
         uuu = uuu + at  ( I, K) * bbb ( K)
        END DO
        uuu = uuu + su ( I) * bbb ( nf+1)
! 
        u ( I) = -0.5 * uuu
        adj ( I) = u ( I) * su ( I)
        sumadj = sumadj + adj ( I)
       END DO
!
!
!         *** Check for negative cloud fraction
!
       DO IC = 0, ncc
        IF (      org ( IC) + adj ( IC) < 0.0                 &
                 .OR. org ( IC) + adj ( IC) > 1.0+ret) THEN
!
         ipass = ipass + 1
         go to 1000
!
        END IF
       END DO
!
!
!         *** Find Flux Allowances
!
       DO K = 1, nf
        R ( K) = 0.5 * tol ( K) * bbb ( K)
        ef ( K) = R ( K) * tol ( K)
        IF (ABS (R ( K) ) > 3.0) iok = iok + 100  !! > 3 sigma FLUX
       END DO
!
!         ***  > 3 sigma CLOUD FRAC
       DO IC = 0, ncc
        IF (ABS (u ( IC)) > 3.0) iok = iok + 10   
       END DO
!
       DO J = 1, ntv
        IF (ABS (uv ( J)) > 3.0) THEN
!         *** > 3 sigma Property Variable
         iok = iok + 1 
        END IF
       END DO
!
!
!-----
! --- -----------------------------------------PRINT--------------------
!##iprt=1
       iprt=0
       IF(iprt.ge.1)then
        print*,(bbb(kk),kk=1,nf+1)
        fvar=0
        idf=0
        print*, ' Cloud Fraction'
        print 104,' Adjustment  Sigma   Adj_as_sig'

        DO i=0,ncc
         print 111,i,-9999,u(i)*su(i),su(i),u(i)
         fvar=fvar+u(i)**2
         idf=idf+1
        ENDDO

        print*, ' Variables'

        DO j=1,ntv
         print*, ' '
         print'(7x,i4,3f10.4,5x)',j,uv(j)*bsig(j),bsig(j),uv(j)
         DO i=0,ncc
          IF(adjv(i,j).ne.0)print'(3x,2i4,3f10.4,5x)',i,j,adjv(i,j),sigv(i,j)
          fvar=fvar+ org(i)* uv(j)**2
         ENDDO 
         idf=idf+1
        ENDDO

        print*, ' Fluxes '

        ovar=0.0
        DO i=1,nf
         print 111,i,-9999,ef(i),tol(i),r(i)
         fvar=fvar+r(i)**2
         ovar=ovar+(dfx(i)/tol(i))**2
         idf=idf+1
        ENDDO

        ovar=ovar/float(idf-1)
        fvar=fvar/float(idf-1)
        osd = sqrt( ovar)
        fsd = sqrt( fvar )

        var_red=0
        IF(ovar.ne.0)var_red=fvar/ovar
        print'(30x,5(3x,a5))','SWFLX','LWFLX','WNFLX','LwRad','WnRad'
        print 105, 'Org. Error Obs-Mod', (dfx(k),k=1,nf)
        print 105, 'Goal Error Obs-Mod', (-ef(k),k=1,nf)
        print 105, 'Improvment Obs-Mod', (dfx(k)+ef(k),k=1,nf)

        print 105 ,'Org Err as sigma ',(dfx(k)/tol(k),k=1,nf) 
        print 105, 'Goal Err as sigma',(-ef(k)/tol(k),k=1,nf)

        print 107, 'Variance reduced to', var_red*100., '% of Original'

        print 109,' Old Cf  Adj Cf   New CF '
        DO ic =0,ncc
         print 110,ic,org(ic),adj(ic) ,org(ic)+adj(ic)
        ENDDO

        DO ic =0,ncc
         DO k=1,nf
          sc(k)=sc(k)+ (org(ic)+adj(ic) )*flx(ic,k)  !!! FLUX CLDFRAC
         ENDDO
        ENDDO

        DO j=1,ntv
         DO i=0,ncc
          DO k=1,nf
          sa(k)=sa(k)+adjv(i,j)*dfdv(i,j,k)*org(i)  !!ADJ CLOUD PROP
          ENDDO
         ENDDO
        ENDDO

        print'(30x,5(3x,a5))','SWFLX','LWFLX','WNFLX','LwRad','WnRad'
        print 109, 'Original          (Fluxes)',( te(k),k=1,nf )

        print 109, 'Cloud frac only   (Fluxes)',(sc(k),k=1,nf)
        print 109, 'Cloud frac & Prop.(Fluxes)',(sc(k)+sa(k),k=1,nf)

        print 109, 'Cloud frac only     (Adj) ',(sc(k)-te(k) ,k=1,nf)
        print 109, '      Property only (Adj) ',(sa(k)     ,k=1,nf)
        print 109, 'TOTAL ADJUSTMENT    (Adj) ',((sc(k)-te(k)) +sa(k),k=1,nf)


103 format(7x,i4,3f10.4,5x)
111 format(3x,i4,i4,3f10.4,5x)
104 format(3x,a40)
105 format(a30,10f8.2)

107 format(/,a20, f8.2,a15)
109 format(a30,20f8.2)

110 format(i4,3f8.3)

       ENDIF ! iprt
! --- -----------------------------------------PRINT--------------------



!-----
!         *** ERROR CHECK
!
       s1 = 0
       s2 = 0
       s3 = 0
       DO IC = 0, ncc
        s1 = s1 + org ( IC)
        s2 = s2 + adj ( IC)
        s3 = s3 + org ( IC) + adj ( IC)
       END DO
!
       IF (       ABS (s1-1.00) > ret                        &
           .OR.        ABS (s2) > ret                        &
           .OR.   ABS (s3-1.00) > ret) THEN
!             *** ERROR --- CLD FRAC ADJUSTMENT
!
        BadFlag = 5
        RVar1_Temp = s1
        RVar2_Temp = s2
        RVar3_Temp = s3
        RVar4_Temp = sumadj
print*,'CFADJERR:',org,adj,s1,s2,s3
       END IF
!
      END IF
!
!
      END IF
!
!
      END SUBROUTINE tune_xxx
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!
      SUBROUTINE LUBKSB (A, N, NP, INDX, B)
!
!
      INTEGER :: N, NP
      REAL    :: A ( NP, NP), B ( N)
      INTEGER :: INDX ( N)
!
      INTEGER :: I, II, J
      INTEGER :: LL
      REAL    :: SUM
!
!
      II = 0
      DO 12 I = 1, N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM /= 0.0) THEN
          II=I
        END IF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        END IF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END SUBROUTINE LUBKSB
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!
      SUBROUTINE LUDCMP (A, N, NP, INDX, D, BadFlag)
!
      INTEGER, PARAMETER :: NMAX = 100
      REAL, PARAMETER    :: TINY = 1.0E-20
      INTEGER            :: N, NP
      INTEGER            :: INDX ( N)
      REAL               :: A ( NP, NP), D
      REAL               :: VV ( NMAX)
!
      INTEGER :: I, J, K
      INTEGER :: IMAX
      REAL    :: AAMAX
      REAL    :: DUM
      REAL    :: SUM
!
      INTEGER, INTENT (OUT) :: BadFlag
      BadFlag = 0
!
!
      D = 1.0
!
      DO I = 1, N
!
          AAMAX = 0.0
          DO J = 1, N
              IF (ABS (A ( I, J)) > AAMAX) AAMAX = ABS (A ( I, J))
          END DO 
!
          IF (AAMAX == 0.0) THEN
!         *** ERROR -  Singular matrix.
!
              BadFlag = 6
              EXIT
!
          END IF
          VV ( I) = 1.0 / AAMAX
      END DO
!
!
      IF (BadFlag == 0) THEN
!     *** No error encountered
!
          DO J = 1, N
!
              IF (J > 1) THEN
                  DO I = 1, J-1
                      SUM = A ( I, J)
                      IF (I > 1) THEN
                         DO K = 1, I-1
                            SUM = SUM -  A ( I, K) * A ( K, J)
                         END DO
                         A ( I, J) = SUM
                     END IF
                 END DO
              END IF
!
             AAMAX = 0.
             DO I = J, N
                 SUM = A ( I, J)
                 IF (J > 1) THEN
                      DO K = 1, J-1
                          SUM = SUM - A ( I, K) * A ( K, J)
                      END DO
                      A ( I, J) = SUM
                  END IF
                  DUM = VV ( I) * ABS (SUM)
                  IF (DUM >= AAMAX) THEN
                      IMAX = I
                      AAMAX = DUM
                  END IF
             END DO
!
              IF (J /= IMAX) THEN
                  DO K = 1, N
                      DUM = A ( IMAX, K)
                      A ( IMAX, K) = A ( J, K)
                      A ( J, K) = DUM
                  END DO
                  D = -D
                  VV ( IMAX) = VV  (J)
              END IF
              INDX ( J) = IMAX
              IF (J /= N) THEN
                 IF (A ( J, J) == 0.0) A ( J, J) = TINY
                   DUM = 1.0 / A ( J, J)
                  DO I = J+1, N
                      A ( I, J) = A ( I, J) * DUM
                  END DO
              END IF
          END DO
!
      END IF
! 
!
      IF (A ( N, N) == 0.0) A ( N, N) = TINY
!
!
      END SUBROUTINE LUDCMP
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE Tune_Code
