MODULE QC_Init
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    QC_Init
!
!
! !Description:
!    Routine ID - M5.1.5
!
!    Purpose - To drive any initialization processes required for the 
!              SARB QC Report 
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
!  Internal Routines:
!    QCSARB_Init - Assigns initial values to variables used in the generation 
!                  of the SARB QC report
!
!  Modules Used:
!    ceres_valregions - (CERESlib) Used by Sbr. QCSARB_Init
!    Profile_Params   - (SARBlib)  Used by Sbr. QCSARB_Init
!    SARB_QC_Params   - (SARBlib)  Used by Sbr. QCSARB_Init
!
! !Revision History:
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Modified Sbr. QCSARB_Init
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Modified Sbr. QCSARB_Init
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Modified Sbr. QCSARB_Init
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
    USE ceres_valregions, ONLY: NumValReg_AWG
    USE Profile_Params
    USE SARB_QC_Params
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
      SUBROUTINE QCSARB_Init (Dflt_4Real,                              & ! I
                                          fqc)                           ! O
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   QCSARB_Init
!
! !Description:
!    Routine ID - 5.1.5
!
!    Purpose - Assigns initial values to variables used in the generation 
!              of the SARB QC report
!
!
! !Input Parameters:
!    Dflt_4Real - Default value for four-byte reals
!
! !Output Parameters:
!    fqc - Structure containing QC report statistic variables
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_valregions - (CERESlib) Parameters: NumValReg_AWG
!    Profile_Params   - (SARBlib)  Parameters: NumDir, NumPass, NumSkyCond
!    SARB_QC_Params   - (SARBlib)  Parameters: avg, curt, icnt, ICnt_DirDiff,  
!                                              ICnt_FOV, ICnt_LW_Inv,  
!                                              ICnt_Night, ICnt_NoFlux,  
!                                              ICnt_NoPres, ICnt_NumPass,  
!                                              ICnt_PAR, ICnt_SfcRad_Init,  
!                                              ICnt_SunGlint, ICnt_SW_Inv,  
!                                              ICnt_TotLWrad_Inv,
!                                              ICnt_Tridag, ICnt_TuneErr, 
!                                              ICnt_WN_Inv, ICnt_WNflx_Inv,
!                                              ICnt_WNrad_Inv, is, it, ncase,
!                                              ng1x, ng1y, ng2x, ng2y, ng3x,
!                                              ng3y, ng4x, ng4y, np, rmax, 
!                                              rmin, sa, skew, std,  
!                                              ValSite_Pass, ValSite_PassCnt
!                                  Structures: fqc_type
!
! !Revision History:
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Modified calling sequence to include as input the default 
!                    value for four-byte reals
!                 2. Added initial definition of variable ICnt_FOVVal
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Enhanced in-line commentary
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Modifications to include window channel statistics
!                     removed: data statements for l2ax, l2ay, l3ax, l3ay, and
!                              the DO loops to build labels from l2ax, l2ay, 
!                              l3ax, and l3ay
!                     modified: ig values to reflect removed DO loops,
!                               data statements for l1x, l1y, l2x, l2y, l3x,
!                               l3y, and l4y
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      REAL,            INTENT (IN)  :: Dflt_4Real
      TYPE (fqc_type), INTENT (OUT) :: fqc
!
      INTEGER :: i
      INTEGER :: icase
      INTEGER :: j
!
      CHARACTER (LEN=11) :: lc ( ncase)
      CHARACTER (LEN=10) :: l1x ( ng1x) 
      CHARACTER (LEN=10) :: l1y ( ng1y) 
      CHARACTER (LEN=10) :: l2x ( ng2x)
      CHARACTER (LEN=10) :: l2y ( ng2y)
      CHARACTER (LEN=10) :: l3x ( ng3x)
      CHARACTER (LEN=10) :: l3y ( ng3y)
      CHARACTER (LEN=10) :: l4x ( ng4x)
      CHARACTER (LEN=10) :: l4y ( ng4y)
!
      data lc / 'ALL', 'CLEAR', 'Cloud', 'PTCLD', 'OVC', 'DAY', 'NIGHT', &
                'OCEAN', 'ClrOC ', 'CldOC', 'PtCldOC', 'OvOC',           &
                'LAND', 'ClrLN', 'CldLN', 'PtCldLN', 'OvLN',             &
                'SNOW', 'ClrSN', 'CldSN', 'PtCldSN', 'OvSN',             &
                'DESERT', 'ClrDS', 'CldDS', 'PtCldDS', 'OvDS',           &
                'OvTau<5', 'Ov5<Tau<20', 'OvTau>30', 'CVZA<30',          &
                '31<CVZA<60', 'CVZA>60', 'MVZA<30', '31<MVZA<60',        & 
                'MVZA>60', 'CGltAng<30', 'CGltAng>30',                   &
                'MGltAng<30', 'MGltAng>30' /
      data l1x  / 'OBS ','UT FL', 'TU FuLiou','UT OBS-FL', 'TU OBS-FL' /
      data l1y  / 'ALB TOA', 'SW TOA', 'LW TOA','WN TOA','LW RAD','WNfltRAD'/
 
      data l2x  / 'SRB Model ', 'TU FuLiou', 'TU SRB-FL' / 
      data l2y  / 'SFC SW Trn', 'SFC SW dn', 'DLF '  /
 
      data l3x  / 'TOA', 'TROP', '500', 'SFC' /
      data l3y  / 'SWDN', 'SWUP', 'LWDN', 'LWUP','SW abs','LW DIV','NET DIV'/
 
      data l4x  / 'ORIG', 'ADJUST' /
      data l4y  / 'SKINT', 'PW', 'SFCALB', 'AERTAU', 'CLDTAU',        &
                                               'CLDTMP', 'CLDFRAC','UTH'/
!
!
! *** Build labels for product-level items
!
      it = 0
      CASE : DO icase = 1, ncase
       is = 0
! 
          DO j = 1, ng1y 
              DO i = 1, ng1x 
                  is = is + 1
                  fqc % la ( is, icase) = lc ( icase) //l1x ( i)// l1y ( j)
                  fqc % ig ( is) = 1
                  fqc % ix ( is) = i
                  fqc % iy ( is) = j 
              END DO
          END DO
!
          DO j = 1, ng2y  
              DO i = 1, ng2x 
                  is = is + 1 
                  fqc % la ( is, icase) = lc ( icase) //l2x ( i) //l2y ( j) 
                  fqc % ig ( is) = 2 
                  fqc % ix ( is) = i 
                  fqc % iy ( is) = j 
              END DO 
          END DO
!
          DO j = 1, ng3y 
              DO i = 1, ng3x
                  is = is + 1
                  fqc % la ( is, icase) = lc ( icase) //l3x ( i) //l3y ( j) 
                  fqc % ig ( is) = 3
                  fqc % ix ( is) = i
                  fqc % iy ( is) = j
              END DO 
          END DO
!
          DO j = 1, ng4y 
              DO i = 1, ng4x
                  is = is + 1
                  fqc % la ( is, icase) = lc ( icase) //l4x ( i) //l4y ( j) 
                  fqc % ig ( is) = 4
                  fqc % ix ( is) = i
                  fqc % iy ( is) = j
              END DO
          END DO
!
      END DO CASE
!
!
! *** Initialize product level counters
!
      CloudInconsist_TotQC = 0
      ICnt_DirDiff ( 1:NumPass) = 0
      ICnt_FOV = 0
      ICnt_FOVVal = 0
      ICnt_LW_Inv ( 1:NumPass, 1:NumSkyCond, 1:NumDir) = 0
      ICnt_Night = 0
      ICnt_NoFlux ( 1:NumPass) = 0
      ICnt_NoPres ( 1:2) = 0
      ICnt_NumPass ( 1:2, 1:NumPass, 0:4) = 0
      ICnt_PAR ( 1:NumPass) = 0
      ICnt_SfcRad_Init = 0 
      ICnt_SunGlint ( 1:2) = 0
      ICnt_SW_Inv ( 1:NumPass, 1:NumSkyCond, 1:NumDir) = 0
      ICnt_TotLWrad_Inv ( 1:NumPass, 1:NumSkyCond) = 0
      ICnt_Tridag ( 1:2, 1:NumPass) = 0
      ICnt_TuneErr ( 1:8) = 0
      ICnt_WN_Inv ( 1:NumPass, 1:NumSkyCond, 1:NumDir) = 0
      ICnt_WNflx_Inv ( 1:NumPass, 1:NumSkyCond) = 0
      ICnt_WNrad_Inv ( 1:NumPass, 1:NumSkyCond) = 0
      InvalidInput_TotQC = 0
      InvalidModel_TotQC = 0
      InvalidTSI_SSF_TotQC = 0
      ValidFOV_TotQC = 0
      ValSite_Pass ( 1:NumValReg_AWG)    = 0
      ValSite_PassCnt ( 1:NumValReg_AWG) = 0
!
!
! *** Assign initial values for the product-level items statistical variables
!
      sa ( 1:np, 1:4) = 0.0
      icnt ( 1:np) = 0
      avg ( 1:np) = Dflt_4Real
      std ( 1:np) = Dflt_4Real
      skew ( 1:np) = Dflt_4Real
      curt ( 1:np) = Dflt_4Real
      rmax ( 1:np) = -1.0E+30
      rmin ( 1:np) =  1.0E+30
!
!
  END SUBROUTINE QCSARB_Init
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE QC_Init
