MODULE SigmaIngest
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SigmaIngest
!
! !Description:
!    Routine ID - M5.1.3
!
!    Purpose - To provide a module of routines to open and ingest SARB
!              Sigma tables.
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
!    Sigma_Init - Drives the ingestion of SARB's sigma tables
!    st_load    - Reads SARB's sigma table ancillary input data file
!
!  Modules Used:
!    IO         - Used by Sbr. Sigma_Init
!    SARB_Error - Used by Sbr. st_load
!    SARB_Var   - Used by Sbr. Sigma_Init
!    SigTab_Var - Used by Sbr. st_load
!
! !Revision History:
!  Version 3.2   December 24, 2001 - SCCR #315
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Modified Sbr. st_load
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Modified Sbr. Sigma_Init
!  Version 2.0   November 21, 1997 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 Initial Version of code
!
! !END
!
!*****************************************************************************
!
      USE IO
      USE PCF
      USE SARB_Error_Process
      USE SARB_Var
!
      IMPLICIT NONE
!
!
  CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Sigma_Init (LID_SigTab)                                ! I
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    Sigma_Init
!
! !Description:
!   Routine ID - 5.1.3
!
!   Purpose - To drive the opening and reading of the sigma table file
!             used by the SARB tuning process
!
! !Input Parameters:
!    LID_SigTab - Logical ID for sigma file in the PCF
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    InstantSARB_IO_Params - Parameters: SigTab_Unit, 
!    IO                    - Sbr. OpenFile
!
!  Internal Routines Used:
!    st_load
!
! !Revision History
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removed PCF LID and unit number declarations statements
!                     to Mod. SARB_Var
!                  2. Renamed PCF LID parameter from SigTab_ID to LID_SigTab
!  Version 2.0   November 21, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN) :: LID_SigTab
!
      INTEGER :: IOS
!
!
      CALL OpenFile (LogicID = LID_SigTab, status = IOS,      & ! I
                     handle = SigTab_Unit,                    & ! I
                     mode = "R", access ="S",                 & ! I
                     format = "F")                              ! I
!
      CALL st_load (SigTab_Unit)                                ! I
!
      CALL CloseFile (file_handle = SigTab_Unit, status = IOS)
!
!
      END SUBROUTINE Sigma_Init
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE st_load (iunit_nl)                                       ! O
!
!******************************************************************************
!
! !F90
!
! !Name
!    st_load
!
! !Description:
!    Routine ID - 5.1.3.1
!
!    Purpose - To load tuning sigmas from NAMELIST unit
!
!
! !Input Parameters:
!    iunit_nl - Unit number for input file containing sigma table
! 
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARB_Error - Sbr. SigmaLoad_Error_Process
!                 Parameters: IVar1_Temp, IVar2_Temp, IVar3_Temp 
!    SigTab_Var - Parameters: abs_flux_tol, iav, mcase, msid, ncase, nsid,  
!                             lut, par_sig, pct_flux_tol, rwv, sigma_table_id,
!                             sigma_table_version
!
!  Error Handling:
!
! !Revision History:
!  Version 3.2   December 24, 2001 - SCCR #315
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Modified code to read sigma table version number
!  Version 2.0   November 21, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!******************************************************************************
!
!
      USE SigTab_Var
!
      INTEGER, INTENT (IN) :: iunit_nl
!
      INTEGER :: IErr
      INTEGER :: IOS
      INTEGER :: IVAR1_Temp, IVAR2_Temp, IVAR3_Temp, IVAR4_Temp
      INTEGER :: K 
!
!
      NAMELIST /ST_PARAM/ isigma_table_id, nsid, ncase, lut
      NAMELIST /ST_SIGF/  pct_flux_tol, abs_flux_tol
      NAMELIST /ST_SIGV/  par_sig
      NAMELIST /ST_CASE/  iav, rwv
      NAMELIST /ST_VERS/  sigma_table_version
!
!
      IErr = 0
      IVAR1_Temp = 0
      IVAR2_Temp = 0
      IVAR3_Temp = 0
      IVAR4_Temp = 0
!
! *** Default lookup table. All surface types point to configuration #1.
!
      lut = 1 
!
      READ (iunit_nl, nml=ST_PARAM, IOSTAT=IOS)
      IF (IOS /= 0) THEN
          IErr = 1
          IVAR1_Temp = IOS
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      END IF
!
      IF (nsid  > msid) THEN
          IErr = 2
          IVAR1_Temp = nsid
          IVAR2_Temp = msid
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      ELSE IF (ncase > mcase) THEN
          IErr = 3
          IVAR1_Temp = ncase
          IVAR2_Temp = mcase
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      END IF
!
      READ (iunit_nl, ST_SIGF, IOSTAT=IOS) 
      IF (IOS /= 0) THEN
          IErr = 4
          IVAR1_Temp = IOS
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      END IF
!
      READ (iunit_nl, ST_SIGV, IOSTAT=IOS) 
      IF (IOS /= 0) THEN
          IErr = 5
          IVAR1_Temp = IOS
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      END IF
!
      READ (iunit_nl, ST_CASE, IOSTAT=IOS) 
      IF (IOS /= 0) THEN
          IErr = 6
          IVAR1_Temp = IOS
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      END IF
!
!
      DO K = -msid, msid 
!
          IF (lut ( K) < 1   .OR.  lut ( K) > nsid) THEN
             IErr = 7
             IVAR1_Temp = lut ( K)
             IVAR2_Temp = nsid
             IVAR3_Temp = K
             CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                           IVAR3_Temp)                     ! I
          END IF
!
      END DO
!
      READ (iunit_nl, ST_VERS, IOSTAT=IOS) 
      IF (IOS /= 0) THEN
          IErr = 8
          IVAR1_Temp = IOS
          CALL SigmaLoad_Error_Process (IErr, IVAR1_Temp, IVAR2_Temp, & ! I
                                        IVAR3_Temp)                     ! I
      ELSE
          SigTabVer = sigma_table_version
      END IF
!
      END SUBROUTINE st_load
!
!
END MODULE SigmaIngest
