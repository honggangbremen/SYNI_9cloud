MODULE SARB_Error_Process
!
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SARB_Error
!
! !Description:
!    Routine ID -
!
!    Purpose - To provide a module of routines for interfacing with the
!              CERESlib wrappers for the EOSDIS Toolkit error message utility
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
!  Key Local Parameters:
!    IVar(nn)_Temp - Integer "clues" to help with the diagnosis of the
!                    encountered error.  Values and definitions vary with
!                    each error message
!    RVar(nn)_Temp - Floating point "clues" to help with the diagnosis of the
!                    encountered error.  Values and definitions vary with
!                    each error message
!
!  Modules Used:
!    MonMODISAer_ErrParams - (SARBlib)  Used by MonAerIngest_Error
!    msg                   - (CERESlib) Used by Sbr. DerivLoad_Error_Process, 
!                                                    Ingest_Error_Process,
!                                                    SigmaLoad_Error_Process,
!                                                    Tune_Error_Process
!    SARB_Var              - (SARBlib)  Used by Sbr. Tune_Error_Process 
!
!  Internal Routines:
!    DerivLoad_Error_Process - Interfaces with message utility for errors 
!                              encountered while ingesting the Derivative 
!                              tables
!    FLMod_Error             - Interfaces with message utility for errors 
!                              encountered within the radiative transfer 
!                              model
!    FluxValue_Error         - Interfaces with message utility for errors 
!                              encountered within the output flux profiles
!    Ingest_Error_Process    - Interfaces with message utility for errors 
!                              encountered while ingesting the SSF, TSI, 
!                              or surface albedo files
!    MonAerIngest_Error      - Interfaces with the CERESlib wrappers for the 
!                              EOSDIS Toolkit error message utility for 
!                              errors encountered while ingesting
!                              the monthly file of gridded daily MODIS 
!                              aerosol data
!    SfcAlb_Error            - Interfaces with message utility for errors 
!                              encountered while calculating the initial 
!                              surface albedo value
!    SigmaLoad_Error_Process - Interfaces with message utility for errors 
!                              encountered while ingesting the Sigma tables
!    Tune_Error_Process      - Interfaces with message utility for errors 
!                              encountered in the SARB tuning process
!
!  External Files Used:
!    PGS_DERIVLOAD_25709.f - Used by Sbr. DerivLoad_Error_Process
!    PGS_FLMODEL_25706     - Used by Sbr. FLMod_Error
!    PGS_FLXRANGE_25705.f  - Used by Sbr. FluxValue_Error
!    PGS_INGEST_25702.f    - Used by Sbr. Ingest_Error_Process, 
!                                         MonAerIngest_Error
!    PGS_SFCALBCALC_25707  - Used by Sbr. SfcAlb_Error
!    PGS_SIGMALOAD_25703.f - Used by Sbr. SigmaLoad_Error_Process
!    PGS_TUNEDRV_25704.f   - Used by Sbr. Tune_Error_Process
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                    1. Added Sbr. MonAerIngest_Error
!    Version 2.4   March 15, 1999 - SCCR #132
!                  Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                    1. Modified Sbr. FluxValue_Error
!    Version 2.0   November 21, 1997 - SCCR #30
!                  Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                  Initial Version of code
!
! !END
!
!******************************************************************************
!
!
   USE MonMODISAer_ErrParams
   USE MSG
   USE SARB_Var
!
      IMPLICIT NONE
!
      INCLUDE 'PGS_INGEST_25702.f'
      INCLUDE 'PGS_SIGMALOAD_25703.f'
      INCLUDE 'PGS_TUNEDRV_25704.f'
      INCLUDE 'PGS_FLXRANGE_25705.f'
      INCLUDE 'PGS_FLMODEL_25706.f'
      INCLUDE 'PGS_SFCALBCALC_25707.f'
      INCLUDE 'PGS_DERIVLOAD_25709.f' 
!
!
      INTEGER, SAVE :: IVar1_Temp, IVar2_Temp, IVar3_Temp, IVar4_Temp
      REAL,    SAVE :: RVar1_Temp, RVar2_Temp, RVar3_Temp, RVar4_Temp
!
!
  CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE DerivLoad_Error_Process (IErr, IVar1_Temp, IVar2_Temp, & ! I
                                                IVar3_Temp, IVar4_Temp)   ! I
!
!******************************************************************************
!
! !F90
!
! !Name:
!    DerivLoad_Error_Process
!
! !Description:
!    Routine ID -
!
!    Purpose - To interface with the CERESlib wrappers for the EOSDIS Toolkit
!              error message utility for errors encountered while ingesting
!              the derivative tables used by the SARB tuning process
!             
!
! !Input Parameters:
!    IErr       - Integer flag indicating which error was encountered in the
!                 ingestion process
!    IVar1_Temp - Diagnostic value
!    IVar2_Temp - Diagnostic value
!    IVar3_Temp - Diagnostic value
!    IVar4_Temp - Diagnostic value
!
! !Output Parameters:
!    None
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_DERIVLOAD_25709.f - Parameters: DERIVLOAD_E_NTBL_READ_ERROR, 
!                                        DERIVLOAD_E_NWORD_READ_ERROR,
!                                        DERIVLOAD_E_NTBL_GT_MAXSUB,
!                                        DERIVLOAD_E_BT_READ_ERROR
!
! !Revision History:
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!******************************************************************************
!
      INTEGER, INTENT (IN) :: IErr
      INTEGER, INTENT (IN) :: IVar1_Temp
      INTEGER, INTENT (IN) :: IVar2_Temp
      INTEGER, INTENT (IN) :: IVar3_Temp
      INTEGER, INTENT (IN) :: IVar4_Temp
!
!
      IF (IErr == 1) THEN
!
          CALL WriteReport (DERIVLOAD_E_NTBL_READ_ERROR,           & ! I
                            FuncName = "dt_load ()",               & ! I
                            IVar1 = IVar1_Temp)                      ! I
!
      ELSE IF (IErr == 2) THEN
!
          CALL WriteReport (DERIVLOAD_E_NWORD_READ_ERROR,          & ! I
                            FuncName = "dt_load ()",               & ! I
                            IVar1 = IVar1_Temp)                      ! I
!
      ELSE IF (IErr == 3) THEN
!
          CALL WriteReport (DERIVLOAD_E_NTBL_GT_MAXSUB,           & ! I
                            FuncName = "dt_load ()",              & ! I
                            IVar1 = IVar1_Temp,                   & ! I
                            IVar2 = IVar2_Temp                )     ! I
!
      ELSE IF (IErr == 4) THEN
!
          CALL WriteReport (DERIVLOAD_E_BT_READ_ERROR,            & ! I
                            FuncName = "dt_load ()",              & ! I
                            IVar1 = IVar1_Temp,                   & ! I
                            IVar2 = IVar2_Temp,                   & ! I
                            IVar3 = IVar3_Temp,                   & ! I
                            IVar4 = IVar4_Temp                )     ! I
!
      END IF
!
!
      END SUBROUTINE DerivLoad_Error_Process
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Ingest_Error_Process (IErr, IVar1_Temp, RVar1_Temp,      & ! I
                                                         RVar2_Temp)        ! I
!
!
!******************************************************************************
!
! !F90
!
! !Name:
!    Ingest_Error_Process
!
! !Description:
!    Routine ID -
!
!    Purpose - To interface with the CERESlib wrappers for the EOSDIS Toolkit
!              error message utility for errors encountered while ingesting
!              the SSF or TSI products
!             
!
! !Input Parameters:
!    IErr       - Integer flag indicating which error was encountered in the
!                 ingestion process
!    IVar1_Temp - Diagnostic value
!    RVar1_Temp - Diagnostic value
!    RVar2_Temp - Diagnostic value
!
! !Output Parameters:
!    None
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_INGEST_25702.f - Parameters: INGEST_E_SSF_READ_ERROR, 
!                                     INGEST_E_TSI_READ_ERROR,
!                                     INGEST_E_10MIN_SFCALB_READ_ERROR,
!                                     INGEST_E_REG_SFCALB_READ_ERROR
!
! !Revision History:
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN) :: IErr
      INTEGER, INTENT (IN) :: IVar1_Temp
      REAL,    INTENT (IN) :: RVar1_Temp
      REAL,    INTENT (IN) :: RVar2_Temp
!
!
      IF (IErr == 1) THEN
!
          CALL WriteReport (INGEST_E_SSF_READ_ERROR,             & ! I
                            FuncName = "Ingest_Input ()",        & ! I
                            IVar1 = IVar1_Temp)                    ! I Rec Num
!
      ELSE IF (IErr == 2) THEN
!
          CALL WriteReport (INGEST_E_TSI_READ_ERROR,             & ! I
                            FuncName = "Ingest_Input ()",        & ! I
                            IVar1 = IVar1_Temp)                    ! I Reg Num
!
      ELSE IF (IErr == 3) THEN
!
          CALL WriteReport (INGEST_E_10MN_SFCALB_READ_ERROR,     & ! I
                            FuncName = "Ingest_Input ()",        & ! I
                            IVar1 = IVar1_Temp,                  & ! I Rec Num
                            RVar1 = RVar1_Temp,                  & ! I Lat
                            RVar2 = RVar2_Temp)                    ! I Long
!
      ELSE IF (IErr == 4) THEN
!
          CALL WriteReport (INGEST_E_REG_SFCALB_READ_ERROR,      & ! I
                            FuncName = "Ingest_Input ()",        & ! I
                            IVar1 = IVar1_Temp)                    ! I Reg Num
!
      END IF
!
      END SUBROUTINE Ingest_Error_Process
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE SigmaLoad_Error_Process (IErr, IVar1_Temp, IVar2_Temp,  & ! I
                                                            IVar3_Temp)    ! I
!
!******************************************************************************
!
! !F90
!
! !Name:
!    SigmaLoad_Error_Process
!
! !Description:
!    Routine ID -
!
!    Purpose - To interface with the CERESlib wrappers for the EOSDIS Toolkit
!              error message utility for errors encountered while ingesting
!              the derivative tables used by the SARB tuning process
!             
!
! !Input Parameters:
!    IErr - Integer flag indicating which error was encountered in the
!           ingestion process
!    IVar1_Temp - Diagnostic value
!    IVar2_Temp - Diagnostic value
!    IVar3_Temp - Diagnostic value
!
! !Output Parameters:
!    None
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_DERIVLOAD_25703.f - Parameters: SIGMALOAD_E_ST_PARAM_READ_ERROR,
!                                        SIGMALOAD_E_NSID_GT_MSID,
!                                        SIGMALOAD_E_NCASE_GT_MCASE,
!                                        SIGMALOAD_E_ST_SIGF_READ_ERROR,
!                                        SIGMALOAD_E_ST_SIGV_READ_ERROR,
!                                        SIGMALOAD_E_ST_CASE_READ_ERROR,
!                                        SIGMALOAD_E_LUT_PNTR_OUT_OF_RANGE 
!
! !Revision History:
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN) :: IErr
      INTEGER, INTENT (IN) :: IVar1_Temp
      INTEGER, INTENT (IN) :: IVar2_Temp
      INTEGER, INTENT (IN) :: IVar3_Temp
!
!
      IF (IErr == 1) THEN
!
          CALL WriteReport (SIGMALOAD_E_ST_PARAM_READ_ERROR,           & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp)                          ! I
!
      ELSE IF (IErr == 2) THEN
!
          CALL WriteReport (SIGMALOAD_E_NSID_GT_MSID,                  & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp,                        & ! I
                            IVar2 = IVar2_Temp)                          ! I
!
      ELSE IF (IErr == 3) THEN
!
          CALL WriteReport (SIGMALOAD_E_NCASE_GT_MCASE,                & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp,                        & ! I
                            IVar2 = IVar2_Temp)                          ! I
!
      ELSE IF (IErr == 4) THEN
!
          CALL WriteReport (SIGMALOAD_E_ST_SIGF_READ_ERROR,            & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp)                          ! I
!
      ELSE IF (IErr == 5) THEN
!
          CALL WriteReport (SIGMALOAD_E_ST_SIGV_READ_ERROR,            & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp)                          ! I
!
      ELSE IF (IErr == 6) THEN
!
          CALL WriteReport (SIGMALOAD_E_ST_CASE_READ_ERROR,            & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp)                          ! I
!
      ELSE IF (IErr == 7) THEN
!
          CALL WriteReport (SIGMALOAD_E_LUTPTR_OUT_OF_RANGE,           & ! I
                            FuncName = "st_load ()",                   & ! I
                            IVar1 = IVar1_Temp,                        & ! I
                            IVar2 = IVar2_Temp,                        & ! I
                            IVar3 = IVar3_Temp)                          ! I
!
      END IF
!
!
      END SUBROUTINE SigmaLoad_Error_Process
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
      SUBROUTINE Tune_Error_Process (BadFlag, NREC)                       ! I
!
!******************************************************************************
!
! !F90
!
! !Name:
!    Tune_Error_Process
!
! !Description:
!    Routine ID -
!
!    Purpose - To interface with the CERESlib wrappers for the EOSDIS Toolkit
!              error message utility for errors encountered during the SARB
!              tuning process
!
!
! !Input Parameters:
!    BadFlag  - Integer flag indicating which error was encountered in the
!               tuning process
!    NRec     - Record number
!
! !Output Parameters:
!   None
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg      - (CERESlib) Sbr: WriteReport
!    SARB_Var - (SARBlib)  Parameters: NRec
!
!  External Files Used:
!    PGS_TUNEDRV_25704.f - Parameters: TUNEDRV_W_ADJCLDFRAC_OUT_OF_RANGE,
!                                      TUNEDRV_W_IPN_OUT_OF_RANGE,
!                                      TUNEDRV_W_TUNE3C_ERROR_PASS,
!                                      TUNEDRV_W_ERROR_CLDFRAC_ADJUSTMENT,
!                                      TUNEDRV_W_SINGULAR_MATRIX
!
! !Revision History:
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN) :: BadFlag, NRec
!
!
      IF (BadFlag == 1) THEN
!
          CALL WriteReport (TUNEDRV_W_SIGMA_TABLE_MAXTUNE,           & ! I
                            FuncName = "st_get_nl ()",               & ! I
                            IVar1 = NRec)                              ! I
!
      ELSE IF (BadFlag == 2) THEN
!
          CALL WriteReport (TUNEDRV_W_SIGMA_TABLE_MCLDC,             & ! I
                            FuncName = "st_get_nl ()",               & ! I
                            IVar1 = NRec)                              ! I
!
      ELSE IF (BadFlag == 3) THEN
!
          CALL WriteReport (TUNEDRV_W_SIGMA_TABLE_NSID,              & ! I
                            FuncName = "st_get_nl ()",               & ! I
                            IVar1 = NRec                )              ! I
!
      ELSE IF (BadFlag == 4) THEN
!
          CALL WriteReport (TUNEDRV_W_TUNEXXX_ERROR_PASS,            & ! I
                            FuncName = "tune_xxx ()",                & ! I
                            IVar1 = NRec)                              ! I
!
      ELSE IF (BadFlag == 5) THEN
!
          CALL WriteReport (TUNEDRV_W_ERR_CLDFRAC_ADJUSTMNT,         & ! I
                            FuncName = "tune_xxx ()",                & ! I
                            IVar1 = NRec)                              ! I
!
      ELSE IF (BadFlag == 6) THEN
!
          CALL WriteReport (TUNEDRV_W_SINGULAR_MATRIX,               & ! I
                            FuncName = "LUDCOMP ()",                 & ! I
                            IVar1 = NRec)                              ! I
!
      ELSE IF (BadFlag == 7) THEN
!
          CALL WriteReport (TUNEDRV_W_AJCLDFRC_OUT_OF_RANGE,         & ! I
                            FuncName = "dt_tune_mem ()",             & ! I
                            IVar1 = NRec)                              ! I
!
      ELSE IF (BadFlag == 8) THEN
!
          CALL WriteReport (TUNEDRV_W_AOD_OUT_OF_RANGE,              & ! I
                            FuncName = "Tune_Drv",                   & ! I
                            IVar1 = NRec)                              ! I 
      END IF
!
!
      END SUBROUTINE Tune_Error_Process
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE FluxValue_Error (BadFlux, RecNum, BadValue, ILev, PLev)  ! I
!
!*****************************************************************************
!
! !F90
! 
! !Name
!   FluxValue_Error
!
! !Description
!   Routine ID -
!
!   Purpose - To generate a warning message via the Toolkit whenever a  
!             SARB-calculated flux is outside of the correct range
!
!
! !Input Parameters:
!   BadFlux  - Flag indicating type of invalid flux 
!   BadValue - Value of flux that is cause of error
!   ILev     - Level number within vertical profile of bad value
!   PLev     - Pressure corresponding to vertical level ILev
!   RecNum   - Current record number
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!    PGS_FLUXRANGE_25705.f - Parameters: FLXRANGE_W_CONSTR_DIRDIFF_INVALID,
!                                        FLXRANGE_W_CONSTR_DNLWCLR_INVLD,
!                                        FLXRANGE_W_CONSTR_DNLWTOT_INVLD,
!                                        FLXRANGE_W_CONSTR_DNSWCLR_INVLD,
!                                        FLXRANGE_W_CONSTR_DNSWTOT_INVLD,
!                                        FLXRANGE_W_CONSTR_UPLWCLR_INVLD,
!                                        FLXRANGE_W_CONSTR_UPLWTOT_INVLD,
!                                        FLXRANGE_W_CONSTR_UPSWCLR_INVLD,
!                                        FLXRANGE_W_CONSTR_UPSWTOT_INVLD,
!                                        FLXRANGE_W_CONSTR_UPWNCLR_INVLD,
!                                        FLXRANGE_W_CONSTR_UPWNTOT_INVLD,
!                                        FLXRANGE_W_INIT_DIRDIFF_INVLD,
!                                        FLXRANGE_W_INIT_DNLWCLR_INVLD, 
!                                        FLXRANGE_W_INIT_DNLWTOT_INVLD,
!                                        FLXRANGE_W_INIT_DNSWCLR_INVLD,
!                                        FLXRANGE_W_INIT_DNSWTOT_INVLD,
!                                        FLXRANGE_W_INIT_UPLWCLR_INVLD,
!                                        FLXRANGE_W_INIT_UPLWTOT_INVLD,
!                                        FLXRANGE_W_INIT_UPSWCLR_INVLD,
!                                        FLXRANGE_W_INIT_UPSWTOT_INVLD,
!                                        FLXRANGE_W_INIT_UPWNCLR_INVLD,
!                                        FLXRANGE_W_INIT_UPWNTOT_INVLD
!
! !Revision History:
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Added window channel flux profiles
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)           :: BadFlux
      REAL,    INTENT (IN)           :: BadValue
      INTEGER, INTENT (IN), OPTIONAL :: ILev
      REAL,    INTENT (IN), OPTIONAL :: PLev
      INTEGER, INTENT (IN)           :: RecNum
!
!
      IF (BadFlux == 1) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_DIRDIFF_INVLD,               & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            RVar1 = BadValue  )                            ! I
!
      ELSE IF (BadFlux == 2) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_UPLWCLR_INVLD,               & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 3) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_DNLWCLR_INVLD,               & ! I
                            FuncName ="FluxRange_Check ()",              & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 4) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_UPLWTOT_INVLD,               & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 5) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_DNLWTOT_INVLD,               & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 6) THEN
! 
         CALL WriteReport (FLXRANGE_W_INIT_UPSWCLR_INVLD,                & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 7) THEN
!
         CALL WriteReport (FLXRANGE_W_INIT_DNSWCLR_INVLD,                & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 8) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_UPSWTOT_INVLD,               & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 9) THEN
!
          CALL WriteReport (FLXRANGE_W_INIT_DNSWTOT_INVLD,               & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 10) THEN
! 
!##         CALL WriteReport (FLXRANGE_W_INIT_UPWNCLR_INVLD,              & ! I
!##                            FuncName = "FluxRange_Check ()",           & ! I
!##                            IVar1 = RecNum,                            & ! I
!##                            IVar2 = ILev,                              & ! I
!##                            RVar1 = BadValue,                          & ! I 
!##                            RVar2 = PLev )                               ! I
!
      ELSE IF (BadFlux == 11) THEN
!
!##         CALL WriteReport (FLXRANGE_W_INIT_DNWNCLR_INVLD,              & ! I
!##                            FuncName = "FluxRange_Check ()",           & ! I
!##                            IVar1 = RecNum,                            & ! I
!##                            IVar2 = ILev,                              & ! I
!##                            RVar1 = BadValue,                          & ! I 
!##                            RVar2 = PLev )                               ! I
!
      ELSE IF (BadFlux == 12) THEN
!
!##          CALL WriteReport (FLXRANGE_W_INIT_UPWNTOT_INVLD,             & ! I
!##                            FuncName = "FluxRange_Check ()",           & ! I
!##                            IVar1 = RecNum,                            & ! I
!##                            IVar2 = ILev,                              & ! I
!##                            RVar1 = BadValue,                          & ! I 
!##                            RVar2 = PLev )                               ! I
!
      ELSE IF (BadFlux == 13) THEN
!
!##          CALL WriteReport (FLXRANGE_W_INIT_DNWNTOT_INVLD,             & ! I
!##                            FuncName = "FluxRange_Check ()",           & ! I
!##                            IVar1 = RecNum,                            & ! I
!##                            IVar2 = ILev,                              & ! I
!##                            RVar1 = BadValue,                          & ! I
!##                            RVar2 = PLev )                               ! I
!
      ELSE IF (BadFlux == 101) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_DIRDIFF_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            RVar1 = BadValue )                             ! I
!
      ELSE IF (BadFlux == 102) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_UPLWCLR_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 103) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_DNLWCLR_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 104) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_UPLWTOT_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 105) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_DNLWTOT_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 106) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_UPSWCLR_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 107) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_DNSWCLR_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 108) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_UPSWTOT_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 109) THEN
!
          CALL WriteReport (FLXRANGE_W_CONSTR_DNSWTOT_INVLD,             & ! I
                            FuncName = "FluxRange_Check ()",             & ! I
                            IVar1 = RecNum,                              & ! I
                            IVar2 = ILev,                                & ! I
                            RVar1 = BadValue,                            & ! I 
                            RVar2 = PLev )                                 ! I
!
      ELSE IF (BadFlux == 110) THEN
!
!##          CALL WriteReport (FLXRANGE_W_CONSTR_UPWNCLR_INVLD,            & ! I
!##                            FuncName = "FluxRange_Check ()",            & ! I
!##                            IVar1 = RecNum,                             & ! I
!##                            IVar2 = ILev,                               & ! I
!##                            RVar1 = BadValue,                           & ! I
!##                            RVar2 = PLev )                                ! I
!
      ELSE IF (BadFlux == 111) THEN
!
!##          CALL WriteReport (FLXRANGE_W_CONSTR_DNWNCLR_INVLD,            & ! I
!##                            FuncName = "FluxRange_Check ()",            & ! I
!##                            IVar1 = RecNum,                             & ! I
!##                            IVar2 = ILev,                               & ! I
!##                            RVar1 = BadValue,                           & ! I
!##                            RVar2 = PLev )                                ! I
!
      ELSE IF (BadFlux == 112) THEN
!
!##          CALL WriteReport (FLXRANGE_W_CONSTR_UPWNTOT_INVLD,            & ! I
!##                            FuncName = "FluxRange_Check ()",            & ! I
!##                            IVar1 = RecNum,                             & ! I
!##                            IVar2 = ILev,                               & ! I
!##                            RVar1 = BadValue,                           & ! I
!##                            RVar2 = PLev )                                ! I
!
      ELSE IF (BadFlux == 113) THEN
!
!##          CALL WriteReport (FLXRANGE_W_CONSTR_DNWNTOT_INVLD,            & ! I
!##                            FuncName = "FluxRange_Check ()",            & ! I
!##                            IVar1 = RecNum,                             & ! I
!##                            IVar2 = ILev,                               & ! I
!##                            RVar1 = BadValue,                           & ! I
!##                            RVar2 = PLev )                                ! I
!
      END IF
!
!
      END SUBROUTINE FluxValue_Error
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE FLMod_Error (IPass, ErrTyp, RecNum)                              ! I
!
!*****************************************************************************
!
! !F90
! 
! !Name
!   FLMod_Error
!
! !Description
!   Routine ID -
!
!   Purpose - To generate a warning message via the Toolkit whenever an
!             error was encountered in the Fu-Liou radiative transfer model 
!
!
! !Input Parameters:
!   ErrTyp  - Flag indicating type of error
!   IPass   - Current pass through model
!   RecNum  - Current record number
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!    PGS_FLMODEL_25706.f - Parameters: FLMODEL_W_CONSTR_TRIDAG_PAUSE1,
!                                      FLMODEL_W_CONSTR_TRIDAG_PAUSE2,
!                                      FLMODEL_W_INITIAL_TRIDAG_PAUSE1,
!                                      FLMODEL_W_INITIAL_TRIDAG_PAUSE2,
!                                      FLMODEL_E_AEROSOL_OUT_OF_RANGE,
!                                      FLMODEL_E_BAD_AEROSOL_TYPE
!
! !Revision History:
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN) :: IPass
      INTEGER, INTENT (IN) :: ErrTyp
      INTEGER, INTENT (IN) :: RecNum
!
!
      IF (ErrTyp /= 0) THEN
!
          CALL WriteReport (FLMODEL_W_INPUT_OUT_OF_RANGE,       & ! I
                            FuncName = "FLMod_Error ()",        & ! I
                            StrVar= " (Record number = %i6,"//  & ! I
                            " Pass number = %i1,"//             & ! I
                            " Error code = %i3)",               & ! I
                            IVar1 = RecNum, IVar2 = IPass,      & ! I
                            IVar3 = ErrTyp)                       ! I
!
      END IF
!
!
      END SUBROUTINE FLMod_Error
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE SfcAlb_Error (IErr, RecNum, Info_Err1, Info_Err2)        ! I
!
!******************************************************************************
!
! !F90
!
! !Name:
!    SfcAlb_Error
!
! !Description:
!    Routine ID -
!
!    Purpose - To interface with the CERESlib wrappers for the EOSDIS Toolkit
!              error message utility for errors encountered while ingesting
!              the derivative tables used by the SARB tuning process
!             
!
! !Input Parameters:
!    IErr      - Integer flag indicating which error was encountered in the
!                ingestion process
!    Info_Err1 - Optional integer value 1 for diagnostic purposes
!    Info_Err2 - Optional integer value 2 for diagnostic purposes
!
! !Output Parameters:
!    None
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_SFCALB_25707.f - Parameters: SFCALBCALC_W_CERES_SID_INVALID, 
!                                     SFCALBCALC_W_ERBE_SID_INVALID
!
! !Revision History:
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN) :: Info_Err1
      INTEGER, INTENT (IN) :: Info_Err2
      INTEGER, INTENT (IN) :: IErr
      INTEGER, INTENT (IN) :: RecNum
!
!
      IF (IErr == 1) THEN
!
          CALL WriteReport (SFCALBCALC_W_ERBE_SID_INVALID,            & ! I
                            FuncName = "SfcAlb_Drv ()",               & ! I
                            IVar1 = RecNum,                           & ! I
                            IVar2 = Info_Err1,                        & ! I
                            IVar3 = Info_Err2)                          ! I
!
      ELSE IF (IErr == 2) THEN
!
          CALL WriteReport (SFCALBCALC_W_CERES_SID_INVALID,           & ! I
                            FuncName = "SfcAlb_Drv ()",               & ! I
                            IVar1 = RecNum,                           & ! I
                            IVar2 = Info_Err1)                          ! I
!
      END IF
!
!
      END SUBROUTINE SfcAlb_Error 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MonAerIngest_Error (IErr, Name_Sbr,            & ! I
                                     Info_Err1, Info_Err2)        ! I (Opt)
!
!******************************************************************************
!
! !F90
!
! !Name:
!    MonAerIngest_Error
!
! !Description:
!    Routine ID -
!
!    Purpose - Interfaces with the CERESlib wrappers for the EOSDIS Toolkit
!              error message utility for errors encountered while ingesting
!              the monthly file of gridded daily MODIS aerosol data
!             
!
! !Input Parameters:
!    IErr      - Integer flag indicating which error was encountered in the
!                ingestion process
!    Info_Err1 - Optional integer value 1 for diagnostic purposes
!    Info_Err2 - Optional integer value 2 for diagnostic purposes
!    Name_Sbr  - Name of the calling routine
!
! !Output Parameters:
!    None
!
! !Team-Unique Header:
!
!  Modules Used:
!    MonMODISAer_ErrParams - (SARBlib)  Params: IErr_AerHistDailyRead
!    msg                   - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_INGEST_25702.f - Parameters: INGEST_E_DAILY_MODISAER_READ
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Initial Version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER,           INTENT (IN), OPTIONAL :: Info_Err1
      INTEGER,           INTENT (IN), OPTIONAL :: Info_Err2
      INTEGER,           INTENT (IN)           :: IErr
      CHARACTER (LEN=*), INTENT (IN)           :: Name_Sbr
! 
!
      IF (IErr == IErr_AerHistDailyRead) THEN
!
          CALL WriteReport (INGEST_E_DAILY_MODISAER_READ,             & ! I
                            FuncName = Name_Sbr,                      & ! I
                            IVar1 = Info_Err1)                          ! I
!
      END IF
!
!
      END SUBROUTINE MonAerIngest_Error 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SARB_Error_Process
