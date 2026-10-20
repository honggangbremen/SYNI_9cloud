MODULE IGBP_Utils
!
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   IGBP_Utils
!
! !Description:
!   Routine ID -
!
!   Purpose - Contains parameters and routines for use with the IGBP map
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
!    IGBP_Ingest - Opens, reads, and closes IGBP map
!
!  Key Local Parameters:
!    IGBP(SceneDescription) - IGBP scene type index
!
!  Modules Used:
!    ceres_status - (CERESlib) Used by Sbr. IGBP_Ingest
!    f90_kind     - (CERESlib) Used by Sbr. IGBP_Ingest
!    io           - (CERESlib) Used by Sbr. IGBP_Ingest
!    msg          - (CERESlib) Used by Sbr. IGBP_Ingest
!                                            
!  External Files Used: 
!    PGS_IGBP_25721.f - Used by Sbr. IGBP_Ingest 
!
! !Revision History
!   Version 3.0
!   Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!       Intitial version of code
!
! !END
!
!
!*******************************************************************************
!
!
    USE ceres_status, ONLY: DIRECT_FILE, UNFMT_FILE, READ_FILE
    USE f90_kind, ONLY: INT1
    USE io, ONLY: CloseFile, OpenFile
    Use msg, ONLY: WriteReport
!
    IMPLICIT NONE
!
!
    INCLUDE 'PGS_IGBPUTIL_25721.f'
!
!
! *** IGBP Scene Type Indexes
!
      INTEGER, PARAMETER :: IGBPEvgNdlFor = 1
      INTEGER, PARAMETER :: IGBPEvgBrdFor = 2
      INTEGER, PARAMETER :: IGBPDcdNdlFor = 3
      INTEGER, PARAMETER :: IGBPDcdBrdFor = 4 
      INTEGER, PARAMETER :: IGBPMixFor    = 5
      INTEGER, PARAMETER :: IGBPClsdShrbs = 6
      INTEGER, PARAMETER :: IGBPOpnShrbs  = 7 
      INTEGER, PARAMETER :: IGBPWoodSavan = 8
      INTEGER, PARAMETER :: IGBPSavan     = 9
      INTEGER, PARAMETER :: IGBPGrassLand = 10
      INTEGER, PARAMETER :: IGBPWetLand   = 11
      INTEGER, PARAMETER :: IGBPCropLand  = 12
      INTEGER, PARAMETER :: IGBPUrban     = 13
      INTEGER, PARAMETER :: IGBPCropMos   = 14
      INTEGER, PARAMETER :: IGBPAntSnow   = 15
      INTEGER, PARAMETER :: IGBPDesert    = 16
      INTEGER, PARAMETER :: IGBPOcean     = 17
      INTEGER, PARAMETER :: IGBPTundra    = 18
      INTEGER, PARAMETER :: IGBPFreshSnow = 19
      INTEGER, PARAMETER :: IGBPSeaIce    = 20
!
!
    CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE IGBP_Ingest (PCF_LID,                                & ! I
                                       igbp)                            ! O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   IGBP_Ingest
!
! !Description:
!   Routine ID -
!
!   Purpose - Opens, reads, and closes IGBP map
!
!
! !Input Parameters:
!   PCF_LID - Logical identifier for IGBP map in Process Control File
!
! !Output Parameters:
!   igbp - 10-minute igbp scene type map
!    
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: DIRECT_FILE, UNFMT_FILE, 
!                                          READ_FILE
!    f90_kind     - (CERESlib) Parameters: INT1
!    io           - (CERESlib) Sbr. CloseFile, OpenFile
!    msg          - (CERESlib) Sbr. WriteReport
!                                           
!    
!  External Files Used: 
!    PGS_IGBPUTIL_25721.f - Parameters: IGBPUTIL_E_IGBP_CLOSE,
!                                       IGBPUTIL_E_IGBP_OPEN, 
!                                       IGBPUTIL_E_IGBP_READ 
!
! !Revision History
!   Version 3.0
!   Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!       Intitial version of code
!
! !END
!
!
!*******************************************************************************
!
!
      INTEGER (INT1), INTENT (OUT) :: igbp (2160,1080)
      INTEGER,        INTENT (IN)  :: PCF_LID
!
      INTEGER :: IGBP_Length
      INTEGER :: IOS 
      INTEGER :: Unit_IGBP
!
!
! *** Open input file 
!
      IGBP_Length = 2160*1080+8

      CALL OpenFile (LogicID = PCF_LID, status = IOS,                   & ! I
                     handle = Unit_IGBP, mode = READ_FILE,              & ! I 
                     access = DIRECT_FILE,                              & ! I
                     format = UNFMT_FILE, reclen = IGBP_Length)           ! I
!
      IF (IOS /= 0) THEN
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (IGBPUTIL_E_IGBP_OPEN,                       & ! I
                            FuncName = "IGBP_Ingest ()",                & ! I
                            StrVar = " IOS = %i4 ",                     & ! I
                            IVar1 = IOS)                                  ! I
!
      END IF
!
!
! *** Read contents of input file
!
      READ (Unit_IGBP, rec=1, IOSTAT = IOS) igbp
      IF (IOS /= 0) THEN
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (IGBPUTIL_E_IGBP_READ,                       & ! I
                            FuncName = "IGBP_Ingest ()",                & ! I
                            StrVar = " IOS = %i4",                      & ! I
                            IVar1 = IOS)                                  ! I
!
      END IF
!
!
! *** Close input file 
!
      CALL CloseFile (file_handle = Unit_IGBP, status = IOS)
      IF (IOS /= 0) THEN
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (IGBPUTIL_E_IGBP_CLOSE,                      & ! I
                            FuncName = "IGBP_Ingest ()",                & ! I
                            StrVar = " IOS = %i4",                      & ! I
                            IVar1 = IOS)                                  ! I
!
      END IF
!
!
      END SUBROUTINE IGBP_Ingest
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
END MODULE IGBP_Utils
