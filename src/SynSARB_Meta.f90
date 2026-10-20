  MODULE SynSARB_Meta
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SynSARB_Meta
!
! !Description:
!
!  Routine ID - M7.2.3.3
!
!  Purpose - Contains the routines necessary to generate the 
!            Synoptic SARB Subsystem metatdata files
!
!
! !Input Parameters:
!   NONE
!
! !Output Parameters:
!   NONE
!
! !Team Unique Header:
!
!  Internal Subroutines:
!    SynSARB_Meta_Drv - Contains the processes needed to write metadata
!
!  Modules Used:
!    ceres_status      - (CERESlib) Used by Sbr. SynSARB_Meta_Drv
!    f90_kind          - (CERESlib) Used by Sbr. SynSARB_Meta_Drv
!    meta_util         - (CERESlib) Used by Sbr. SynSARB_Meta_Drv 
!    msg               - (CERESlib) Used by Sbr. SynSARB_Meta_Drv
!    SARB_IO_Utils     - (SARBlib)  Used by Sbr. SynSARB_Meta_Drv
!    SARB_Var          - (SARBlib)  Used by Sbr. SynSARB_Meta_Drv
!    SynSARB_IO_Params - (SynSARB)  Used by Sbr. SynSARB_Meta_Drv
!
!  External Files:
!    PGS_SARBMETA_25726.f - Used by Sbr. SynSARB_Meta_Drv
!
! !Revision History:
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Intitial version of code
!
! !END
!
!******************************************************************************
!
!
      USE ceres_status, ONLY: QA_PASS
      USE meta_util, ONLY: Bounding_NULL, Meta_NULL, Meta_type,             &
                           WriteMeta, Bounding_type 
      USE msg
      USE SARB_IO_Utils
      USE SARB_Var
      USE SynSARB_IO_Params
!
!
      IMPLICIT NONE
!
      INCLUDE 'PGS_SARBMETA_25726.f'
!
 CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
    SUBROUTINE SynSARB_Meta_Drv 
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SynSARB_Meta_Drv
!
! !Description:
!
!  Routine ID - 7.2.3.3
!
!  Purpose - To drive generation of the metadata files for the Instantaneous 
!            SARB Subsystem's output products
!
!
! !Input Parameters:
!   NONE
!
! !Output Parameters:
!   NONE
!
! !Team Unique Header:
!
!  Modules Used:
!    ceres_status      - (CERESlib) Params: QA_PASS
!    meta_util         - (CERESlib) Sbr. WriteMeta
!                                   Params: Bounding_NULL, Meta_NULL
!                                   Structures: Bounding_type, Meta_type
!    msg               - (CERESlib) Sbr. WriteReport
!    SARB_IO_Utils     - (SARBlib)  Sbr. DaysPerMonth
!    SARB_Var          - (SARBlib)  Params: DataYear, DataMonth
!    SynSARB_IO_Params - (SynSARB)  Params: LID_QCMCF, LID_QCMETA, 
!                                           LID_SYN_MCF, LID_SYN_META
!
!  External Files:
!    PGS_SARBMETA_25726.f - Parameters: SARBMETA_E_MAIN_SYNI,
!                                       SARBMETA_E_SYNMAIN_QC
!
! !Revision History:
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Intitial version of code
!
! !END
!
!******************************************************************************
!
!
      CHARACTER (LEN=27)   :: BeginDT
      TYPE (Bounding_type) :: SYNI_Boundary
      CHARACTER (Len=11)   :: DateStub
      CHARACTER (LEN=25)   :: dummy
      CHARACTER (LEN=27)   :: EndDT
      INTEGER              :: MCF_ID
      INTEGER              :: META_ID
      INTEGER              :: MetaWrite_Status
      INTEGER              :: NumDays_Mon
!
!
! *** Set bounding rectangle values
!
      SYNI_Boundary = Bounding_NULL
!
      SYNI_Boundary % East  = 360.0
      SYNI_Boundary % West  =   0.0
      SYNI_Boundary % South = 180.0
      SYNI_Boundary % North =   0.0
!
!
! *** Define beginning and ending date values
!
      DateStub = "    -  -01T"
      WRITE (DateStub(1:4), '(i4)') DataYear
      WRITE (DateStub(6:7), '(a2)') CMonth
!
      BeginDT = DateStub//"00:01:01.000000Z"
!
! *** Ending date and time
!
      NumDays_Mon = DaysPerMonth (DataMonth, DataYear)
      WRITE (DateStub(9:10), '(i2)') NumDays_Mon
      EndDT   = DateStub//"23:59:59.999999Z"
!
!
      MCF_ID  = LID_SYN_MCF
      META_ID = LID_SYN_META
!
!
! *** Write WriteMeta for DIRECT ACCESS BINARY file
!
      CALL WriteMeta (MCF_LID=MCF_ID, Output_LID=LID_SYN,                     &
                      BeginDT = BeginDT, EndDT = EndDT,                       &
                      QAFlag = QA_PASS, QAFlagExplain = "No error detected.", &
                      BoundingRect=SYNI_Boundary,                             &
                      LoadFile_LID = META_ID, WriteHead = 0,                  &
                      write_status = MetaWrite_Status)
!
      IF (MetaWrite_Status /= 0) THEN
!    *** Error writing meta data file
!  
         CALL WriteReport (SARBMETA_E_MAIN_SYNI,                         & ! I
                           FuncName = "SynSARB_Meta_Drv ()")               ! I
!
      END IF
!
!
! *** Write metadata for ASCII QC report file
!
      MCF_ID = LID_QCMCF
      META_ID = LID_QCMETA
!      
      CALL WriteMeta (MCF_LID=MCF_ID, Output_LID=LID_QC,                      &
                      BeginDT = BeginDT, EndDT = EndDT,                       &
                      QAFlag = QA_PASS, QAFlagExplain = "No error detected.", &
                      BoundingRect=SYNI_Boundary,                             &
                      LoadFile_LID = META_ID, WriteHead = 0,                  &
                      write_status = MetaWrite_Status)
!
      IF (MetaWrite_Status /= 0) THEN
!    *** Error writing meta data file
!
         CALL WriteReport (SARBMETA_E_SYNMAIN_QC,                        & ! I
                           FuncName = "SynSARB_Meta_Drv ()")               ! I
!
!
      END IF



RETURN
! DEAD CODE BELOW


      IF (SYN_Subset_Flag == 1) THEN
!
! *** Write WriteMeta for DIRECT ACCESS BINARY Subset file
!
       MCF_ID  = LID_SYNS_MCF
       META_ID = LID_SYNS_META
!
       CALL WriteMeta (MCF_LID=MCF_ID, Output_LID=LID_SYN_Subset,              &
                       BeginDT = BeginDT, EndDT = EndDT,                       &
                       QAFlag = QA_PASS, QAFlagExplain = "No error detected.", &
                       BoundingRect=SYNI_Boundary,                             &
                       LoadFile_LID = META_ID, WriteHead = 0,                  &
                       write_status = MetaWrite_Status)
!
       IF (MetaWrite_Status /= 0) THEN
!    *** Error writing meta data file
!  
          CALL WriteReport (SARBMETA_E_MAIN_SYNI,                         & ! I
                            FuncName = "SynSARB_Meta_Drv ()")               ! I
!
       END IF
!
      ENDIF
!
!
      END SUBROUTINE SynSARB_Meta_Drv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  END MODULE SynSARB_Meta

