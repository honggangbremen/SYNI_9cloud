MODULE Wrap_Up_SS7_2
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Finish7_2
!
! !Description:
!   Routine ID - M7.2.3
!
!   Purpose - A module of routines for finalizing Synoptoc SARB Subsystem 
!             processing
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
!   MOA_IO            - (CERESlib) Used by Sbr. Finish7_2
!   msg               - (CERESlib) Used by Sbr. Close_Syn_IO
!   QC_Fin            - (SARBlib)  Used by Sbr. SYNQC_Gen
!   SARB_IO_Utils     - (SARBlib)  Used by Sbr. Close_Syn_IO, SYNQC_Gen
!   SARB_Params       - (CERESlib) Used by Sbr. Close_Syn_IO
!   SARB_Var          - (SARBlib)  Used by Sbr. Finish7_2
!   SynSARB_IO_Params - (SynSARB)  Used by Sbr. Close_Syn_IO, Finish7_2,
!                                               SYNQC_Gen
!   SynSARB_Meta      - (SynSARB)  Used by Sbr. Finish7_2
! 
!  Internal Routines:
!    Close_Syn_IO - Closes input and output files used throughout 
!                   subsystem processing
!    Finish7_2    - Drives the finalization of subsystem processing
!    SYNQC_Gen    - Drives the production of the Synoptic SARB QC Report
!
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of Release 3 code
!
! !END
!
!*****************************************************************************
!
!
      USE SynSARB_Meta 
      USE MOA_IO_NESTED
      USE msg, ONLY: WriteReport
      USE QC_Fin
      USE SARB_IO_Utils
      USE SARB_Params
      USE SARB_QC_Params
      USE SARB_Var
      USE SynSARB_IO_Params

      USE NETCDF, ONLY: nf90_close  !HG-20260408 
!
!
  IMPLICIT NONE
!
      INCLUDE 'PGS_WRAPSYN_26006.f'
!
  CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Finish7_2 (fqc)                                          ! I
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Finish7_2
!
! !Description:
!   Routine ID - 7.2.3
!
!   Purpose - Drives the finalization of subsystem processing
!
!
! !Input Parameters:
!    fqc - QC report statistics
! 
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!   MOA_IO         - (CERESlib) Sbr. MOA_Close_Term
!   SARB_QC_PARAMS - (SARBlib)  Structures: fqc_type
!   SARB_Var       - (SARBlib)  Params: MOA_Unit
!   SYN_IO         - (CERESlib) Sbr. SYN_Close_Term
!   SynSARB_Meta   - (SynSARB)  Sbr. MetaDrv
!
!  Internal Routines Used:
!    Close_Syn_IO
!    SYNQC_Gen
! 
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of Release 3 code
!
! !END
!
!*****************************************************************************
!
!
      TYPE (fqc_type),   INTENT (IN)    :: fqc
!
!
! *** Close last opened  MOA files
!
!      CALL MOA_Close_Wrapper (MOA_Unit)                                  ! I
      CALL MOA_NCDF_Close_Wrapper ( MOA_Unit )!
!
! *** Write header, close the primary input and output files
!
      CALL Close_Syn_IO
!
!
! *** Generate the QC report
!
      CALL SYNQC_Gen (fqc)
!
!
! *** Generate metadata 
!
      CALL SynSARB_Meta_Drv
!
!
      END SUBROUTINE Finish7_2
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Close_Syn_IO
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   Close_Syn_IO
!
! !Description:
!   Routine ID - 7.2.3.1
!
!   Purpose - Closes input and output files used throughout subsystem processing
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
!  Modules Used:
!    SARB_IO_Utils     - (SARBlib)  Sbr. SARB_GenClose
!    SARB_Params       - (CERESlib) Structures: Spectral_TYPE_Header
!                                   Params: NumSpecInt_SW
!    SynSARB_IO_Params - (SynSARB)  Params: IUnit_TSI, OUnit_SYN, OUnit_SYNVB
!
! !Revision History:
!  Version 4.0  March 24, 2004 - SCCR #550
!               Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                 Added call to SARB_RdWrtError
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Initial version of Release 3 code
!
! !END
!
!*******************************************************************************
!
! 
      INTEGER                       :: IOS
      CHARACTER (LEN=15), PARAMETER :: Name_Sbr = 'Close_Syn_IO ()'
!      TYPE (Spectral_TYPE_Header)   :: WL_SpecInt_SW


! *** Write Header record to output product

!      WL_SpecInt_SW % SpecIntWL_SW ( 1:NumSpecInt_SW+1) = 0.0

!      WRITE (OUnit_SYN, rec=1, IOSTAT = IOS) WL_SpecInt_SW
!      IF (IOS /= 0)  CALL SARB_RdWrtError (27, IOS, Name_Sbr, IRec = 1)                               ! I

      
!
! *** Close files opened during processing
! HG-2-260408 comment out binary close
!      CALL SARB_GenClose (Name_Sbr, IUnit_TSI)                           ! I

      IOS = nf90_close(IUnit_TSI)                            ! +++ HG-20260408: close NetCDF TSI file      
      
!
      CALL SARB_GenClose (Name_Sbr, OUnit_SYN)                           ! I
!
!      IF (SYN_Subset_Flag == 1) THEN 
!       CALL SARB_GenClose (Name_Sbr, OUnit_SYN_Subset)                   ! I
!      ENDIF
!
!##      CALL SARB_GenClose (Name_Sbr, OUnit_SYNVB)                         ! I
!
!
      END SUBROUTINE Close_Syn_IO
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE SYNQC_Gen (fqc)                                          ! I
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   SYNQC_Gen
!
! !Description:
!   Routine ID - 7.2.3.2
!
!   Purpose - Drives the production of the Synoptic SARB QC Report
!
!
! !Input Parameters:
!    fqc - QC report statistics
! 
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    QC_Fin            - (SARBlib) Sbr. QC_Gen
!    SARB_IO_Utils     - (SARBlib) Sbr. SARB_GenClose, SARB_GenClose
!                                  Params: Flag_File_Format, Flag_File_Seq,
!                                          Flag_File_Write
!    SynSARB_IO_Params - (SynSARB) Params: LID_QC, OUnit_SYNQC
!
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Initial version of Release 3 code
!
! !END
!
!*******************************************************************************
!
!
      TYPE (fqc_type),   INTENT (IN)    :: fqc
!
      CHARACTER (LEN=12), PARAMETER :: Name_Sbr = 'SYNQC_Gen ()'
!
!
      CALL SARB_GenOpen (LID_QC, Flag_File_Write, Flag_File_Seq,         & ! I
                         Flag_File_Format, Name_Sbr,                     & ! I
                                                       OUnit_QC)           ! O
!
!
      CALL QC_Gen (ICnt_FOV, fqc, OUnit_QC)                                ! I
!
      CALL SARB_GenClose (Name_Sbr, OUnit_QC)                              ! I
!
      END SUBROUTINE SYNQC_Gen
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
END MODULE Wrap_Up_SS7_2
