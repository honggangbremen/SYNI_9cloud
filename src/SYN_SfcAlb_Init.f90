MODULE SYN_SfcAlb_Init
!
!
!****************************************************************************
!
! !F90
!
! !Name
!   SYN_SfcAlb_Init
!
! !Description:
!    Routine ID - M7.2.1.4
!
!    Purpose - To provide a module of routines dedicated to the ingestion of
!              the input data files accessed in determining 
!              the surface albedo value for a CERES region  
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
!    SfcAlb_IO_Drv - Drives the ingestion of input data required by
!                    the surface albedo correction technique
!
!  Modules Used: 
!    ASSALUT_Sfcalb     - (SARBlib)  Used by Sbr. SfcAlb_IO_Drv
!    FLSA_LUT_Utils     - (SARBlib)  Used by Sbr. SfcAlb_IO_Drv
!    HCM_Ocn_Alb_IO     - (SARBlib)  Used by Sbr. SfcAlb_IO_Drv
!    Monthly_SfcAlb_IO  - (SARBlib)  Used by Sbr. SfcAlb_IO_Drv
!    SynSARB_IO_Params  - (SynSARB)  Used by Sbr. SfcAlb_IO_Drv
!    ZJIN_Mod        - (SARBlib)  Used by Sbr. SfcAlb_IO_Drv
!
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial Version of code
!
! !END
!
!****************************************************************************
!
!
    USE FLSA_LUT_Utils
    USE ASSALUT_Sfcalb
!    USE Monthly_SfcAlb_IO
    USE SynSARB_IO_Params
!    USE ZJIN_Mod, ONLY: ZJIN_Init
!
    IMPLICIT NONE
!
    CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE SfcAlb_IO_Drv 
!
!****************************************************************************
!
! !F90
!
! !Name
!   SfcAlb_IO_Drv
!
! !Description:
!    Routine ID - 7.2.1.4
!
!    Purpose - Drives the ingestion of the input data files accessed
!              in determining the surface albedo value for a CERES FOV.
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
!    ASSALUT_LUT_Ingest - (SARBLIB) Sbr. ASSALUT_LUT_Ingest
!    FLSA_LUT_Utils    - (SARBlib)  Sbr. FLSA_LUT_Ingest
!    Monthly_SfcAlb_IO - (SARBlib)  Sbr. MonSA_Ingest
!    SynSARB_IO_Params - (InstSARB) Params: LID_IGBP, LID_SALB,
!                                           LID_SfcAlb_LUT, LID_ZJIN,
!                                           StreamID, TwoStream_Ver
!    ZJIN_Mod       - (SARBlib)  Sbr. ZJIN_Init
!
! !Revision History:
! Version X.X   August 29, 2005
!               Scott Zentz, s.m.zentz@larc.nasa.gov
!               Added call to ASSALUT_LUT_Ingest
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial Version of code
!
! !END
!
!****************************************************************************
!
!
      INTEGER, PARAMETER :: IIO_Idx = 1
      INTEGER, PARAMETER :: IVer_SfcAlb = 1
!
!
! *** Ingest surface albedo look up table for Fu-Liou.
!
      StreamID = TwoStream_Ver
      CALL FLSA_LUT_Ingest (LID_SfcAlb_LUT, StreamID)                 ! I
!
!
! *** Ingest Assalut look up table
      CALL ASSALUT_LUT_Ingest (LID_Assalut) 
!  
! *** Ingest ZJin surface albedo data
!
 !     CALL ZJIN_Init (LID_ZJIN)                                      ! I
!
!
! *** Ingest historical surface albedo file
!
!      CALL MonSA_Ingest (LID_SALB, IIO_Idx, IVer_SfcAlb)              ! I
!
!
! *** Generate ivector and igbpa arrays
!
!      CALL gen_ivector (LID_IGBP)                                     ! I
!
!
      END SUBROUTINE SfcAlb_IO_Drv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SYN_SfcAlb_Init

