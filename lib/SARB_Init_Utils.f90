MODULE SARB_Init_Utils
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SARB_Init_Utils
!
! !Description:
!    Routine ID -
!
!    Purpose - A module of routines used by SARB during initialization of 
!              subsystem processing
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
!    Define_WidthWN  - Defines the instrument-dependent width of the 
!                      window channel and the subsequent range of 
!                      allowable window flux values
!    InstrIdx_Define - Defines the instrument index parameter
!
!  Modules Used:
!    ceres_constants  - (CERESlib) Used by Sbr. Define_WidthWN
!    SARBInput_Params - (SARBlib)  Used by Sbr. InstrIdx_Define
!                                       
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!Initial version of code
!
! !END
!
!*****************************************************************************
!
!
    USE SARBInput_PARAMS
    USE ceres_constants
!
!
    IMPLICIT NONE
!
    CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
      SUBROUTINE InstrIdx_Define (Instr_Name,                            & ! I
                                              Instr_Idx)                   ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   InstrIdx_Define
!
! !Decription:
!    Routine ID -
!
!    Purpose - Defines the instrument index parameter
!
! 
! !Input Parameters:
!   Instr_Name - Instrument character name
!
! !Output Parameters:
!   Instr_Idx - Instrument integer index
!
! !Team-Unique Header:
!
!  Modules Used:    (Parameters followed by "*" are defined by this routine)
!   SARBInput_Param - (SARBlib)  Params: CERES_FM1, CERES_FM2, CERES_FM3,
!                                        CERES_FM4, CERES_FM5, CERES_PFM
!                                       
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
! SCCRNNN Added CERES_FM6
!Initial version of code
! !END
!
!*****************************************************************************
!
!
      INTEGER,           INTENT (OUT) :: Instr_Idx
      CHARACTER (LEN=3), INTENT (IN)  :: Instr_Name
!
!
      SELECT CASE (Instr_Name)
          CASE ("PFM")
               Instr_Idx = CERES_PFM
          CASE ("FM1")
               Instr_Idx = CERES_FM1
          CASE ("FM2")
               Instr_Idx = CERES_FM2
          CASE ("FM3")
               Instr_Idx = CERES_FM3
          CASE ("FM4")
               Instr_Idx = CERES_FM4
          CASE ("FM5")
               Instr_Idx = CERES_FM5
          CASE ("FM6")
               Instr_Idx = CERES_FM6
      END SELECT 
!
!
      END SUBROUTINE InstrIdx_Define
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Define_WidthWN (Instr_Idx,                              & ! I
                                 WN_Width, WNflx_Low, WNflx_High)          ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Define_WidthWN
!
! !Decription:
!    Routine ID -
!
!    Purpose - Defines the instrument-dependent width of the window channel
!              and the subsequent range of allowable window flux values
!
! 
! !Input Parameters:
!   Instr_Idx - Instrument index
!
! !Output Parameters:
!   WN_Width   - Width (microns) of the window channel for specified instrument
!   WNflx_High - Maximum allowable window channel flux value
!   WNflx_Low  - Minimum allowable window channel flux value
!
! !Team-Unique Header:
!
!  Modules Used:    (Parameters followed by "*" are defined by this routine)
!   ceres_constants - (CERESlib) Params: WN_CHAN_WIDTH 
!                                       
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!Initial version of code
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: Instr_Idx
      REAL,    INTENT (OUT) :: WN_Width
      REAL,    INTENT (OUT) :: WNflx_High
      REAL,    INTENT (OUT) :: WNflx_Low
!
!
! *** Capture window channel width for the current instrument and 
!     define the window flux range limits accordingly.
!
      WN_Width = WN_CHAN_WIDTH ( Instr_Idx)
!
      WNflx_Low = 0.0 * WN_Width
      WNflx_High = 100.0 * WN_Width
!
!
      END SUBROUTINE Define_WidthWN 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SARB_Init_Utils
