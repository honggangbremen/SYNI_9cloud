 MODULE Ancill_Init
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Ancill_Init
!
! !Description:
!    Routine ID - M5.1.8
!
!    Purpose - To open ancillary input data files needed for both the
!              Instantaneous SARB and the Synoptic SARB Subsystems
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
!    AerClim_OpenDrive - Determines the availability of the possible aerosol
!                        climatological ancilliary input data sets and drives 
!                        the opening of the available data file
!    Open_MOA          - Drives the opening of the MOA input file(s) and the 
!                        retrieval of MOA header data pertinent to the  
!                        remainder of Subsystem processing
!
!  Modules Used:
!   COLLINS_ASSIMILATION - (SARBlib)  Used by Sbr. AerClim_OpenDrive
!   gfdl_aer_clim        - (SARBlib)  Used by Sbr. AerClim_OpenDrive
!   MOA_IO               - (CERESlib) Used by Sbr. Open_MOA
!   msg                  - (CERESlib) Used by Sbr. AerClim_OpenDrive
!   pcf                  - (CERESlib) Used by Sbr. AerClim_OpenDrive
!   SARBInput_Params     - (SARBlib)  Used by Sbr. AerClim_OpenDrive
!   SARB_Var             - (SARBlib)  Used by Sbr. AerClim_OpenDrive, Open_MOA
!
!  External Files Used:
!    PGS_ANCINIT_25725.f - Used by: AerClim_OpenDrive
!
! !Revision History:
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Modified Sbr. AerClim_OpenDrive
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial Version of Module 
!                 1. Breakout from Sbr. Start5 (Module Init_SS5, InstSARB)  
!
! !END
!
!*****************************************************************************
!
!
!      USE COLLINS_ASSIMILATION
!      USE gfdl_aer_clim
!      USE MOA_IO
      USE MOA_IO_NESTED
      USE msg
      USE pcf
      USE SARBInput_Params
      USE SARB_Var
!
!
      IMPLICIT NONE
!
!
      INCLUDE 'PGS_ANCINIT_25725.f'
!
!
      CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
      SUBROUTINE Open_MOA (LID_MOA)                                       ! I
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Open_MOA
!
! !Description:
!    Routine ID - 5.1.8
!
!    Purpose - Drives the opening of the MOA input file(s) and the 
!              retrieval of MOA header data pertinent to the remainder 
!              of Subsystem processing
!
! 
! !Input Parameters:
!    LID_MOA - Logical ID for the MOA files in the PCF
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!    MOA_IO   - (CERESlib) Sbr. MOA_Open_Wrapper
!                          Structures: MOA_HEADER_TYPE
!    SARB_Var - (SARBlib)  Parameters:IHour, MOA_FixLev*, MOA_GrdIdx*, 
!                                      MOA_Unit*
!                                 
!  Input Files:
!    MOA - MOA data for current hour
!
! !Revision History:
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Breakout from Sbr. Start5 (Module Init_SS5) into own 
!                   Sbr. and module 
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN) :: LID_MOA
!
      TYPE (MOA_HEADER_TYPE) :: MOA_Header
!
!
! *** Open MOA input file 
!
!      CALL MOA_Open_Wrapper (LID_MOA, IHour,                           & ! I
!                                          MOA_Unit, MOA_Header)          ! O

      CALL MOA_NCDFOpen_Wrapper ( LID_MOA, IHour,                    &  ! I
                                              MOA_Unit, MOA_Header)     ! I

!
!
! *** Save MOA grid index, necessary for retrieval of regional data for
!     each FOV.
!
      MOA_GrdIdx = MOA_Header % GridIdx_Output
!
!
! *** Save the MOA vertical profile heights (in hPa) for later use in
!     building the vertical profiles for each FOV
!
      MOA_FixLev ( 1:58) = MOA_Header % Pres_MP ( 1:58)
!
!
      END SUBROUTINE Open_MOA
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
END MODULE Ancill_Init
