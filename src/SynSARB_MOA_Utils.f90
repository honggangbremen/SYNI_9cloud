MODULE SynSARB_MOA_Utils
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SynSARB_MOA_Utils
!
! !Description:
!    Routine ID -
!
!    Purpose - A module of routines for MOA I/O operations specific to 
!              the Synoptic SARB Subsystem
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
!    SynMOA_Open - Opens a new MOA file upon determining it is needed
!
!  Modules Used: 
!   Ancill_Init       - (SARBlib)  Used by Sbr. SynMOA_Open
!   ceres_status      - (CERESlib) Used by Sbr. SynMOA_Open
!   MOA_IO            - (CERESlib) Used by Sbr. SynMOA_Open
!   SARBInput_Params  - (SARBlib)  Used by Sbr. SynMOA_Open
!   SARB_Var          - (SARBlib)  Used by Sbr. SynMOA_Open
!   SynSARB_IO_Params - (SynSARB)  Used by Sbr. SynMOA_Open
!
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Initial version of code
!
! !END
!
!******************************************************************************
!
!
    USE Ancill_Init
    USE ceres_status, ONLY: READ_FILE
    USE MOA_IO_NESTED
    USE SARBInput_Params
    USE SARB_Var
    USE SynSARB_IO_Params
!
    IMPLICIT NONE
!
    CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE SynMOA_Open (MonthHour, PCF_LID)                         ! I
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SynMOA_Open
!
! !Description:
!    Routine ID -
!
!    Purpose - Opens a new MOA file upon determining it is needed
!
!
! !Input Parameters:
!    MonthHour - Hour of month [1 ... 744]
!    PCF_LID   - Logical file ID for access into PCF
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header: 
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!   Ancill_Init       - (SARBlib)  Sbr. Open_MOA
!   ceres_status      - (CERESlib) Params: READ_FILE
!   MOA_IO            - (CERESlib) Sbr. MOA_Close_Term, MOA_Open_Term
!   SynSARB_IO_Params - (SARBlib)  Params: NumHr_MOAInt
!   SARB_Var          - (SARBlib)  Params: IHour*, MOA_Unit*
!
! !Revision History:
!  Version 3.4  June 20, 2003 - SCCR #445  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN) :: PCF_LID
      INTEGER, INTENT (IN) :: MonthHour
!
      INTEGER :: IVer_MOA, Ios
      INTEGER :: UnitNum
      TYPE (MOA_HEADER_TYPE) :: MOA_Header

!
!
! *** Determine if a new MOA file needs to be opened
!
      print*,'moa utils ',MonthHour
      
      IF ((MonthHour > 1  .AND.  MOD (MonthHour-1,NumHr_MOAInt) == 0)     &
           .OR.  MonthHour == 2) THEN
!     *** Open new MOA file 
!
          IVer_MOA = INT ( (MonthHour-1)/NumHr_MOAInt) + 2
! 
      print*,'moa utils ',IVer_MOA
          CALL MOA_NCDF_Open (LID_MOA,                       &
                                  UnitNum, Ios, 	     &  ! O
                                       MOA_Header, IVer_MOA)    ! I
      print*,'moa utils units ',UnitNum

!          CALL MOA_Open_Term (LID_MOA, READ_FILE,                   & ! I
!                                         UnitNum,                   & ! O
!                                                  Vers=IVer_MOA)      ! I (Opt)
!
          IF (MonthHour > 2) THEN
              CALL MOA_NCDF_Close ( MOA_Unit ( 1) )
!	      CALL MOA_Close_Term ( MOA_Unit ( 1)) 
              MOA_Unit ( 1) = MOA_Unit ( 2)
!
          END IF
          MOA_Unit ( 2) = UnitNum
!
! 
      ELSE IF (MonthHour == 1) THEN
!     *** For first hour of month, call routine to also define grid index 
!         and fixed pressure heights
!
          IHour = 0
          CALL Open_MOA (LID_MOA)
!
      END IF
!
!
      END SUBROUTINE SynMOA_Open
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SynSARB_MOA_Utils
