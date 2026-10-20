MODULE SARB_MOA_Utils
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SARB_MOA_Utils
!
! !Description:
!    Routine ID -
!
!    Purpose - A module of routines for MOA I/O operations specific to 
!              SARB processors that execute on a monthly basis
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
!  Local Parameters:
!    MOA_Interval - Array of the number of hours between MOA files for each 
!                   of the possible MOA grids
!
!  Internal Routines:
!    IntervalMOA_Open - Opens a new MOA file upon determining it is needed
!
!  Modules Used: 
!   ceres_status - (CERESlib) Used by Sbr. IntervalMOA_Open
!   MOA_IO       - (CERESlib) Used by Sbr. IntervalMOA_Open
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
    USE ceres_status, ONLY: READ_FILE
!    USE MOA_IO
    USE MOA_IO_NESTED
!
    IMPLICIT NONE
!
!
!      INTEGER, PARAMETER :: MOA_Interval ( 30) =                           &
!                                       (/1, 6, 6, 6, 6, 6, 6, 6, 6, 6,     &
!				         0, 0, 0, 0, 0, 0, 0, 0, 0, 6,     &
!				         0, 0, 0, 0, 0, 0, 0, 0, 0, 6/)
      INTEGER, PARAMETER :: MOA_Interval ( 35) =                          &
                                      (/1, 6, 6, 6, 6, 6, 6, 6, 6, 6,     &
				         0, 0, 0, 0, 0, 0, 0, 0, 0, 6,     &
				         0, 0, 0, 0, 0, 0, 0, 0, 0, 6,     &
					 0, 0, 0, 0, 3/)
!
!
    CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE IntervalMOA_Open (MOAHour, PCF_LID, NumHr_MOAInt,   & ! I
                                                         MOA_Units)    ! I/O
!
!******************************************************************************
!
! !F90
!
! !Name:
!   IntervalMOA_Open
!
! !Description:
!    Routine ID -
!
!    Purpose - Opens a new MOA file upon determining it is needed
!
!
! !Input Parameters:
!    MOAHour      - Hour of month [0 ... 743]
!    NumHr_MOAInt - Time interval in hours between MOA granules
!    PCF_LID      - Logical file ID for access into PCF
!
!  Input/Output Parameters:
!    MOA_Units - Array of MOA file unit numbers returned from Toolkit file 
!                opening routines
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header: 
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!   ceres_status - (CERESlib) Params: READ_FILE
!   MOA_IO       - (CERESlib) Sbr. MOA_Close_Term, MOA_Open_Term
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
      INTEGER, INTENT (INOUT) :: MOA_Units ( 2)
      INTEGER, INTENT (IN)    :: MOAHour
      INTEGER, INTENT (IN)    :: NumHr_MOAInt
      INTEGER, INTENT (IN)    :: PCF_LID
!
      INTEGER, SAVE :: IVer_MOA
      INTEGER       :: UnitNum, Stat
      TYPE (MOA_HEADER_TYPE) :: MOA_Header
!
!
! *** Determine if a new MOA file needs to be opened
!
      IF ((MOD (MOAHour,NumHr_MOAInt) == 1) .OR. MOAHour == 0) THEN
!     *** Open new MOA file 
!
          IF (MOAHour > 0) THEN
              IVer_MOA = INT ( (MOAHour)/NumHr_MOAInt) + 2
          ELSE
              IVer_MOA = 1
          END IF
!

!          CALL MOA_Open_Term (PCF_LID, READ_FILE,                   & ! I
!                                         UnitNum,                   & ! O
!                                                  Vers=IVer_MOA)      ! I (Opt)

          CALL MOA_NCDF_Open (PCF_LID,                             &  ! I
                                      UnitNum, Stat,               &  ! O
                                            MOA_Header, IVer_MOA)     ! I

!
          MOA_Units ( 2) = UnitNum
!
          IF (MOAHour == 0) THEN
!
               MOA_Units ( 1) = UnitNum
               MOA_Units ( 2) = -9
!  
          END IF
!
!
      ELSE IF ((MOD (MOAHour,NumHr_MOAInt) == 0)  .AND.  MOAHour > 0) THEN
!     *** Close open MOA file that is no longer needed and assign new
!         MOA file unit number
!
!          CALL MOA_Close_Term ( MOA_Units ( 1)) 
          CALL MOA_NCDF_Close ( MOA_Units ( 1) )
          MOA_Units ( 1) = MOA_Units ( 2)
          MOA_Units ( 2) = -9
!
      END IF
!
! 
      END SUBROUTINE IntervalMOA_Open
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SARB_MOA_Utils
