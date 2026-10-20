MODULE GADS_Aer
!
!
!******************************************************************************
!
! !F90
!
! !Name:
!   GADS_Aer 
!
! !Description:
!    Routine ID -
!
!    Purpose - To provide i/o interfaces for the GADS aerosol optical
!              depth file
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
!    find_gads_AOT
!    GADSAer_Ingest
!
!  Modules Used:
!    f90_kind - Used by module main body
!    io       - Used by Sbr. GADSAer_Ingest
!    msg      - Used by Sbr. GADSAer_Ingest
!
!  External Files Used:
!    PGS_GADSAER_25715.f - Used by Sbr. GADSAer_Ingest
!
! !Revision History:
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
! 
! !END
!
!******************************************************************************
!
  USE f90_kind
  USE io
  USE msg
!
!
  IMPLICIT NONE
!
     INTEGER, PARAMETER :: GADSAer_LID = 21
     INTEGER, PARAMETER :: NumReg_GADS = 2664
!
     REAL (REAL4), SAVE :: aot ( NumReg_GADS)
     INTEGER     , SAVE :: GADSAer_Unit
!
!
     INCLUDE 'PGS_GADSAER_25715.f'
!
!
  CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE GADSAer_Ingest
!
!******************************************************************************
!
! !F90
!
! !Name:
!   GADSAer_Ingest
!
! !Description:
!    Routine ID -
!
!    Purpose - To open and read the input file containing the GADS
!              aerosol optical depth values
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
!   io  - Sbr. OpenFile
!   msg - Sbr. WriteReport
!
!  External Files Used:
!    PGS_GADSAER_25715.f - Parameters: GADSAER_E_FILE_OPEN_ERROR,
!                                      GADSAER_E_FILE_READ_ERROR,
!                                      GADSAER_W_FILE_OPEN_SUCCESS,
!                                      GADSAER_W_FILE_READ_SUCCESS
!
! !Revision History:
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
! 
! !END
!
!******************************************************************************
!
!
      INTEGER :: IOS
      INTEGER :: GADSAer_Length
!##integer :: i, j
!##real :: ilat, jlong, aot_value
!
      GADSAer_Length = NumReg_GADS * 4
      CALL OpenFile (LogicID = GADSAer_LID, status = IOS, &
                     handle = GADSAer_Unit, mode = "R", access = "D", &
                     format = "U", reclen = GADSAer_Length)
!
      IF (IOS == 0) THEN
!     *** Successful open.
!
          CALL WriteReport (GADSAER_W_FILE_OPEN_SUCCESS,                & ! I
                            FuncName = "GADSAer_Ingest ()")               ! I
!
      ELSE
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (GADSAER_E_FILE_OPEN_ERROR,                  & ! I
                            FuncName = "GADSAer_Ingest ()")               ! I
!
      END IF
!
      READ (GADSAer_Unit,rec=1,iostat=IOS) aot
      IF (IOS == 0) THEN
!     *** Successful read.
!
          CALL WriteReport (GADSAER_W_FILE_READ_SUCCESS,                & ! I
                            FuncName = "GADSAer_Ingest ()")               ! I
!
      ELSE
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (GADSAER_E_FILE_READ_ERROR,                  & ! I
                            FuncName = "GADSAer_Ingest ()")               ! I
!
      END IF
!##do i = -90, 90, 5
!##    do j = 1, 360, 5
 !##       ilat = real ( i)
  !##      jlong = real ( j)
  !##      aot_value = find_gads_AOT (ilat, jlong)
  !##      print*, ' I=',I,' J=',J,' aot_value=',aot_value
  !##  end do
!##end do
!##stop
!
      END SUBROUTINE GADSAer_Ingest
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
      FUNCTION find_gads_AOT (rlat, rloni) RESULT (find_gads_AOT_result)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   find_gads_AOT
!
! !Description:
!    Routine ID -
!
!    Purpose - To determine which value of the aot array corresponds to
!              the provided latitudal and longitudinal coordinates
!
!
! !Input Parameters:
!   rlat  - Latitudinal value
!   rloni - Initial longitidinal value
! 
! !Output Parameters:
!   find_gads_AOT_result - Value of aot array corresponding to the 
!                          input coordinates
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
! 
! !END
!
!******************************************************************************
!
!
      REAL, INTENT (IN)  :: rlat
      REAL, INTENT (IN)  :: rloni
!
      REAL    :: find_gads_AOT_result
      INTEGER :: ii
      INTEGER :: ilat
      INTEGER :: ilon
      REAL    :: rlon
!
!
      rlon = rloni
      if (rlon >= 180 ) rlon = rlon - 360.0

      ilat = - INT ( ((rlat-90.0) / 5.0) - 1)
      ilon =   INT ( ((rlon+180.0) / 5.0) + 1) 
! 
      ii = (ilat-1) * 72 + ilon
!
      find_gads_AOT_result = aot ( ii)

      END FUNCTION find_gads_AOT
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE GADS_Aer
