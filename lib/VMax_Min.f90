MODULE VMAX_MIN
! 
!******************************************************************************
!
! !F90
!
! !Name:
!   VMAX_MIN
!
! !Description:
!   Routine ID -
!
!   Purpose - To provide routines for determine the position of maximum
!             and minimum values within an array
!
!
! !Input Products:
!    NONE
!
! !Output Products:
!   NONE
!
! !Team-Unique Header:
!
!  Internal Routines:
!    VMAX - Determines position of maximum value within array
!    VMIN - Determines position of minimum value within array
!
! !Revision History:
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!
! !end
! 
!******************************************************************************
!
!
  IMPLICIT NONE
!
!
  CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE VMAX (VECTOR, LEN, VAL, IPOS)
! 
!******************************************************************************
!
! !F90
!
! !Name:
!   VMAX
!
! !Description:
!   Routine ID -
!
!   Purpose - To determine position of maximum value within an array
!
!
! !Input Products:
!    LEN    - Number of positions within array
!    VECTOR - Array
!
! !Output Products:
!   IPOS - Postion within array of maximum value
!   VAL  - Maximum value
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!
! !end
! 
!******************************************************************************
!
!
        INTEGER, INTENT (OUT) :: IPOS
        INTEGER, INTENT (IN)  :: LEN
        REAL,    INTENT (OUT) :: VAL
        REAL,    INTENT (IN)  :: VECTOR ( LEN)
!
        INTEGER :: I
!
!
        VAL = -10.**32
        DO I = 1, LEN
           IF (VECTOR ( I) > VAL) THEN
               VAL = VECTOR ( I)
               IPOS = I
           END IF
        END DO
!
END SUBROUTINE VMAX
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE VMIN (VECTOR, LEN, VAL, IPOS)
! 
!******************************************************************************
!
! !F90
!
! !Name:
!   VMIN
!
! !Description:
!   Routine ID -
!
!   Purpose - To determine position of minimum value within an array
!
!
! !Input Products:
!    LEN    - Number of positions within array
!    VECTOR - Array
!
! !Output Products:
!   IPOS - Postion within array of minimum value
!   VAL  - Maximum value
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!
! !end
! 
!******************************************************************************
!
!
        INTEGER, INTENT (OUT) :: IPOS
        INTEGER, INTENT (IN)  :: LEN
        REAL,    INTENT (OUT) :: VAL
        REAL,    INTENT (IN)  :: VECTOR ( LEN)
! 
        INTEGER :: I
!
!
        VAL = 10.**32
!
        DO I = 1, LEN
            IF (VECTOR ( I) < VAL) THEN
                VAL = VECTOR ( I)
                IPOS = I
            END IF
        END DO
!
END SUBROUTINE VMIN
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE VMAX_MIN
