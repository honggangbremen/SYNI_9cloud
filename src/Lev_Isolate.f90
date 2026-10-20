MODULE Lev_Isolate
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    Lev_Isolate
!
! !Description:
!    Routine ID -
!
!    Purpose - To provide a module of routines for isolating a subset 
!              of fluxes from the model profile at the levels required  
!              by output products
!
!
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Description:
!
!  Internal Routines:
!    FlxDiv - Calculates the heating/cooling rate of a specified LAYER in 
!             deg/day between selected levels
!    ixp    - Identifies index of pressure level within the "full" profile
!             that will become part of the "subset" profile
!    SelFlx - Drives the levels isolation process
!
! !Revision History:
!  Version 3.3   December 20, 2002 - SCCR #408  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Sbr. SelFlx - modified
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                  Sbr. SelFlx - modified
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
    IMPLICIT NONE
!
!
    CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
       SUBROUTINE SelFlx (nv1, npsel, psel, omit, omit_int, tol,      & ! I
                          plx, fup, fdn,                              & ! I
                          pls, fups, fdns, degday)                      ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    SelFlx
!
! !Description:
!    Routine ID -
!
!    Purpose - To drive the isolation of the vertical profile levels that 
!              will be written to the output product
!
!
! !Input Parameters:
!   fdn      - Downwards flux at pressure levels
!   fup      - Upwards flux at pressure levels
!   npsel    - Number of levels to extract Fluxes &/or Heating Rates
!   nv1      - Number of input levels 
!   omit     - Missing data value for reals
!   omit_int - Default value for integers
!   plx      - Input pressure levels
!   psel     - Pressure levels to extract Fluxes &/or Heating Rates
!   tol      - Tolerance in fractional atmosphere (0.01 = 10 mb at sfc, 
!              1 mb at 100 mb)
!
! !Output Parameters:
!   degday - Heating/cooling rate of LAYER in deg/day between selected levels
!   fdns   - Downwards flux at pressure levels plc
!   fups   - Upwards flux at pressure levels plc
!   pls    - Pressure levels found as closest match to specified ones in psel
!
! !Team-Unique Header:
!
!  Internal Routines Used:
!    flxdiv
!    ixp
!
! !Revision History:
!  Version 3.3   December 20, 2002 - SCCR #408  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Addition of variable omit_int to input calling arguments
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. Modified call to Func. ixp in order to pass in
!                    INT4_Dflt instead of REAL4_Dflt
!                 2. Modified the IF statement in order to account for
!                    the INT4_Dflt value  
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: npsel
      INTEGER, INTENT (IN)  :: nv1
      REAL   , INTENT (OUT) :: degday ( npsel-1)
      REAL   , INTENT (IN)  :: fdn ( nv1)
      REAL   , INTENT (OUT) :: fdns ( npsel)
      REAL   , INTENT (IN)  :: fup ( nv1)
      REAL   , INTENT (OUT) :: fups ( npsel)
      REAL   , INTENT (IN)  :: omit
      INTEGER, INTENT (IN)  :: omit_int
      REAL   , INTENT (OUT) :: pls ( npsel) 
      REAL   , INTENT (IN)  :: plx ( nv1) 
      REAL   , INTENT (IN)  :: psel ( npsel)
      REAL   , INTENT (IN)  :: tol
!
      INTEGER :: IC
      INTEGER :: IXP1
!
!
      DO IC = 1, npsel
!
       IXP1 = ixp (plx, nv1, psel ( IC), tol, omit_int)
!
	   IF (IXP1 >= 1 .AND. IXP1 < omit_int) THEN
        pls ( IC) = plx ( IXP1)
        fups ( IC) = fup ( IXP1)
        fdns ( IC) = fdn ( IXP1)
       ELSE
		pls ( IC) = omit
        fups ( IC) = omit
        fdns ( IC) = omit 
       END IF
!
      END DO 
!
      CALL FlxDiv (fups, fdns, pls, degday, npsel, omit)
!
!
      END SUBROUTINE SelFlx
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      FUNCTION ixp (plx, nv1, psel, tol, omit) RESULT (ixp_result)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    ixp
!
! !Description:
!    Routine ID -
!
!    Purpose - Identify index of pressure level within the "full" profile
!              that will become part of the "subset" profile
!
!
! !Input Parameters:
!    nv1  - Number of pressure levels
!    omit - Missing value
!    plx  - Pressure levels
!    psel - Pressure level for which to select indexes
!    tol  - Tolerance in fractional atmosphere (0.001 = 1mb at sfc)
!
! !Output Parameters:
!    ixp_result - Pressure level index
!
! !Team-Unique Header
!
! !Revision History:
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. Modified the type for the variable ixp_result
!                    from REAL to INTEGER 
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
        INTEGER, INTENT (IN) :: nv1
        INTEGER, INTENT (IN) :: omit
        REAL   , INTENT (IN) :: plx ( nv1)
        REAL   , INTENT (IN) :: psel
        REAL   , INTENT (IN) :: tol
!
        REAL    :: diff  
        INTEGER :: I
        INTEGER :: ixp_result
        REAL    :: tolu
!
!
   IF (psel >= 100) THEN
    tolu = tol * psel   
   ELSE
    tolu = tol * 100
   END IF
!
!
   diff = 1.0E+36
   ixp_result = omit
   DO I = 1, nv1
    IF (ABS (plx ( I) - psel) < diff ) THEN
        diff = ABS (plx ( I) - psel)
		IF ( diff < tolu ) ixp_result = I
    END IF
   END DO
!
      END FUNCTION ixp
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!
      SUBROUTINE FlxDiv (fu, fd, ps, degday, nl, omit)
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    FlxDiv
!
! !Description:
!    Routine ID -
!
!    Purpose - To calculate the heating/cooling rate of LAYER in deg/day 
!              between selected levels 
!
!
! !Input Parameters:
!    fd   - Downward fluxes
!    fu   - Upward Fluxes
!    nl   - Number of levels to select
!    omit - Missing data flag
!    ps   - Pressure levels found as closest match to specified ones in psel
!
! !Output Parameters:
!    degday - Heating/cooling rate of LAYER in deg/day between selected 
!             levels
!
! !Team-Unique Header
!
! !Revision History:
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
        INTEGER, INTENT (IN)  :: nl
        REAL   , INTENT (OUT) :: degday ( nl-1)
        REAL   , INTENT (IN)  :: fd ( nl)
        REAL   , INTENT (IN)  :: fu ( nl)
        REAL   , INTENT (IN)  :: omit
        REAL   , INTENT (IN)  :: ps ( nl)
!
        INTEGER, PARAMETER :: ml = 100
        REAL               :: dp ( ml)
        REAL, PARAMETER    :: fac = 8.4418
        REAL               :: fn ( ml) 
        INTEGER            :: I
!
!
   DO I = 1, nl
!
    IF ( fd ( I) < 0   .or.   fu ( I) < 0) THEN
        fn ( I) = omit
    ELSE
        fn ( I) = fd ( I) - fu ( I)
    END IF
!
    IF ( I == nl ) THEN
        dp ( I) = omit
            ELSE IF ( ps ( I) < 0   .or.   ps ( I+1) < 0 ) THEN
        dp ( I) = omit
    ELSE
        dp ( I) = ps ( I+1) - ps ( I)
    END IF
!
   END DO
!
!
   DO I = 1, nl-1
!
    IF (      fn ( I) /= omit .and. fn ( I+1) /= omit   &
                .and. dp ( I) /= omit) THEN
        degday ( I) = fac * (fn ( I) - fn ( I+1)) / dp ( I)
    ELSE
        degday ( I) = omit 
    END IF
!
   END DO
!
!
      END SUBROUTINE FlxDiv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!
END MODULE Lev_Isolate
