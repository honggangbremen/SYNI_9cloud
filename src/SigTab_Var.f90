MODULE SigTab_Var
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SigTab_Var
!
! !Description:
!    Routine ID - 
!
!    Purpose - To provide a module of parameters associated with the SARB
!              Sigma Tables 
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
! !Revision History:
!  Version 3.2   December 24, 2001 - SCCR #315
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 1. Added sigma table version number parameter
!  Version 2.4   March 15, 1999 - SCCR #132
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                 1. Changed value of parameter maxtune from 7 to 8.
!                 2. Changed value of parameter mflx from 2 to 5.
!                 3. Changed dimensions for abs_flux_tol and pct_flux_tol
!                    arrays from 2 to mflx.
!  Version 2.0   November 21, 1997  SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
! 
      INTEGER, PARAMETER :: maxtune = 8        !! # of Tuning Variables 
      INTEGER, PARAMETER :: mcase = 100
      INTEGER, PARAMETER :: mflx = 5           !! MAX # of fluxes       
      INTEGER, PARAMETER :: msid = 50
      INTEGER, PARAMETER :: mtol = 10          !! Source dependent tolerances  
      REAL,    PARAMETER :: omit = -9999.
!
      REAL,    SAVE :: abs_flux_tol ( mflx)
      INTEGER, SAVE :: iav ( 3, mcase)
      INTEGER, SAVE :: lut ( -msid:msid)
      INTEGER, SAVE :: ncase
      INTEGER, SAVE :: nsid
      REAL,    SAVE :: par_sig ( maxtune, msid)
      REAL,    SAVE :: pct_flux_tol ( mflx, mtol)
      REAL,    SAVE :: rwv ( mcase)
      INTEGER, SAVE :: isigma_table_id
      INTEGER, SAVE :: sigma_table_version
!
!
END MODULE SigTab_Var

