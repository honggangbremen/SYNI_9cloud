MODULE AOTFIT
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   AOTFIT
!
! !Description:
!
!    Purpose - Apply log fit corrections to aerosol optical depths
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
!    lsrsqr           - Derive correction coefficients for aerosol optical depth 
!    aotstdwl_MultiWL - Apply correction to multiple wavelength aerosol optical depths
!    aotstdwl_OneWL   - Apply correction to single wavelength aerosol optical depth
!
!  Modules Used:
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!

!
      IMPLICIT NONE
!
      INTERFACE aotstdwl
          MODULE PROCEDURE aotstdwl_MultiWL, aotstdwl_OneWL
      END INTERFACE
!
      CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE lsrsqr ( x, y, npts,                              & ! I
                                      beta, alpha, corr)             ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   lsrsqr
!
! !Description:
!    Purpose - Derive correction coefficients for aerosol optical depth
!
! 
! !Input Parameters:
!    x, y, npts
!
! !Output Parameters:
!    beta, alpha, corr
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
! 
      REAL,    INTENT (OUT) :: alpha
      REAL,    INTENT (OUT) :: beta
      REAL,    INTENT (OUT) :: corr
      INTEGER, INTENT (IN)  :: npts
      REAL,    INTENT (IN)  :: x ( npts)
      REAL,    INTENT (IN)  :: y ( npts)
!
!
      REAL :: denom
      REAL :: resid ( npts)
      REAL :: rn
      REAL :: sumx
      REAL :: sumxy
      REAL :: sumx2
      REAL :: sumy
      REAL :: sumy2
      REAL :: xmean
      REAL :: xstd
      REAL :: yfit ( npts)
      REAL :: ymean
      REAL :: ystd
      REAL xstdarg, ystdarg, dnom1, dnom2
!
      rn = REAL (npts)
!
      sumx  = SUM (x ( 1:npts) )
      sumy  = SUM (y ( 1:npts) )
      sumx2 = SUM (x ( 1:npts) * x ( 1:npts) )
      sumy2 = SUM (y ( 1:npts) * y ( 1:npts) )
      sumxy = SUM (x ( 1:npts) * y ( 1:npts) )
!
      xmean = sumx / rn
      xstd  = SQRT ( (sumx2-rn*xmean*xmean) / (rn-1) ) 
      ymean = sumy / rn 
      ystd  = SQRT ( (sumy2-rn*ymean*ymean) / (rn-1) )
      dnom1 = rn*sumx2-sumx*sumx
      dnom2 = rn*sumy2-sumy*sumy
      if(dnom1 >= 0.0 .and. dnom2 >= 0.0) then
       denom = SQRT(dnom1) * SQRT(dnom2)
      else
       denom = 1.0
      end if

      corr  = ( rn*sumxy-sumx*sumy ) / denom
!
      beta  =  (sumy*sumx-rn*sumxy) / (sumx*sumx-rn*sumx2)  ! slope
      alpha =  (sumy-beta*sumx)  / rn                       ! intercept
!
      yfit  = alpha + x * beta
      resid = y - yfit
!
!
      END SUBROUTINE lsrsqr 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE aotstdwl_MultiWL (aotin, wlin, nin, aotout, wlout, nout, beta)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   aotstdwl_MultiWL
!
! !Description:
!    Purpose - Apply correction to multiple wavelength aerosol optical depths
!
! 
! !Input Parameters:
!    aotin, wlin, nin, nout, wlout
!
! !Output Parameters:
!    aotout, beta
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      REAL,    INTENT (OUT) :: beta
      INTEGER, INTENT (IN)  :: nin
      INTEGER, INTENT (IN)  :: nout
      REAL,    INTENT (IN)  :: aotin ( nin)
      REAL,    INTENT (OUT) :: aotout ( nout)
      REAL,    INTENT (IN)  :: wlin ( nin)
      REAL,    INTENT (IN)  :: wlout ( nout)
!
      REAL    :: alpha
!      REAL    :: beta
      REAL    :: corr
      INTEGER :: I
      INTEGER :: J
      INTEGER :: NJ
      REAL    :: x ( nin)
      REAL    :: y ( nin)
!	
!
      J = 0
      DO I = 1, nin
          IF ( aotin ( I) >= 0.0 .AND. aotin ( I) < 100.0) THEN
              J = J + 1
              x ( J) = LOG (wlin ( I))
              IF (aotin ( I) >= 0.00001) THEN
                  y ( J) = LOG (aotin ( I))
              ELSE
                  y ( J) = LOG (0.00001)
              END IF
          END IF
      END DO
      NJ = J
!
!
      IF (NJ > 1) THEN
          CALL lsrsqr ( x, y, NJ, beta, alpha, corr)
      ELSE IF (NJ == 1) THEN
          beta = 0.0
          alpha = y ( NJ)
      END IF
!
!        
      IF (NJ > 0) THEN
          DO I = 1, nout
  	      aotout ( I) = EXP (beta * LOG (wlout ( I)) + alpha)
	  END DO
      END IF
!
!
      END SUBROUTINE aotstdwl_MultiWL
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE aotstdwl_OneWL (aotin, wlin, nin, aotout, wlout, beta)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   aotstdwl_OneWL
!
! !Description:
!    Purpose - Apply correction to single wavelength aerosol optical depth
!
! 
! !Input Parameters:
!    aotin, wlin, nin, wlout
!
! !Output Parameters:
!    aotout, beta
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      REAL,    INTENT (OUT) :: beta
      INTEGER, INTENT (IN)  :: nin
      REAL,    INTENT (IN)  :: aotin ( nin)
      REAL,    INTENT (OUT) :: aotout 
      REAL,    INTENT (IN)  :: wlin ( nin)
      REAL,    INTENT (IN)  :: wlout
!
      REAL    :: alpha
!      REAL    :: beta
      REAL    :: corr
      INTEGER :: I
      INTEGER :: J
      INTEGER :: NJ
      REAL    :: x ( nin)
      REAL    :: y ( nin)
!	
!
      J = 0
      DO I = 1, nin
          IF ( aotin ( I) >= 0.0 .AND. aotin ( I) < 100.0) THEN
              J = J + 1
              x ( J) = LOG (wlin ( I))
              IF (aotin ( I) >= 0.00001) THEN
                  y ( J) = LOG (aotin ( I))
              ELSE
                  y ( J) = LOG (0.00001)
              END IF
          END IF
      END DO
      NJ = J
!
!
      IF (NJ > 1) THEN
          CALL lsrsqr ( x, y, NJ, beta, alpha, corr)
      ELSE IF (NJ == 1) THEN
          beta = 0.0
          alpha = y ( NJ)
      END IF
!
!        
      IF (NJ > 0) THEN
          aotout = EXP (beta * LOG (wlout) + alpha)
      END IF
!
!
      END SUBROUTINE aotstdwl_OneWL
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE AOTFIT
