MODULE SARB_General
!
!
! 
!*****************************************************************************
!
! !F90
!
! !Name
!   SARB_General
!
! !Description:
!   Routine ID -
!
!   Purpose - To provide a module of routines used by various other SARB
!             routines
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
!    ceres_meteor - (CERESlib) Used by Sbr. MOA_Inter
!    FL_IO_Params - (SARBlib)  Used by Sbr. Temp_Pres
!    VMax_Min     - (SARBlib)  Used by Sbr. Temp_Pres
!
!  Internal Routines:
!    MOA_Inter - Interpolates MOA data for floating levels
!    Temp_Pres - Converts temperature to pressure (hPa)
!
! !Revision History:
!  Version 3.3   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Modified Sbr. Temp_Pres
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                  Sbr. MOA_Inter - added (moved from Module FL_SetUp)
!  Version 2.0   November 21, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      USE ceres_meteor, ONLY: g_eln_inter
      USE FL_IO_Params
      USE VMax_Min
!
!
IMPLICIT NONE
!
CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE MOA_Inter (Lev_1, Lev_3, Lev_2,                              & ! I
                      Temp_1, Temp_3,                                   & ! I
                      Humid_1, Humid_3,                                 & ! I
                      Oz_1, Oz_3,                                       & ! I
                      Temp_2, Humid_2, Oz_2)                              ! O
!
!******************************************************************************
!
! !F90
!
! !Name:
!    MOA_Inter
!
! !Description:
!    Routine ID - 
!
!    Purpose - To interpolate meteorological and ozone data for a floating 
!              level between two fixed levels, a and b.
!
!
! !Input Parameters:
!    Humid_1 - Humidity at fixed level a
!    Humid_3 - Humidity at fixed level b
!    Lev_1   - Fixed pressure level a
!    Lev_2   - Pressure at floating level
!    Lev_3   - Fixed pressure level b
!    Oz_1    - Ozone at fixed level a
!    Oz_3    - Ozone at fixed level b
!    Temp_1  - Temperature at fixed level a
!    Temp_3  - Temperature at fixed level b
!
! !Output Parameters:
!    Humid_2 - Humidity at floating level
!    Oz_2    - Ozone at floating level  
!    Temp_2  - Temperature at floating level
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_meteor - Func. g_eln_inter
!
! !Revision History:
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                  No non-prolog changes--moved from Module FL_SetUp
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Release 2 Code
!  Version 1.0   March 21, 1996
!                Tammy Olaisen, SAIC, t.l.olaisen@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!******************************************************************************
!
!
      REAL, INTENT (IN)  :: Lev_1, Lev_3, Lev_2
      REAL, INTENT (IN)  :: Temp_1, Temp_3, Humid_1, Humid_3, Oz_1, Oz_3
      REAL, INTENT (OUT) :: Temp_2, Humid_2, Oz_2
!
!
! *** Interpolate to floating levels
!
      Temp_2 =  g_eln_inter (Temp_1, Temp_3, Lev_1, Lev_3, Lev_2)
!
      Humid_2 = g_eln_inter (Humid_1, Humid_3, Lev_1, Lev_3, Lev_2)
!
      Oz_2 = g_eln_inter (Oz_1, Oz_3, Lev_1, Lev_3, Lev_2)
!
!
END SUBROUTINE MOA_Inter
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - !
!
      SUBROUTINE Temp_Pres (stdp, stdt, nh, np, sfcp, sfcT, tlev,         & ! I
                                                             plev, istat)   ! O
!
!****************************************************************************
!
! !F90
!
! !Name
!   Temp_Pres
!
! !Description:
!   Routine ID -
!
!   Purpose - To convert temperature to pressure
!
!
! !Input Parameters:
!    nh   - Number of levels
!    np   - Number of levels in tropopause (only searches 1:np out of nh)
!    sfcp - Surface pressure
!    sfcT - Surface temperature
!    stdp - Presure levels
!    stdT - Temperatures at pressure levels
!    tlev - Cloud temperature 
!
! !Output Parameters:
!    istat - Status of placement function (0=ok,1=inversion,2=>warm,3=<cold )
!    plev  - Cloud pressure
!
! !Team-Unique Header:
!
!  Modules Used:
!    FL_IO_Params - (SARBlib)  Parameters: Default_REAL4_FL
!    VMAX_MIN     - (SARBlib)  Sbr. VMAX, VMIN
!
! !Revision History:
!  Version 3.3   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Using default values from SARBlib Mod. FL_IO_Params
!                    instead of from CERESlib Mod. ceres_defaults
!  Version 1.0   March 21, 1996
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!****************************************************************************
!
!
      INTEGER, INTENT (OUT) :: istat
      INTEGER, INTENT (IN)  :: nh
      INTEGER, INTENT (IN)  :: np
      REAL,    INTENT (OUT) :: plev
      REAL,    INTENT (IN)  :: sfcp
      REAL,    INTENT (IN)  :: sfcT
      REAL,    INTENT (IN)  :: stdp ( nh)
      REAL,    INTENT (IN)  :: stdt ( nh)
      REAL,    INTENT (IN)  :: tlev
!
      REAL               :: dt
      REAL               :: dtm
      INTEGER            :: iais
      INTEGER            :: IExit
      INTEGER, PARAMETER :: iflip = 1
      INTEGER            :: imax, imin
      INTEGER            :: K, KK
      INTEGER, PARAMETER :: maxlev = 100 
      REAL               :: ptp ( maxlev)
      REAL               :: ppp ( maxlev)
      REAL               :: rf
      REAL               :: rlr
      REAL               :: rmax, rmin
      REAL               :: rpl
!
!
! *** np use only troposphere
      plev = Default_REAL4_FL
      istat = 0
      IExit = 0
!
!
      DO K = 1, nh
       IF (iflip == 0) KK = K
       IF (iflip == 1) KK = (nh+1) - K
!
       ppp ( K) = stdp ( KK)
       ptp ( K) = stdt ( KK)
      END DO
!
      CALL VMIN (ptp, np, rmin, imin)
      CALL VMAX (ptp, np, rmax, imax)
!
! LAYER Of SFC (Find Sfc)....
      DO WHILE (ppp ( imax) > sfcp  .and.  ppp ( imax+1) > sfcp)
       ptp ( imax) = sfcT
       ppp ( imax) = sfcp
       rmax = sfcT
       imax = imax + 1
      END DO
!
      IF (ppp ( imax) > sfcp  .and.  ppp ( imax+1) < sfcp) THEN
       ptp ( imax) = sfcT
       ppp ( imax) = sfcp
       rmax = sfcT
       CALL VMAX (ptp, np, rmax, imax)
      END IF
!
! If SFC@ layer 1 set bottom layer to SFC
! If cloud Temp between (sfcTemp & MAX temp layer) ie Inversion.
! Force check to begin @ Surface 
! If cloud >5K colder than inversion place Above Inversion
!
      dtm = rmax - tlev  !!! DIFF ( Inversion - Cloud) Temperature
      iais = 0
!
      IF (imin <= imax) THEN
!     *** ALTITUDE OF MINIMUM Temp. is below MAX temp.(warm Tropopause) 
!         EXTREME ARTIC
!
       imax = imin !!Cold SFC
       imin = np   !! Default Tropopause
!
      ELSE
!     *** INVERSION SOLUTIONS (Cloud warmer than Sfc but less than Max
!         snding temp)
!
       IF (tlev > sfcT  .and.  tlev < rmax  .and.  imax > 1) THEN
!         *** Cloud near Inversion Max
!
        IF (dtm < 5.0) THEN
          DO WHILE (ppp ( imax) < sfcp .and. imax > 1)
           imax = imax - 1
          END DO
          ppp ( imax) = sfcp
          ptp ( imax) = sfcT
!
! +++if(sfcp.lt.ppp(imax) ) pause 'Low_lev press err.'
! +++print*, sfcp,sfcT,tlev,ppp(imax),rmax,imax,'Low Level Inversion'
!
        ELSE 
!        *** Cloud Placed Above a STRONG Inversion (but still wamrmer 
!            than sfc) Flag Above Inversion Soln.
!
         iais = 1
!
        END IF
!
       END IF
!
      END IF
!
! 
      DO K = imax, imin-1
!
       rlr = ptp ( K) - ptp ( K+1)
!
       IF (rlr < 0) THEN
!         *** LOCAL INVERSION
!
        IF (tlev >= ptp ( K) .and. tlev < ptp ( K+1) ) THEN
!             *** NON_INVERSION
!
         dt = ptp ( K) - tlev
         rf = dt/rlr
         rpl = ppp ( K) - ppp ( K+1)
         plev = ppp ( K) - rf*rpl
         istat = 1
         IExit = 1
!
        END IF
!
       ELSE
!         *** NOT_inversion
!
        IF (tlev <= ptp ( K) .and. tlev > ptp ( K+1) )then 
         dt = ptp ( K) - tlev
         rf = dt/rlr
         rpl = ppp ( K) - ppp ( K+1)
         plev = ppp ( K) - rf*rpl
         istat = 0
         IExit = 1
        END IF
!
       END IF
!
      END DO
!
      IF ( IExit == 0) THEN
       IF (tlev <= rmin) THEN
        istat = 3
        plev = ppp ( imin)
       ELSE IF (tlev > rmax) THEN
        istat = 2
        plev = ppp ( imax)
       END IF
      END IF
!
!
      END SUBROUTINE Temp_Pres
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
END MODULE SARB_General
