MODULE UpTropHum
!
!******************************************************************************
!
! !F90
! 
! !Name:
!
! !Description:
!   Routine ID - M5.2....
!
!   Purpose - To provide routines to calculate values of various upper and
!             lower tropospheric humidity variables
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
!    GeoPtPro_Calc           - Calculates geopotential height profile
!    HumAdj_Apply            - Adjusts humidity profile for the constrained 
!                              pass through the radiative transfer model
!    UpperLower_Hum_Separate - Calculates the lower and upper tropospheric  
!                              column humidity values
!    UTH_CalcDrv             - Drives calculation of the upper tropospheric 
!                              relative humidity
!    utrh                    - Calculates the upper tropospheric relative
!                              humidity 
!
!  Modules Used:
!    ceres_meteor     - (CERESlib) Used by Sbr. UpperLower_Hum_Separate, utrh
!    SARBInput_Params - (SARBlib)  Used by Sbr. HumAdj_Apply,  
!                                               UpperLower_Hum_Separate,
!                                               UTH_CalcDrv
!
! !Revision History:
!  Version 3.3  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Modified Sbr. GeoPtPro_Calc
!                 Modified Sbr. HumAdj_Apply
!                 Modified Sbr. UpperLower_Hum_Separate
!                 Modified Sbr. UTH_CalcDrv
!  Version 2.4   March 15, 1999 - SCCR #132
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
   USE ceres_meteor
   USE SARBInput_Params
!
!
   IMPLICIT NONE
!
!
   CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE HumAdj_Apply ( NLev, LHt, Ratio_LT, Ratio_UT,        & ! I
                                                          SpHum)        ! I/O
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   HumAdj_Apply
!
! !Description:
!    Routine ID -
!
!    Purpose - To adjust each value within a specific humidity profile in 
!              accordance to whether the value corresponds to the upper or 
!              lower troposphere
!            
!
! !Input Parameters:
!    LHt      - Array of the heights (hPa) of the levels in the specific 
!               humidity profile
!    NLev     - Number of total levels in the specific humidity profile
!    Ratio_LT - Ratio applicable to SpHum array elements corresponding to 
!               lower tropspheric heights
!    Ratio_UT - Ratio applicable to SpHum array elements corresponding to 
!               upper tropspheric heights
!
!  Input/Output Parameters:
!    SpHum - Array containing the specific humidity (g kg-1) corresponding to 
!            each level in the LHt array
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Parameters: UTH_LowerBound, UTH_UpperBound
! 
! !Revision History:
!  Version 3.3  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Added NLev, LHt, SpHum, Ratio_LT, and Ratio_UT to 
!                   calling argument list to remove dependence on
!                   Mod. SARB_Var and therefore increase reusability
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)    :: NLev
      REAL,    INTENT (IN)    :: LHt ( NLev)
      REAL,    INTENT (IN)    :: Ratio_LT
      REAL,    INTENT (IN)    :: Ratio_UT
      REAL,    INTENT (INOUT) :: SpHum ( NLev)
!
      INTEGER :: I
!
!
! *** Adjust humidity profile for constrained calculations
!
      DO I = 1, NLev
!
           IF (LHt ( I) >= UTH_LowerBound) THEN
!          *** Apply lower tropospheric adjustment
!
               SpHum ( I) = SpHum ( I) * Ratio_LT
!
           ELSE IF (       LHt ( I) <= UTH_LowerBound                  &
                     .AND. LHt ( I) >= UTH_UpperBound) THEN
!          *** Apply upper tropospheric adjustment
!
               SpHum ( I) = SpHum ( I) * Ratio_UT
!
           END IF
!
      END DO
!
!
      END SUBROUTINE HumAdj_Apply 
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE UpperLower_Hum_Separate ( NLev, LHt, SpHum,            & ! I
                                                   wv_LTrop, wv_UTrop)    ! O
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   UpperLower_Hum_Separate
!
! !Description:
!    Routine ID -
!
!    Purpose - Calculates the lower and upper tropospheric column 
!              humidity values
!
!
! !Input Parameters:
!    LHt   - Array of the heights (hPa) of the levels in the specific 
!            humidity profile
!    NLev  - Number of total levels in the specific humidity profile
!    SpHum - Array containing the specific humidity (g kg-1) corresponding to 
!            each level in the LHt array
!
! !Output Parameters:
!   wv_LTrop - Lower troposphere water vapor (cm)
!   wv_UTrop - Upper troposphere water vapor (cm)
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_meteor     - (CERESlib) Func. ql_to_pw
!    SARBInput_Params - (SARBlib)  Parameters: UTH_LowerBound, UTH_UpperBound
!
! !Revision History:
!  Version 3.3  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Added NLev, LHt, SpHum, wv_LTrop, and wv_UTrop to 
!                   calling argument list to remove dependence on
!                   Mod. SARB_Var and therefore increase reusability
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
! 
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: NLev
      REAL,    INTENT (IN)  :: LHt ( NLev)
      REAL,    INTENT (IN)  :: SpHum ( NLev)
      REAL,    INTENT (OUT) :: wv_LTrop
      REAL,    INTENT (OUT) :: wv_UTrop
!
      INTEGER :: I 
      REAL    :: LowLev ( NLev)
      REAL    :: LowHum ( NLev)
      INTEGER :: NumLowLev 
      INTEGER :: NumUpLev
      REAL    :: UpLev ( NLev)
      REAL    :: UpHum ( NLev)
!
!
! *** Initialize
!
      NumLowLev = 0
      NumUpLev = 0
      LowLev ( 1:NLev) = 0.0
      LowHum ( 1:NLev) = 0.0
      UpLev ( 1:NLev) = 0.0
      UpHum ( 1:NLev) = 0.0
!
!
! *** Calculate initial upper and lower troposphere humidity values
!
      DO I = 1, NLev
          IF (LHt ( I) <= UTH_LowerBound .AND. LHt ( I) >= UTH_UpperBound) THEN
!         *** This level is part of the upper troposphere
!
              NumUpLev = NumUpLev + 1
              UpLev ( NumUpLev) = LHt ( I)
              UpHum ( NumUpLev) = SpHum ( I)
!
          ELSE IF ( LHt ( I) > UTH_LowerBound) THEN
!         *** This level is part of the lower troposphere
!
              NumLowLev = NumLowLev + 1
              LowLev ( NumLowLev) = LHt ( I)
              LowHum ( NumLowLev) = SpHum ( I)
!
          END IF
      END DO
!
!
      wv_LTrop = ql_to_pw (NumLowLev, LowHum ( 1:NumLowLev),           &
                                                    LowLev ( 1:NumLowLev) )
      wv_UTrop = ql_to_pw (NumUpLev, UpHum ( 1:NumUpLev),              &
                                                    UpLev ( 1:NumUpLev) )
!
!
      END SUBROUTINE UpperLower_Hum_Separate
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE UTH_CalcDrv ( NLev, LHt, SpHum, TempPro, SfcGeo,     & ! I
                                                   GeoPt, UTH_Out)      ! O
!
!******************************************************************************
!
! !F90
! 
! !Name:
!   UTH_CalcDrv
!
! !Description:
!   Routine ID -
!
!   Purpose - To drive the calculation of the upper tropospheric relative
!             humidity, required by the SARB constrainment algorithm, based 
!             on the profiles of temperature and specific humidity used
!             by the initial pass of the radiative transfer model
!
!
! !Input Parameters:
!    LHt     - Array of the heights (hPa) of the levels in the specific 
!              humidity profile
!    NLev    - Number of total levels in the specific humidity profile
!    SfcGeo  - Surface geopotential height
!    SpHum   - Array containing the specific humidity (g kg-1) corresponding 
!              to each level in the LHt array
!    TempPro - Array containing the temperatures (K) corresponding to each
!              level in the LHt array
!
! !Output Parameters:
!    GeoPt   - Array of geopotential heights corresponding to each level in
!              the LHt array
!    UTH_Out - Upper tropospheric relative humidity
! 
! !Team-Unique Header:
!
!  Internal Routines Used:
!    GeoPtPro_Calc
!    utrh
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Parameters: UTH_LowerBound, UTH_UpperBound
!
! !Revision History:
!  Version 3.3  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Added GeoPt, NLev, LHt, SfcGeo, SpHum, TempPro, and UTH_Out
!                   to calling argument list to remove dependence on
!                   Mod. SARB_Var and therefore increase reusability
!  Version 2.4   March 15, 1999 - SCCR #132
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN)  :: NLev
      REAL,    INTENT (OUT) :: GeoPt ( NLev)
      REAL,    INTENT (IN)  :: LHt ( NLev)
      REAL,    INTENT (IN)  :: SfcGeo
      REAL,    INTENT (IN)  :: SpHum ( NLev)
      REAL,    INTENT (IN)  :: TempPro ( NLev)
      REAL,    INTENT (OUT) :: UTH_Out
!
!
! *** Calculate geopotential height profile
!
      GeoPt ( 1:NLev) = 0.0
      CALL GeoPtPro_Calc (NLev, LHt ( 1:NLev), TempPro ( 1:NLev),      & ! I
                          SpHum ( 1:NLev), SfcGeo,                     & ! I
                                                 GeoPt ( 1:NLev))        ! O
!
!
! *** Compute upper tropospheric humidity value
!
      CALL utrh (NLev, GeoPt ( 1:NLev), LHt ( 1:NLev),                 & ! I
                 TempPro ( 1:NLev), SpHum ( 1:NLev),                   & ! I
                 UTH_UpperBound, UTH_LowerBound,                       & ! I
                                                 UTH_Out)                ! O

!
!
      END SUBROUTINE UTH_CalcDrv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE GeoPtPro_Calc (nal, pl, tl, ql, SfcGeo,               & ! I
                                                           zz)           ! O
! 
!******************************************************************************
!
! !F90
! 
! !Name:
!
! !Description:
!   Routine ID -
!
!   Purpose - To calculate the geopotential height profile
!
!
! !Input Parameters:
!    nal       - Number of levels in profile
!    pl ( nal) - Profile of Pressure (mb)
!    SfcGeoPt  - Surface geopotential height
!    tl ( nal) - Profile of Temperature (K)
!    ql ( nal) - Profile of Specific Humidity (g/g)
!
! !Output Parameters:
!    zz ( nal) - Vertical profile of Geopotential Height( meters)
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!   avt  - Mean virtual temperature for an atmospheric layer
!   Rd_g - Gas constant for dry air (Rd) corrected for gravity (g)
!          = Rd / g  
!
! !Revision History:
!  Version 3.3  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Added SfcGeo to calling argument list to remove dependence
!                   on Mod. SARB_Var, therefore increasing reusability
!  Version 2.4   March 15, 1999 - SCCR #132
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN)  :: nal
      REAL,    INTENT (IN)  :: pl ( nal)
      REAL,    INTENT (IN)  :: ql ( nal)
      REAL,    INTENT (IN)  :: SfcGeo
      REAL,    INTENT (IN)  :: tl ( nal)
      REAL,    INTENT (OUT) :: zz ( nal)
!
      REAL              :: avt
      REAL              :: h
      INTEGER           :: I
      REAL,   PARAMETER :: Rd_g = 29.3
!
!
! *** Convert the surface geopotential height from meters to kilometers.
!
      zz ( nal) = SfcGeo * 0.001 !! to KM
!
      DO I = nal-1, 1, -1
!
          avt = (  tl ( I+1) * (1.0 + 0.61 * ql ( I+1))                     &
                 + tl ( I)   * (1.0 + 0.61 * ql ( I))   ) * 0.5
!
          h = .001 * Rd_g * avt                              ! SCALE Height KM
!
          zz ( I) = zz ( I+1) + h * LOG (pl ( I+1) / pl ( I))
!
      END DO
!
      END SUBROUTINE GeoPtPro_Calc
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE utrh (nal, zz, pl, tl, ql, p1, p2,                 & ! I
                                                    rhbar)            ! O
!
!******************************************************************************
!
! !F90
! 
! !Name:
!
! !Description:
!   Routine ID -
!
!   Purpose - Calculates the upper tropospheric relative humidity required 
!             by the SARB constrainment algorithm
!
!
! !Input Parameters:
!    nal       - Number of levels in profile
!    zz ( nal) - Profile of Geopotential Height( meters)
!    pl ( nal) - Profile of Pressure (mb)
!    tl ( nal) - Profile of Temperature (K)
!    ql ( nal) - Profile of Specific Humidity (g/g)
!    p1, p2    - Pressure bounds ex ( p1=200,p2=500 )
!
! !Output Parameters:
!    rhbar - Height mean relative humidity between p1:p2
! 
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_meteor - (CERESlib) Sbr. sh_rh
!
! !Revision History:
!  Version 2.4   March 15, 1999 - SCCR #132
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN)  :: nal
      REAL,    INTENT (IN)  :: pl ( nal)
      REAL,    INTENT (IN)  :: p1
      REAL,    INTENT (IN)  :: p2
      REAL,    INTENT (IN)  :: ql ( nal)
      REAL,    INTENT (OUT) :: rhbar
      REAL,    INTENT (IN)  :: tl ( nal) 
      REAL,    INTENT (IN)  :: zz ( nal)
!
      INTEGER :: I
      REAL    :: dz
      REAL    :: rh
      REAL    :: rh1
      REAL    :: rh2
      REAL    :: tdz
!
!
      rhbar=0
      tdz=0
!
      DO I = 1, nal-1
          IF (pl ( I) >= p1 .AND. pl ( I+1) <= p2) THEN
!         *** Include this profile level
!
            dz = zz ( I+1) - zz ( I)
            tdz = tdz + dz
!
            CALL sh_rh (rh1,                                          & ! O
                             tl ( I),   pl ( I),   ql ( I))             ! I
!
            CALL sh_rh (rh2,                                          & ! O
                             tl ( I+1), pl ( I+1), ql ( I+1))           ! I
!
            rh = (rh1 + rh2) * 0.5 * dz
            rhbar = rhbar + rh
!
          END IF
!
      END DO
!
      rhbar = rhbar / tdz
!
!
      END SUBROUTINE utrh
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE UpTropHum
