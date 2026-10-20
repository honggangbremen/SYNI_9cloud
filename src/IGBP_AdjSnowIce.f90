MODULE IGBP_AdjSnowIce
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   IGBP_AdjSnowIce
!
! !Description:
!   Routine ID -
!
!   Purpose -
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
!    AdjIGBP_Drv 
!    AdjustSphere
!    CreateSphere
!    CryoAdjust_Drv 
!    RmSph_IGBPAntSnow
!    RmSph_IGBPFreshSnow
!    RmSph_IGBPSeaIce
!    RmSph_IGBPSnowIce
!    SnowIceType_Sum 
!
!  Modules Used:
!    FL_IO_Params     - (SARBlib) Used by Sbr. CreateSphere
!    IGBP_Utils       - (SARBlib) Used by Sbr. CreateSphere, RmSph_IGBPAntSnow, 
!                                              RmSph_IGBPFreshSnow, 
!                                              RmSph_IGBPSeaIce, SnowIceType_Sum
!    SARBInput_Params - (SARBlib) Used by Sbr. AdjIGBP_Drv, AdjustSphere,
!                                              CreateSphere, CryoAdjust_Drv, 
!                                              RmSph_IGBPAntSnow, 
!                                              RmSph_IGBPFreshSnow, 
!                                              RmSph_IGBPSeaIce, SnowIceType_Sum
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      USE FL_IO_Params
      USE IGBP_Utils
      USE SARBInput_Params
!
      IMPLICIT NONE
!
      CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE AdjIGBP_Drv (NumSfcTyp, Snow_Ice,                      & ! I
                                         sfc_type, sfc_area)              ! I/O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    AdjIGBP_Drv
!
! !Description:
!    Routine ID - 
!
!    Purpose - Takes IGBP scene type vector and alters it to conform to
!              the Snow/Ice percentage contained on the SSF.
!
!
! !Input Parameters:
!    NumSfcTyp - Number of surface types  
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_type - Array of IGBP/SNOWICE MAP scene type vector (1-20)
!    sfc_area - Array of IGBP/SNOWICE MAP area vector (0-100)
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Params: IN2
!
!  Comments:
!    The IGBP scene vector a primary source of surface albedo information
!    in SARB. This routine "trusts" the snow_ice percentage over the Gridded
!    Snow ice map information given as IGBP (15,19,20)
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
!Case0 = Do Nothing
!Case1 = Alter ( Add or Subtract SNOW/ICE) 
!Case2 = REMOVE  SNOW/ICE
!Case3 = ADD   SNOW/ICE
!
      INTEGER,       INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2), INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2), INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2), INTENT (IN)    :: Snow_Ice
!
      INTEGER (IN2)     :: anc
      INTEGER (IN2)     :: asnow
      INTEGER (IN2)     :: asnow2
      INTEGER (IN2)     :: a15
      INTEGER (IN2)     :: a19
      INTEGER (IN2)     :: a20
      CHARACTER (LEN=2) :: HCASE
      INTEGER           :: I
      INTEGER           :: kbad
      INTEGER           :: kkk
!
!
      asnow  = -9999
      asnow2 = -9999
      HCASE = '0a'
!
!
      kbad = 0
      DO I = 1, NumSfcTyp
          kkk = sfc_type ( I)
          IF (sfc_area ( I) > 100 .OR. sfc_area ( I) < 0) THEN
              kbad = kbad + 1
              sfc_area ( I) = 0
              sfc_type ( I) = Default_INT2_FL
          ELSE IF ( kkk >= 1 .AND. kkk <= 20) THEN
!         *** OK 
! 
              CYCLE
!
          ELSE
!         *** BAD SCENE INFO: Do Nothing
!
              HCASE='0b'
              EXIT
!
          END IF
      END DO

      IF (kbad == NumSfcTyp) THEN
!     *** NO SCENE INFO: Do Nothing
!
          HCASE='0c'
!
      ELSE
!
          CALL SnowIceType_Sum (NumSfcTyp, Snow_Ice,                  & ! I
                                asnow, asnow2, sfc_area, sfc_type,    & ! I/O
                                                            HCASE)      ! O
!
      END IF
!
!
      END SUBROUTINE AdjIGBP_Drv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE SnowIceType_Sum (NumSfcTyp, Snow_Ice,                 & ! I
                                  asnow, asnow2, sfc_area, sfc_type,   & ! I/O
                                                              HCASE)     ! O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    SnowIceType_Sum
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    NumSfcTyp - Number of input surface types
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    asnow    - Percent combined snow and ice surface area coverage
!    asnow2
!    sfc_area - Array containing the percentage of coverage for each of the
!               scene types indicated in the sfc_type array
!    sfc_type - Array containing the most prominent NumSfcTyp scene types 
!               included in the current FOV
!
! !Output Parameters:
!    HCASE
!
! !Team-Unique Header:
!
!  Internal Routines Used:
!    CryoAdjust_Drv
!
!  Modules Used:
!    IGBP_Utils       - (SARBlib) Params: IGBPAntSnow, IGBPFreshSnow, IGBPSeaIce
!    SARBInput_Params - (SARBlib) Params: IN2
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER (IN2),     INTENT (INOUT) :: asnow
      INTEGER (IN2),     INTENT (INOUT) :: asnow2
      CHARACTER (LEN=2), INTENT (OUT)   :: HCASE
      INTEGER,           INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2),     INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2),     INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2),     INTENT (IN)    :: Snow_Ice
!
      INTEGER (IN2)     :: AbSum
      INTEGER (IN2)     :: anc
!      INTEGER (IN2)     :: asnow
!      INTEGER (IN2)     :: asnow2
      INTEGER (IN2)     :: a15
      INTEGER (IN2)     :: a19
      INTEGER (IN2)     :: a20
      INTEGER           :: da
!      CHARACTER (LEN=2) :: HCASE
      INTEGER           :: ig 
      INTEGER           :: i15
      INTEGER           :: i19
      INTEGER           :: i20
!
!
      anc = 0 
      a15 = 0
      a19 = 0 
      a20 = 0 
!
      DO ig = 1, NumSfcTyp
          IF (sfc_type ( ig) == IGBPAntSnow) THEN
              a15 = sfc_area ( ig)
              i15 = ig
          ELSE IF (sfc_type ( ig) == IGBPFreshSnow) THEN
              a19 = sfc_area ( ig)
              i19 = ig
          ELSE IF (sfc_type ( ig) == IGBPSeaIce) THEN
              a20 = sfc_area ( ig)
              i20 = ig
          ELSE
              anc = anc + sfc_area ( ig)
          END IF
      END DO
!
      AbSum = ABS (anc+a15+a19+a20 - 100)
!
      IF ( AbSum > 5 ) THEN
!     *** SUM IGBP AREA error
!
          HCASE='0f'
!        
      ELSE
!
          asnow  = a15 + a19 + a20
          asnow2 = asnow ! result = inital if Case0
!
          CALL CryoAdjust_Drv (anc, a15, a19, a20, NumSfcTyp,        & ! I
                               i15, i19, i20, Snow_Ice,              & ! I
                                      asnow, sfc_area, sfc_type,     & ! I/O
                                                          HCASE)       ! O
!
          IF (SUM (sfc_area) /= 100 ) THEN
!         *** ROUND OFF CORRECTION
!
              da = SUM (sfc_area) - 100
              sfc_area (MAXLOC (sfc_area)) = sfc_area (MAXLOC (sfc_area)) - da
!
          END IF
!
      END IF
!
!
      END SUBROUTINE SnowIceType_Sum
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE CryoAdjust_Drv (anc, a15, a19, a20, NumSfcTyp,       & ! I
                                 i15, i19, i20, Snow_Ice,             & ! I
                                       asnow, sfc_area, sfc_type,     & ! I/O
                                                           HCASE)       ! O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    CryoAdjust_Drv
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    anc       - Percent surface area for non snow and ice surface types
!    a15       - Percent surface area for Antarctic snow IGBP scene type
!    a19       - Percent surface area for fresh snow IGBP scene type
!    a20       - Percent surface area for sea ice IGBP scene type
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!    sfc_type - Array containing the surface types contained in the cuurent
!               FOV
!
! !Output Parameters:
!    HCASE
!
! !Team-Unique Header:
!
!  Internal Routines Used:
!    AdjustSphere
!    CreateSphere
!    RmSph_IGBPAntSnow
!    RmSph_IGBPFreshSnow
!    RmSph_IGBPSeaIce
!    RmSph_IGBPSnowIce
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Params: IN2
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER (IN2),     INTENT (IN)    :: anc
      INTEGER (IN2),     INTENT (INOUT) :: asnow
      INTEGER (IN2),     INTENT (IN)    :: a15
      INTEGER (IN2),     INTENT (IN)    :: a19
      INTEGER (IN2),     INTENT (IN)    :: a20
      CHARACTER (LEN=2), INTENT (OUT)   :: HCASE
      INTEGER,           INTENT (IN)    :: i15
      INTEGER,           INTENT (IN)    :: i19
      INTEGER,           INTENT (IN)    :: i20
      INTEGER,           INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2),     INTENT (IN)    :: Snow_Ice
      INTEGER (IN2),     INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2),     INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
!
!
       IF ((a15 > 0 .or. a19 > 0 .or. a20 > 0) .and. anc > 0) THEN
!      *** Case1 SNOW and/or ICE  & OTHER Types Present'
!
           HCASE='1a'
           CALL AdjustSphere (asnow, a15, a19, a20, i15, i19, i20,     & ! I
                              NumSfcTyp, Snow_Ice, anc,                & ! I
                                                    sfc_area)            ! I/O
!
       ELSE IF (a15 > 0 .and. a19 == 0 .and. a20 == 0 .and. anc == 0) THEN
!      *** Case2a ONLY SNOW IGBP 15 PRESENT
!
           HCASE='2a'
           CALL RmSph_IGBPAntSnow (asnow, a15, NumSfcTyp, Snow_Ice,    & ! I
                                                 sfc_area, sfc_type)     ! I/O
!
!
       ELSE IF (a19 > 0 .and. a15 == 0 .and. a20 == 0 .and. anc == 0) THEN
!      *** Case2b ONLY SNOW IGBP 19 PRESENT
!
           HCASE='2b'
           CALL RmSph_IGBPFreshSnow (asnow, a19, NumSfcTyp, Snow_Ice,  & ! I
                                                   sfc_area, sfc_type)   ! I/O
!
       ELSE IF (a20 > 0 .and. a15 == 0 .and. a19 == 0 .and. anc == 0) THEN
!      *** Case2c ONLY SEA ICE IGBP 20 PRESENT
!
           HCASE='2c'
           CALL RmSph_IGBPSeaIce (asnow, a20, NumSfcTyp, Snow_Ice,    & ! I
                                               sfc_area, sfc_type)      ! I/O
!
      ELSE IF (a15 > 0 .or. a19 > 0 .or. a20 > 0 .and. anc == 0) THEN
!     *** Case2d ONLY SNOW/ICE IGBP 15 or 19 or  PRESENT
!
          HCASE='2d'
          CALL RmSph_IGBPSnowIce (asnow, a15, a19, a20,              & ! I
                                  NumSfcTyp, Snow_Ice,               & ! I
                                               sfc_area, sfc_type)     ! I/O
!
      ELSE IF ((a15 == 0 .AND. a19 == 0 .AND. a20 == 0) .AND. anc > 0) THEN
!
          CALL CreateSphere (NumSfcTyp, Snow_Ice,                    & ! I
                                sfc_area, sfc_type,                  & ! I/O
                                                    HCASE)             ! O
!
       ELSE
!      *** Type0e :NOT HANDLED 
!
           HCASE='0e'
!
       END IF
!
!
      END SUBROUTINE CryoAdjust_Drv
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE AdjustSphere (asnow, a15, a19, a20, i15, i19, i20,     & ! I
                               NumSfcTyp, Snow_Ice, anc,                & ! I
                                                     sfc_area)            ! I/O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    AdjustSphere
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    anc       - Percent surface area for non snow and ice surface types
!    asnow     - Percent combined snow and ice surface area coverage
!    a15       - Percent surface area for Antarctic snow IGBP scene type
!    a19       - Percent surface area for fresh snow IGBP scene type
!    a20       - Percent surface area for sea ice IGBP scene type
!    i15       - Index into sfc_area array for the Antarctic snow IGBP scene 
!                type
!    i19       - Index into sfc_area array for the fresh snow IGBP scene type
!    i20       - Index into sfc_area array for the sea ice IGBP scene type
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Params: IN2
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER (IN2), INTENT (IN)    :: anc
      INTEGER (IN2), INTENT (IN)    :: asnow
      INTEGER (IN2), INTENT (IN)    :: a15
      INTEGER (IN2), INTENT (IN)    :: a19
      INTEGER (IN2), INTENT (IN)    :: a20
      INTEGER,       INTENT (IN)    :: i15
      INTEGER,       INTENT (IN)    :: i19
      INTEGER,       INTENT (IN)    :: i20
      INTEGER,       INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2), INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2), INTENT (IN)    :: Snow_Ice
!
      REAL    :: a15r
      REAL    :: a19r
      REAL    :: a20r
      INTEGER :: da
!
!
      a15r = REAL (a15) / (REAL (asnow) * 1.0)
      a19r = REAL (a19) / (REAL (asnow) * 1.0)
      a20r = REAL (a20) / (REAL (asnow) * 1.0)

      da = Snow_Ice - asnow
!       
      WHERE (sfc_area ( 1:NumSfcTyp) > 0 )                                     &
        sfc_area (1:NumSfcTyp) = NINT (REAL (sfc_area ( 1:NumSfcTyp))          &
                                        - REAL (da * sfc_area ( 1:NumSfcTyp))  &
                                         / (REAL (anc) * 1.0))
!
      IF (a15r > 0) sfc_area ( i15) = a15r * snow_ice
      IF (a19r > 0) sfc_area ( i19) = a19r * snow_ice
      IF (a20r > 0) sfc_area ( i20) = a20r * snow_ice
!
!
      END SUBROUTINE AdjustSphere
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE RmSph_IGBPAntSnow (asnow, a15, NumSfcTyp, Snow_Ice,    & ! I
                                                 sfc_area, sfc_type)      ! I/O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    RmSph_IGBPAntSnow
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    asnow     - Percent combined snow and ice surface area coverage
!    a15       - Percent surface area for Antarctic snow IGBP scene type
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!    sfc_type - Array containing the surface types contained in the cuurent
!               FOV
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Params: IN2
!    IGBP_Utils       - (SARBlib) Params: IGBPAntSnow, IGBPTundra 
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER (IN2), INTENT (IN)    :: asnow
      INTEGER (IN2), INTENT (IN)    :: a15
      INTEGER,       INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2), INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2), INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2), INTENT (IN)    :: Snow_Ice
!
      INTEGER :: da
!
!
      da = snow_ice - a15
      sfc_area ( 1) = snow_ice
      sfc_area ( 2) = 100 - snow_ice
      sfc_type ( 1) = IGBPAntSnow
      sfc_type ( 2) = IGBPTundra ! IMPOSE Tundra ? 
!
!
      END SUBROUTINE RmSph_IGBPAntSnow
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE RmSph_IGBPFreshSnow (asnow, a19, NumSfcTyp, Snow_Ice,   & ! I
                                                   sfc_area, sfc_type)     ! I/O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    RmSph_IGBPFreshSnow
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    asnow      - Percent combined snow and ice surface area coverage
!    a19        - Percent surface area for fresh snow IGBP scene type
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice   - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!    sfc_type - Array containing the surface types contained in the cuurent
!               FOV
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    IGBP_Utils       - (SARBlib) Params: IGBPFreshSnow, IGBPTundra 
!    SARBInput_Params - (SARBlib) Params: IN2
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER (IN2), INTENT (IN)    :: asnow
      INTEGER (IN2), INTENT (IN)    :: a19
      INTEGER,       INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2), INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2), INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2), INTENT (IN)    :: Snow_Ice
!
      INTEGER :: da
!
!
      da = snow_ice - a19
      sfc_area ( 1) = snow_ice
      sfc_area ( 2) = 100 - snow_ice
      sfc_type ( 1) = IGBPFreshSnow
      sfc_type ( 2) = IGBPTundra ! IMPOSE Tundra ?
!
!
      END SUBROUTINE RmSph_IGBPFreshSnow
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE RmSph_IGBPSeaIce (asnow, a20, NumSfcTyp, Snow_Ice,    & ! I
                                                sfc_area, sfc_type)      ! I/O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   RmSph_IGBPSeaIce
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    asnow     - Percent combined snow and ice surface area coverage
!    a19       - Percent surface area for fresh snow IGBP scene type
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!    sfc_type - Array containing the surface types contained in the cuurent
!               FOV
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    IGBP_Utils       - (SARBlib) Params: IGBPOcean, IGBPSeaIce
!    SARBInput_Params - (SARBlib) Params: IN2
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER (IN2), INTENT (IN)    :: asnow
      INTEGER (IN2), INTENT (IN)    :: a20
      INTEGER,       INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2), INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2), INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2), INTENT (IN)    :: Snow_Ice
!
      INTEGER :: da
!
!
!          PRINT*,'Case2c ONLY SEA ICE IGBP 20 PRESENT'
!
      da = snow_ice - a20
!
      sfc_area ( 1) = Snow_Ice
      sfc_area ( 2) = 100 - Snow_Ice
      sfc_type ( 1) = IGBPSeaIce
      sfc_type ( 2) = IGBPOcean ! IMPOSE Ocean
!
!
      END SUBROUTINE RmSph_IGBPSeaIce
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE RmSph_IGBPSnowIce (asnow, a15, a19, a20,               & ! I
                                    NumSfcTyp, Snow_Ice,                & ! I
                                                sfc_area, sfc_type)       ! I/O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    RmSph_IGBPSnowIce
!
! !Description:
!   Routine ID -
!
!   Purpose -
!
!
! !Input Parameters:
!    asnow     - Percent combined snow and ice surface area coverage
!    a15       - Percent surface area for Antarctic snow IGBP scene type
!    a19       - Percent surface area for fresh snow IGBP scene type
!    a20       - Percent surface area for sea ice IGBP scene type
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice  - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!    sfc_type - Array containing the surface types contained in the cuurent
!               FOV
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    SARBInput_Params - (SARBlib) Params: IN2
!    IGBP_Utils       - (SARBlib) Params: IGBPFreshSnow, IGBPOcean, IGBPSeaIce,
!                                         IGBPTundra
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!         PRINT*,'Case2d ONLY SNOW/ICE IGBP 15 or 19 or  PRESENT'
!
      INTEGER (IN2), INTENT (IN)    :: asnow
      INTEGER (IN2), INTENT (IN)    :: a15
      INTEGER (IN2), INTENT (IN)    :: a19
      INTEGER (IN2), INTENT (IN)    :: a20
      INTEGER,       INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2), INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2), INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2), INTENT (IN)    :: Snow_Ice
!
      INTEGER :: da
      INTEGER :: i1
      INTEGER :: i2
!
!
      da = snow_ice - asnow
!
      IF (a20 > (a19+a15) ) THEN
!     *** SEAICE > SNOW ie OVER OCEAN
!
          i1 = IGBPSeaIce
          i2 = IGBPOcean
!
      ELSE IF (a20 < (a19+a15) ) THEN
!     *** SEAICE < SNOW ie OVER LAND
!
          i1 = IGBPFreshSnow
          i2 = IGBPTundra
!
      END IF
!
      sfc_area ( 1) = snow_ice
      sfc_area ( 2) = 100 - snow_ice
      sfc_type ( 1) = i1
      sfc_type ( 2) = i2 
!
!
      END SUBROUTINE RmSph_IGBPSnowIce
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE CreateSphere (NumSfcTyp, Snow_Ice,                   & ! I
                                 sfc_area, sfc_type,                  & ! I/O
                                                     HCASE)             ! O
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   CreateSphere 
!
! !Description:
!   Routine ID -
!
!   Purpose - Creates 
!
!
! !Input Parameters:
!    NumSfcTyp - Number of possible surface types
!    Snow_Ice   - Cloud working group snow/ice area fraction
!
!  Input/Output Parameters:
!    sfc_area - Array containing the percent coverage for each corresponding 
!               surface type indicated by the sfc_type array
!    sfc_type - Array containing the surface types contained in the cuurent
!               FOV
!
! !Output Parameters:
!    HCASE
!
! !Team-Unique Header:
!
!  Modules Used:
!    FL_IO_Params     - (SARBlib) Params: Default_INT2_FL
!    IGBP_Utils       - (SARBlib) Params: IGPBFreshSnow, IGBPOcean, IGBPSeaIce
!    SARBInput_Params - (SARBlib) Params: IN2
!
! !Revision History:
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
! TYPE 3: CREATE IGBP CRYOSPHERE
!
      CHARACTER (LEN=2), INTENT (OUT)   :: HCASE
      INTEGER,           INTENT (IN)    :: NumSfcTyp
      INTEGER (IN2),     INTENT (INOUT) :: sfc_type ( NumSfcTyp) 
      INTEGER (IN2),     INTENT (INOUT) :: sfc_area ( NumSfcTyp)
      INTEGER (IN2),     INTENT (IN)    :: Snow_Ice
!
      INTEGER :: da
      INTEGER :: fpsid
      INTEGER :: intype
      INTEGER :: psid
!
!
      IF (Snow_Ice /= 0) THEN
!     *** Case3 Cldmask snow GT 0 but no IGBP 15 or 19 or 20

          psid  = MAXVAL (sfc_type (MAXLOC (sfc_area))) 
          fpsid = MAXVAL (sfc_area (MAXLOC (sfc_area))) 
          da = Snow_Ice - fpsid
!          sfc_area ( psid) = Snow_Ice

          IF (psid == IGBPOcean) THEN
!         *** Predominately Over OCEAN CREATE SEAICE 
!
              intype = IGBPSeaIce 
              HCASE = '3a'
!
          ELSE
!         *** Predominately Over Land CREATE SNOW 
!
              intype = IGBPFreshSnow
              HCASE = '3b'
!
          END IF
!
!
          sfc_area ( 1) = Snow_Ice
          sfc_area ( 2) = 100 - Snow_Ice
          sfc_type ( 1) = intype
          sfc_type ( 2) = psid ! OCEAN 
          sfc_type ( 3:NumSfcTyp) = Default_INT2_FL
          sfc_area ( 3:NumSfcTyp) = 0
!
      ELSE 
!     *** Type0d Cldmask snow EQ 0 AND NO IGBP 15 or 19 or 20 : Do Nothing
!
          HCASE='0d'
!
      END IF
!
      END SUBROUTINE CreateSphere
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
END MODULE IGBP_AdjSnowIce
