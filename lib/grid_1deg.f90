MODULE grid_1deg
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !  !F90
  !  Name: grid_1deg
  !
  !  !Description
  !  Routine ID:
  !
  !  Purpose:
  !  This module provides the grid characteristics for a 1 degree,
  !  equal-angle grid with the following nesting scheme.
  !
  !        Latitude       grid size    number of regions:
  !          0 - 45          1 x 1       360 * 90 = 32400
  !         45 - 70          2 x 1       180 * 50 =  9000
  !         70 - 80          4 x 1        90 * 20 =  1800
  !         80 - 89          8 x 1        45 * 18 =   810
  !         89 - 90        360 x 1                      2
  !                                                 _____
  !                                                 44012
  !
  !
  !  !Revision History:
  !  Revision 1.1  1996/08/12
  !  Joe Stassi, SAIC (j.c.stassi@larc.nasa.gov)
  !  Initial version of code
  !
  !  !end
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: &
       NUM_ZONES,                 &  !*** grid characteristics
       NUM_REGIONS,               &
       STARTING_LONGITUDE,        &
       NUM_NONPOLAR_ZONES,        &
       NUM_POLAR_ZONES,           &
       SOUTH_ZONE_NO,             &
       NORTH_ZONE_NO,             &
       REGIONS_PER_ZONE,          &
       MAX_REGIONS_PER_ZONE,      &
       INTEGER_BOUNDARIES,        &

       NUM_NESTED_REGIONS,        &  !*** nesting characteristics
       NESTING_FACTOR,            &
       LatOff_CERESNESTED,        &  
       LongOff_CERESNESTED,       &  
       NumBands_CERESNESTED,      & 
       NumReg_Global_CERESNESTED, &
       RegHgt_CERESNESTED,        &
       CLat_CERESNESTED,          &
       CLong_CERESNESTED,         &
       BegReg_CERESNESTED,        &              
       EndReg_CERESNESTED,        &  
       NumReg_CERESNESTED,        &
       zone

  !*----------------------*
  !* grid characteristics *
  !*----------------------*
  INTEGER, PARAMETER :: NUM_ZONES = 180
  INTEGER, PARAMETER :: NUM_REGIONS = 64800
  INTEGER, PARAMETER :: STARTING_LONGITUDE = 180
  INTEGER, PARAMETER :: NUM_POLAR_ZONES = 46
  INTEGER, PARAMETER :: NUM_NONPOLAR_ZONES = 134
  INTEGER, PARAMETER :: SOUTH_ZONE_NO = 180
  INTEGER, PARAMETER :: NORTH_ZONE_NO = 1
  INTEGER, PARAMETER :: REGIONS_PER_ZONE(NUM_ZONES) = 360
  INTEGER, PARAMETER :: MAX_REGIONS_PER_ZONE = 360
  LOGICAL, PARAMETER :: INTEGER_BOUNDARIES = .TRUE.

  !*-------------------------*
  !* nesting characteristics *
  !*-------------------------*
  INTEGER :: zone
  INTEGER, PARAMETER :: NUM_NESTED_REGIONS = 44012
  INTEGER, PARAMETER :: NESTING_FACTOR(NUM_ZONES) = (/ &
       ( 360, zone =   1,   1 ),  &
       (   8, zone =   2,  10 ),  &
       (   4, zone =  11,  20 ),  &
       (   2, zone =  21,  45 ),  &
       (   1, zone =  46, 135 ),  &
       (   2, zone = 136, 160 ),  &
       (   4, zone = 161, 170 ),  &
       (   8, zone = 171, 179 ),  &
       ( 360, zone = 180, 180 ) /)

!
! *** CERES nested grid parameters  ( 1 x 1 equal area nested at poles)
!
      REAL,    PARAMETER :: LatOff_CERESNESTED   = 0.0
      REAL,    PARAMETER :: LongOff_CERESNESTED  = 0.0
      INTEGER, PARAMETER :: NumBands_CERESNESTED = 180
      INTEGER, PARAMETER :: NumReg_Global_CERESNESTED = 44012

      REAL    :: RegHgt_CERESNESTED( NumBands_CERESNESTED) = 1.0
      REAL    :: CLat_CERESNESTED ( NumBands_CERESNESTED) = 0.0
      REAL    :: CLong_CERESNESTED ( NumReg_Global_CERESNESTED) = 0.0

      INTEGER, SAVE :: BegReg_CERESNESTED( NumBands_CERESNESTED) =  0              
      INTEGER, SAVE :: EndReg_CERESNESTED( NumBands_CERESNESTED) =  0  

!      INTEGER :: zone
      INTEGER :: NumReg_CERESNESTED( NumBands_CERESNESTED) = (/   &
                                    (   1, zone =   1,   1 ),  &
                                    (  45, zone =   2,  10 ),  &
                                    (  90, zone =  11,  20 ),  &
                                    ( 180, zone =  21,  45 ),  &
                                    ( 360, zone =  46, 135 ),  &
                                    ( 180, zone = 136, 160 ),  &
                                    (  90, zone = 161, 170 ),  &
                                    (  45, zone = 171, 179 ),  &
                                    (   1, zone = 180, 180 )    /)





END MODULE grid_1deg
