MODULE grid_values
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !  !F90
  !  Name: grid_1deg
  !
  !  !Description
  !  Routine ID:
  !
  !  Purpose:
  !  This module provides the grid characteristics for the CERES
  !  reference grid.
  !
  !  !Team-Unique Header:
  !
  !  Comments:
  !  1. This module provides the grid_routines and the grid_nest_routines
  !     modules (and ultimately, the reference_grid module) with an
  !     interface to the grid characteristics.
  !  2. The grid pointed to by the reference_grid module can be changed
  !     by simply changing the grid module pointed to in the USE statement
  !     in this module.
  !  3. Currently, there are 2 grid modules available for pointing to.
  !     a. grid_1deg
  !     b. grid_1_25deg
  !
  !  !Revision History:
  !  Revision 1.1  1996/08/12
  !  Joe Stassi, SAIC (j.c.stassi@larc.nasa.gov)
  !  Initial version of code
  !
  !  !end
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !USE grid_1_25deg, only :        &
  USE grid_1deg, only :           &
!  USE grid_05deg, only :           &
       NUM_ZONES,                 &
       NUM_REGIONS,               &
       STARTING_LONGITUDE,        &
       NUM_NONPOLAR_ZONES,        &
       NUM_POLAR_ZONES,           &
       SOUTH_ZONE_NO,             &
       NORTH_ZONE_NO,             &
       REGIONS_PER_ZONE,          &
       MAX_REGIONS_PER_ZONE,      &
       NUM_NESTED_REGIONS,        &
       NESTING_FACTOR,            &
       INTEGER_BOUNDARIES,        &
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
       NUM_NESTED_REGIONS,        &  !*** nesting characteristics
       NESTING_FACTOR,            &
       INTEGER_BOUNDARIES,        &
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

END MODULE grid_values
