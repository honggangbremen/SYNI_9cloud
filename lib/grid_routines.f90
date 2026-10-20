MODULE grid_routines
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !  !F90
  !  Name: grid_routines
  !
  !  !Description
  !  Routine ID:
  !
  !  Purpose:
  !  This module provides routines for querying the CERES reference grid.
  !
  !  !Team-Unique Header:
  !
  !  Comments:
  !  The routines in this module were originally in the reference_grid
  !  module.
  !
  !  !Revision History:
  !  Revision 1.3  1996/08/12
  !  Joe Stassi, SAIC (j.c.stassi@larc.nasa.gov)
  !  Initial version of code
  !
  !  !end
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  USE f90_kind, only : int4, real4, real8
  USE grid_values, only : NUM_ZONES, NUM_REGIONS, STARTING_LONGITUDE, &
       SOUTH_ZONE_NO, NORTH_ZONE_NO, REGIONS_PER_ZONE
  IMPLICIT NONE

  INCLUDE 'PGS_REFGRID_26410.f'

  PRIVATE
  PUBLIC :: &
       centroid_distance,         &
       first_region,              &
       get_borders,               &
       get_centroid,              &
       get_south_north_dp,        &
       is_polar,                  &
       last_region,               &
       number_of_regions,         &
       number_of_zones,           &
       region_area,               &
       region_number,             &
       region_width,              &
       region_width_dp,           &
       zone_center_colat,         &
       zone_center_lat,           &
       zone_height,               &
       zone_number,               &
       zone_number1,              &
       zone_number2

  !*------------------------------*
  !* module variable declarations *
  !*------------------------------*
  INTEGER (int4) :: first_region_in_zone(NUM_ZONES)
  REAL (real8)   :: region_width_in_zone(NUM_ZONES)
  LOGICAL :: first_func_ready = .false.
  LOGICAL :: width_func_ready = .false.
  REAL(real4) :: centroid_colat(NUM_ZONES) = -1.0

  !*-------------------------*
  !* module interface blocks *
  !*-------------------------*
  INTERFACE zone_number
     MODULE PROCEDURE zone_number1, zone_number2
  END INTERFACE

CONTAINS

  !**********************-------------------**********************
  !**********************  PUBLIC ROUTINES  **********************
  !**********************-------------------**********************

  FUNCTION centroid_distance(region_no, colatitude, longitude) &
       result (angular_distance_deg)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: centroid_distance
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine calculates and returns the approximate earth central
    !  angle distance between a given colatitude/longitude location and the
    !  centroid of a specified region.
    !
    !  !Input Parameters:
    !  region_no  : region number
    !  colatitude : colatitude
    !  longitude  : longitude
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Team-Unique Header:
    !
    !  Comments:
    !  1. This function approximates distance over small surface areas (which
    !     approximate a flat surface) and should not be used for distances
    !     over a couple degrees.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE ceres_constants, only : DEGREES_TO_RADIANS
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: region_no
    REAL (real4), INTENT(in) :: colatitude, longitude
    REAL (real4) :: angular_distance_deg
    REAL (real4) :: longitude_mod
    REAL (real4) :: centroid_colatitude, centroid_longitude
    REAL (real8) :: long_diff, colat_diff
    REAL (real8) :: colatitude_rad
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::centroid_distance"

    !*** range check the region number and colatitude
    CALL check(subr_name, regnum_val=region_no, colat_val=colatitude)

    !*** find region centroid location
    CALL get_centroid(region_no, centroid_colatitude, centroid_longitude)

    !*** alternate forms of colatitude and longitude
    colatitude_rad = colatitude * DEGREES_TO_RADIANS
    longitude_mod = MODULO(longitude, 360.0)

    !*** find degree differences in colatitudes and longitudes
    colat_diff = ABS(colatitude - centroid_colatitude)
    long_diff = ABS(longitude_mod - centroid_longitude)
    IF (long_diff > 180.0) long_diff = 360.0 - long_diff

    !*** here's a distance formula application
    angular_distance_deg = &
         SQRT( colat_diff**2 + (long_diff*SIN(colatitude_rad))**2 )

    !----------------------------------------------------------------------
    !--!*** convert angular measures from degrees to radians
    !--colatitude_rad = colatitude * degrees_to_radians
    !--centroid_colatitude_rad = centroid_colatitude * degrees_to_radians
    !--delta_longitude_rad = (longitude - centroid_longitude) &
    !--                      * degrees_to_radians
    !--
    !--!*** calculate distance to centroid
    !--term1 = COS(colatitude_rad) * COS(centroid_colatitude_rad)
    !--term2 = SIN(colatitude_rad) * COS(centroid_colatitude_rad)
    !--term3 = COS(delta_longitude_rad)
    !--
    !--angular_distance_deg = ACOS(term1 + term2*term3)
    !----------------------------------------------------------------------

    RETURN
  END FUNCTION centroid_distance

  FUNCTION first_region(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: first_region
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the reference grid number of the first region
    !  within a specified latitudinal zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    INTEGER (int4) :: first_region
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::first_region"

    !*** be sure first region array has been initialized
    IF (.NOT.first_func_ready) THEN
       CALL initialize_first_regions
    END IF

    !*** range check zone number
    CALL check(subr_name, zonum_val=zone_no)

    !*** return first region
    first_region = first_region_in_zone(zone_no)

    RETURN
  END FUNCTION first_region

  SUBROUTINE get_borders( &
       region_no, colat_south, colat_north, long_west, long_east)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: get_borders
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine calculates the colatitude and longitude borders of
    !  a specified region.
    !
    !  !Input Parameters:
    !  region_no : region number
    !
    !  !Output Parameters:
    !  colat_south : colatitude of region's southern boundary
    !  colat_north : colatitude of region's northern boundary
    !  long_west   : longitude of region's western boundary
    !  long_east   : longitude of region's eastern boundary
    !
    !  !Team-Unique Header:
    !
    !  Implementation/Unique Features:
    !  See IMPLEMENTATION NOTE contained within the body of the code.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: region_no
    REAL (real4), INTENT(out) :: colat_south, colat_north
    REAL (real4), INTENT(out) :: long_west, long_east
    REAL (real8) :: south_dp, north_dp, width_dp, west_border_dp
    INTEGER (int4) :: zone_no
    CHARACTER(LEN=*), PARAMETER :: &
         subr_name = 'grid_routines::get_borders'

    !*** range check region number
    CALL check(subr_name, regnum_val=region_no)

    !*** calculate the north/south borders of zone
    zone_no = zone_number(region_no)
    CALL get_south_north_dp(zone_no, south_dp, north_dp)
    colat_north = north_dp
    colat_south = south_dp

    !*** calculate the east/west borders
    width_dp = region_width_dp(zone_no)
    west_border_dp = STARTING_LONGITUDE + &
         (region_no - first_region(zone_no))*width_dp

    !*---------------------------------------------------------------
    !* IMPLEMENTATION NOTE:  The pattern of MODULO use below was done
    !* because of precision problems.  Please be very careful about
    !* changing these lines.
    !*---------------------------------------------------------------
    long_west = MODULO(west_border_dp, 360.0_real8)
    long_east = west_border_dp + width_dp
    long_east = MODULO(long_east, 360.0)

    RETURN
  END SUBROUTINE get_borders

  SUBROUTINE get_centroid(region_no, colatitude, longitude)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: get_centroid
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine returns the colatitude/longitude location for the
    !  centroid of a specified region.
    !
    !  !Input Parameters:
    !  region_no : input region number
    !
    !  !Output Parameters:
    !  colatitude : colatitude (degrees) of region centroid
    !  longitude  : longitude (degrees) of region centroid
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: region_no
    REAL (real4), INTENT(out) :: colatitude, longitude
    REAL (real8) :: width_dp
    INTEGER (int4) :: zone_no
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::get_centroid"

    !*** range check region number
    CALL check(subr_name, regnum_val=region_no)

    !*** colatitude of centroid
    zone_no = zone_number(region_no)
    IF (centroid_colat(zone_no) == -1.0) THEN
       CALL calc_centroid_colat
    END IF
    colatitude = centroid_colat(zone_no)

    !*** longitude of centroid
    width_dp = region_width_dp(zone_no)

    longitude = STARTING_LONGITUDE +                       &
         (region_no - first_region(zone_no))*width_dp +    &
         0.5_real8*width_dp

    longitude = MODULO(longitude, 360.0)

    RETURN
  END SUBROUTINE get_centroid

  SUBROUTINE get_south_north_dp(zone_no, south_dp, north_dp)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: get_south_north_dp
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine returns (in double precision) the south and north
    !  colatitude positions for all regions in a specified zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number for which to find the south and north borders
    !
    !  !Output Parameters:
    !  south_dp : (double precision) southern colatitude of regions in
    !             specified zone
    !  north_dp : (double precision) northern colatitude of regions in
    !             specified zone
    !
    !  !Team-Unique Header:
    !
    !  Implementation/Unique Features:
    !  This subroutine uses the SOUTH_ZONE_NO and NORTH_ZONE_NO PARAMETERS
    !  to determine whether the grid is south-to-north or north-to-south.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER, INTENT(in) :: zone_no
    REAL (real8), INTENT(out) :: north_dp, south_dp

    IF (SOUTH_ZONE_NO < NORTH_ZONE_NO) THEN
       north_dp = 180.0_real8 - REAL(zone_no,real8)*zone_height()
       south_dp = north_dp + zone_height()
    ELSE
       north_dp = (REAL(zone_no,real8) - 1.0_real8) * zone_height()
       south_dp = north_dp + zone_height()
    END IF

    RETURN
  END SUBROUTINE get_south_north_dp

  FUNCTION is_polar(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: is_polar
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns .true. if a zone is contained within one of the
    !  polar regions (with 23.45 degrees of either pole) and .false. if it
    !  is not.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Team-Unique Header:
    !
    !  Comments:
    !  1. This function is implemented to return .true. only if the center of
    !     the zone is contained within the arctic (or antarctic) circle.
    !  2. Because of symmetry of arctic and antarctic about the equator,
    !     it doesn't matter whether this a north -> south grid or a
    !     south -> north grid.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    LOGICAL :: is_polar

    IF ((zone_center_lat(zone_no) < -66.55) .or. &
         (zone_center_lat(zone_no) > 66.55)) THEN
       is_polar = .true.
    ELSE
       is_polar = .false.
    END IF

    RETURN
  END FUNCTION is_polar

  FUNCTION last_region(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: last_region
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine returns the reference grid region number of the last
    !  region within a specified latitudinal zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    INTEGER (int4) :: last_region
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::last_region"

    !*** range check zone number
    CALL check(subr_name, zonum_val=zone_no)

    IF (zone_no == NUM_ZONES) THEN
       last_region = NUM_REGIONS
    ELSE
       last_region = first_region(zone_no+1) - 1
    END IF

    RETURN
  END FUNCTION last_region

  FUNCTION number_of_regions(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: number_of_regions
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the number of regions within a specified
    !  latitudinal zone.  If no zone number is specified, it returns the
    !  total number of regions within the entire reference grid.
    !
    !  !Input Parameters:
    !  zone_no (optional) : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in), OPTIONAL :: zone_no
    INTEGER (int4) :: number_of_regions
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::number_of_regions"

    IF (PRESENT(zone_no)) THEN
       CALL check(subr_name, zonum_val=zone_no)
       number_of_regions = REGIONS_PER_ZONE(zone_no)
    ELSE
       number_of_regions = NUM_REGIONS
    END IF

    RETURN
  END FUNCTION number_of_regions

  FUNCTION number_of_zones()
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: number_of_zones
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the number of latitudinal zones in the
    !  reference grid.
    !
    !  !Input Parameters:
    !  not applicable
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4) :: number_of_zones

    number_of_zones = NUM_ZONES

    RETURN
  END FUNCTION number_of_zones

  FUNCTION region_area(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: region_area
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the approximate area (km**2) of regions within
    !  a specified latitudinal zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE ceres_constants, only : DEGREES_TO_RADIANS, EARTH_CIRCUM_MEAN
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    REAL :: region_area
    REAL (real8) :: zone_center_colat_rad
    REAL (real8) :: km_per_colatdeg = EARTH_CIRCUM_MEAN / 360.0
    REAL (real8) :: km_per_longdeg
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::region_area"

    !*** range check zone number
    CALL check(subr_name, zonum_val=zone_no)

    !*** calculate the km per longitude at the zone center colatitude
    zone_center_colat_rad = zone_center_colat(zone_no) * DEGREES_TO_RADIANS
    km_per_longdeg = km_per_colatdeg * SIN(zone_center_colat_rad)

    !*** calculate area
    region_area = region_width_dp(zone_no) * zone_height() * &
         km_per_colatdeg * km_per_longdeg

    RETURN
  END FUNCTION region_area

  FUNCTION region_number(colatitude, longitude)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: region_number
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the reference grid region number for a given
    !  colatitude/longitude location.
    !
    !  !Input Parameters:
    !  colatitude : colatitude location (degrees)
    !  longitude  : longitude location (degrees)
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Team-Unique Header:
    !
    !  Implementation/Unique Features:
    !  See IMPLEMENTATION NOTEs contained within the body of the code.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE grid_values, ONLY : INTEGER_BOUNDARIES
    IMPLICIT NONE
    REAL (real4), INTENT(in) :: colatitude, longitude

    INTEGER (int4) :: region_number
    INTEGER (int4) :: zone_no
    INTEGER :: region_no_delta
    REAL (real4) :: adj_longitude
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::region_number"

    !*** be sure that first_region array is initialized
    IF (.NOT. first_func_ready) THEN
       CALL initialize_first_regions
    END IF

    !*** range check colatitude
    CALL check(subr_name, colat_val=colatitude)

    !*** calculate the zone number
    !*-------------------------------------------------------------------*
    !*  IMPLEMENTATION NOTE:  If INTEGER_BOUNDARIES is .TRUE., then all
    !*  region borders fall along integer colatitude and longitude lines.
    !*  In this case, move the adj_longitude to the nearest half-degree
    !*  mark for computational purposes only.
    !*-------------------------------------------------------------------*
    IF (INTEGER_BOUNDARIES) THEN
       adj_longitude = FLOOR(longitude) + 0.5 + STARTING_LONGITUDE
    ELSE
       adj_longitude = longitude + STARTING_LONGITUDE
       adj_longitude = adj_longitude + SPACING(adj_longitude)
    END IF

    !*** calculate region number delta
    !*--------------------------------------------------------------*
    !*  IMPLEMENTATION NOTE:  Because of precision considerations,
    !*  the MOD function is called for the region_no_delta rather
    !*  than for the adj_longitude variable.  Trust me.
    !*--------------------------------------------------------------*
    zone_no = zone_number(colatitude, longitude)
    region_no_delta = INT( adj_longitude/region_width(zone_no) )
    region_no_delta = MOD(region_no_delta, number_of_regions(zone_no))

    !*** calculate region number
    region_number = first_region(zone_no)  + region_no_delta

    RETURN
  END FUNCTION region_number

  FUNCTION region_width(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: region_width
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the angular width of regions within a specified
    !  latitudinal zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    REAL (real4) :: region_width
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::region_width"

    !*** be sure that region_width array is initialized
    IF (.NOT. width_func_ready) THEN
       CALL initialize_region_widths
    END IF

    !*** range check zone number
    CALL check(subr_name, zonum_val=zone_no)

    region_width = region_width_in_zone(zone_no)

    RETURN
  END FUNCTION region_width

  FUNCTION region_width_dp(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: region_width_dp
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the angular width of regions within a specified
    !  latitudinal zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    REAL (real8) :: region_width_dp
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::region_width_dp"

    !*** be sure that region_width array is initialized
    IF (.NOT. width_func_ready) THEN
       CALL initialize_region_widths
    END IF

    !*** range check zone number
    CALL check(subr_name, zonum_val=zone_no)

    region_width_dp = region_width_in_zone(zone_no)

    RETURN
  END FUNCTION region_width_dp

  FUNCTION zone_height()
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: zone_height
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the angular height of the latitudinal zones in
    !  the reference grid.
    !
    !  !Input Parameters:
    !  not applicable
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    REAL (real8) :: zone_height

    zone_height = 180.0_real8 / NUM_ZONES

    RETURN
  END FUNCTION zone_height

  FUNCTION zone_center_colat(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: zone_center_colat
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the center colatitude of a zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Revision History:
    !  Revision 1.1  1997/01/07
    !  Joe Stassi, SAIC (j.c.stassi@larc.nasa.gov)
    !  Initial version of code
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    REAL (real8) :: zone_center_colat
    CHARACTER (LEN=*), PARAMETER :: &
         func_name = "grid_routines::zone_center_colat"

    !*** range check zone number
    CALL check(func_name, zonum_val=zone_no)

    !*** get center colatituded of the zone
    zone_center_colat = (REAL(zone_no,real8) - 0.5_real8) * zone_height()
    IF (SOUTH_ZONE_NO == 1) THEN
       zone_center_colat = 180.0_real8 - zone_center_colat
    END IF

    RETURN
  END FUNCTION zone_center_colat

  FUNCTION zone_center_lat(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: zone_center_lat
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the center latitude of a zone.
    !
    !  !Input Parameters:
    !  zone_no : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Revision History:
    !  Revision 1.1  1997/01/07
    !  Joe Stassi, SAIC (j.c.stassi@larc.nasa.gov)
    !  Initial version of code
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    REAL (real8) :: zone_center_lat
    CHARACTER (LEN=*), PARAMETER :: &
         func_name = "grid_routines::zone_center_lat"

    !*** range check zone number
    CALL check(func_name, zonum_val=zone_no)

    !*** get center latituded of the zone
    zone_center_lat = 90.0_real8 - zone_center_colat(zone_no)

    RETURN
  END FUNCTION zone_center_lat

  FUNCTION zone_number1(region_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: zone_number1
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the reference grid zone number for a specified
    !  region.
    !
    !  !Input Parameters:
    !  region_no : region number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Team-Unique Header:
    !
    !  Comments:
    !  1. This routine is part of the generic zone number routine.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: region_no
    INTEGER (int4) :: zone_number1, zone_no
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = 'grid_routines::zone_number1'

    !*** range check region number
    CALL check(subr_name, regnum_val=region_no)

    !*** calculate zone number
    DO zone_no = 1, NUM_ZONES
       IF (region_no >= first_region(zone_no)) THEN
          zone_number1 = zone_no
       ELSE
          EXIT
       END IF
    END DO

    RETURN
  END FUNCTION zone_number1

  FUNCTION zone_number2(colatitude,longitude)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: zone_number2
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the reference grid zone number for a specified
    !  colatitude/longitude location.
    !
    !  !Input Parameters:
    !  colatitude : colatitude location (degrees)
    !  longitude  : longitude location (degrees)
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Team-Unique Header:
    !
    !  Comments:
    !  1. This routine is part of the generic zone number routine.
    !  2. Longitude is not necessary for zone number calculation.  However,
    !     it has been included as a parameter to keep this routine from being
    !     executed in the event that someone accidentally sends the generic
    !     routine a single REAL-valued region number.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE grid_values, ONLY : INTEGER_BOUNDARIES
    IMPLICIT NONE
    REAL (real4), INTENT(in) :: colatitude
    REAL (real4), INTENT(in) :: longitude
    REAL :: adj_colatitude
    REAL :: dummy
    INTEGER (int4) :: zone_number2
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = 'grid_routines::zone_number2'

    !*** this is a dummy statement whose sole purpose is to quiet the compiler
    dummy = longitude

    !*** range check and adjust colatitude
    CALL check(subr_name, colat_val=colatitude)

    IF (INTEGER_BOUNDARIES) THEN
       IF (colatitude /= 0.0) THEN
          adj_colatitude = CEILING(colatitude) - 0.5
       ELSE
          adj_colatitude = colatitude
       END IF
    ELSE
       adj_colatitude = colatitude
    END IF

    !*** calculate zone number
    IF (NORTH_ZONE_NO < SOUTH_ZONE_NO) THEN

       !* north to south
       zone_number2 = NUM_ZONES - INT((180.0 - adj_colatitude)/zone_height())
    ELSE

       !* south to north
       zone_number2 = INT((180.0 - adj_colatitude)/zone_height()) + 1

    END IF

    !*** correct for out-of-bound conditions
    !*** (NOTE: this also puts North Pole in northern-most zone!!!)
    IF (zone_number2 < 1) THEN
       zone_number2 = 1
    END IF

    IF (zone_number2 > NUM_ZONES) THEN
       zone_number2 = NUM_ZONES
    END IF

    RETURN
  END FUNCTION zone_number2

  !**********************--------------------**********************
  !**********************  PRIVATE ROUTINES  **********************
  !**********************--------------------**********************

  SUBROUTINE calc_centroid_colat
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: calc_centroid_colat
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine calculates the colatitude of the centroid for the
    !  regions in each zone.
    !
    !  !Input Parameters:
    !  not applicable
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !Team-Unique Header:
    !
    !  Comments:
    !  1. This routine treats the region as an isosceles trapezoid which
    !     can be divided into a rectangle and two side right triangles.
    !  2. Since the verical component of the centroid is known for
    !     rectangles (1/2 height) and right triangles (1/3 height), this
    !     information can be used to estimate the vertical component of
    !     the centroid for the region.
    !  3. The colatitude of the centroid will be the same for all regions
    !     within a colatitude zone.
    !  4. The centroid colatitudes for each zone are stored within the
    !     module centroid_colat array.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE ceres_constants, only : &
         DEGREES_TO_RADIANS,    &
         EARTH_CIRCUM_MEAN

    IMPLICIT NONE
    REAL(real8) :: km_per_deg, km_per_deg_north, km_per_deg_south
    REAL(real8) :: north_colat, south_colat, north_colat_rad, south_colat_rad
    REAL(real8) :: north_width_km, south_width_km, min_width_km, delta_width_km
    REAL(real8) :: area, area_rectangle, area_triangle
    REAL(real8) :: zone_height_km, centroid_ratio
    REAL(real4) :: centroid_height
    INTEGER :: zone_no

    km_per_deg = EARTH_CIRCUM_MEAN / 360.0_real8
    zone_height_km = zone_height() * km_per_deg

    DO zone_no = 1, NUM_ZONES

       !*** calculate km per degree at northern and southern boundaries
       CALL get_south_north_dp(zone_no, south_colat, north_colat)

       north_colat_rad = north_colat * degrees_to_radians
       km_per_deg_north = km_per_deg * SIN(north_colat_rad)

       south_colat_rad = south_colat * degrees_to_radians
       km_per_deg_south = km_per_deg * SIN(south_colat_rad)

       !*** calculate north, south, min, and delta widths in kilometers
       north_width_km = region_width_dp(zone_no) * km_per_deg_north
       south_width_km = region_width_dp(zone_no) * km_per_deg_south
       min_width_km   = MIN(north_width_km, south_width_km)
       delta_width_km = ABS(north_width_km - south_width_km)

       !*** calc approx area of region by considering region as upright isosceles
       !*** trapezoid divided into rectangle and two side right triangles
       area_rectangle = zone_height_km * min_width_km
       area_triangle = 0.25 * zone_height_km * delta_width_km
       area = area_rectangle + 2.0 * area_triangle

       !*** use known centroid height of rectangles & triangles to
       !*** calculate centroid height of region
       centroid_ratio = (3.*area_rectangle + 4.*area_triangle)/(6.*area)
       centroid_height = centroid_ratio * zone_height()

       !*** now calculate centroid colatitude ...
       IF (north_width_km < south_width_km) THEN

          !*** ... for upright trapezoidal regions in northern hemisphere
          centroid_colat(zone_no) = south_colat - centroid_height
       ELSE

          !*** ... for upside-down trapezoidal regions in southern hemisphere
          centroid_colat(zone_no) = north_colat + centroid_height
       END IF

    END DO

    RETURN
  END SUBROUTINE calc_centroid_colat

  SUBROUTINE check(subr_name, regnum_val, zonum_val, colat_val)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: check
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine handles the checking of input parameters for the
    !  other routines in this module.
    !
    !  !Input Parameters:
    !  subr_name             : the subroutine sending the values
    !  regnum_val (optional) : region number value
    !  zonum_val (optional)  : zone number value
    !  colat_val (optional)  : colatitude value
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE msg, only : WriteReport
    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(in) :: subr_name
    INTEGER (int4), INTENT(in), OPTIONAL :: regnum_val, zonum_val
    REAL (real4), INTENT(in), OPTIONAL :: colat_val
    INTEGER :: mnemonic, ivar1, ivar2
    CHARACTER (LEN=60) :: strvar

    !*** check region number
    IF (PRESENT(regnum_val)) THEN
       IF ((regnum_val < 1) .or. (regnum_val > NUM_REGIONS)) THEN
          mnemonic = REFGRID_E_REGNUMOUTOFBOUNDS
          strvar = ' Region number %i is not between 1 and %i.'
          ivar1 = regnum_val
          ivar2 = NUM_REGIONS
          CALL WriteReport(mnemonic, subr_name, strvar, ivar1, ivar2)
       END IF
    END IF

    !*** check zone number
    IF (PRESENT(zonum_val)) THEN
       IF ((zonum_val < 1) .or. (zonum_val > NUM_ZONES)) THEN
          mnemonic = REFGRID_E_ZONENUMOUTOFBOUNDS
          strvar = ' Zone number %i is not between 1 and %i.'
          ivar1 = zonum_val
          ivar2 = NUM_ZONES
          CALL WriteReport(mnemonic, subr_name, strvar, ivar1, ivar2)
       END IF
    END IF

    !*** check colatitude value
    IF (PRESENT(colat_val)) THEN
       IF ((colat_val < 0.0) .or. (colat_val > 180.0)) THEN
          mnemonic = REFGRID_E_COLATOUTOFBOUNDS
          strvar = ' Colatitude %f is not between 0.0 and 180.0.'
          CALL WriteReport(mnemonic, subr_name, strvar, rvar1=colat_val)
       END IF
    END IF

    RETURN
  END SUBROUTINE check

  SUBROUTINE initialize_first_regions
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: initialize_first_regions
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine initializes the module array which contains the
    !  region number of the first region in each zone.
    !
    !  !Input Parameters:
    !  not applicable
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER :: zone_no
    INTEGER :: counter, i
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::initialize_first_regions"

    DO zone_no = 1, NUM_ZONES
       counter = 0
       DO i = 1, zone_no-1
          counter = counter + REGIONS_PER_ZONE(i)
       END DO
       first_region_in_zone(zone_no) = counter + 1
    END DO
    first_func_ready = .true.

    RETURN
  END SUBROUTINE initialize_first_regions

  SUBROUTINE initialize_region_widths
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: initialize_region_widths
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine initializes the module array which contains the
    !  degree width of regions in each zone.
    !
    !  !Input Parameters:
    !  not applicable
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER :: zone_no
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_routines::initialize_region_widths"

    DO zone_no = 1, NUM_ZONES
       region_width_in_zone(zone_no) = 360.0_real8 / REGIONS_PER_ZONE(zone_no)
    END DO
    width_func_ready = .true.

    RETURN
  END SUBROUTINE initialize_region_widths

END MODULE grid_routines
