MODULE grid_nest_routines
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !  !F90
  !  Name: grid_nest_routines
  !
  !  !Description
  !  Routine ID:
  !
  !  Purpose:
  !  This module provides routines for querying the nesting characteristics
  !  of the CERES reference grid.
  !
  !  Revision 1.1  1995/05/09
  !  Joe Stassi, SAIC (j.c.stassi@larc.nasa.gov)
  !  Initial version of code
  !
  !  !end
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  USE f90_kind, only : int4, real4, real8
  USE grid_routines, only : first_region, get_south_north_dp,      &
       last_region, number_of_regions, region_area, region_number, &
       region_width_dp, zone_height, zone_number
  USE grid_values, only : NUM_ZONES, NUM_REGIONS, STARTING_LONGITUDE,  &
       SOUTH_ZONE_NO, NORTH_ZONE_NO, NESTING_FACTOR, NUM_NESTED_REGIONS
  IMPLICIT NONE

  INCLUDE 'PGS_REFGRID_26410.f'

  PRIVATE
  PUBLIC :: &
       get_nested_borders,        &
       get_nested_centroid,       &
       last_nested_region,        &
       nested_centroid_distance,  &
       nested_number,             &
       nested_region_number,      &
       nested_region_number_alt,  &
       nested_region_area,        &
       nested_region_width,       &
       nested_region_width_dp,    &
       number_nested_regions,     &
       sequence_number,           &
       sequence_number_alt,       &
       region_reps

  !*---------------------------------*
  !* module variables and PARAMETERS *
  !*---------------------------------*
  REAL(real4) :: nested_centroid_colat(NUM_ZONES) = -1.0
  INTEGER, PARAMETER :: NO = 0, YES = 1

  !*-------------------------*
  !* module interface blocks *
  !*-------------------------*
  INTERFACE nested_region_number
     MODULE PROCEDURE nested_region_number, nested_region_number_alt
  END INTERFACE

  INTERFACE sequence_number
     MODULE PROCEDURE sequence_number, sequence_number_alt
  END INTERFACE

CONTAINS

  !**********************-------------------**********************
  !**********************  PUBLIC ROUTINES  **********************
  !**********************-------------------**********************

  SUBROUTINE get_nested_borders( &
       region_no, colat_south, colat_north, long_west, long_east)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: get_nested_borders
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine calculates the colatitude and longitude borders of
    !  a specified region within the nested grid.
    !
    !  !Input Parameters:
    !  not applicable
    !
    !  Input/Output Parameters
    !  region_no : region number identifier for nested grid (see Comments below)
    !
    !  !Output Parameters:
    !  colat_south : colatitude of region's southern boundary
    !  colat_north : colatitude of region's northern boundary
    !  long_west   : longitude of region's western boundary
    !  long_east   : longitude of region's eastern boundary
    !
    !  Comments:
    !  In nested grid, region_no gets converted to the first nested region
    !  within the region, i.e. the region grid identifier
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: region_no
    REAL (real4), INTENT(out) :: colat_south, colat_north
    REAL (real4), INTENT(out) :: long_west, long_east
    REAL (real8) :: south_dp, north_dp, west_border_dp
    REAL (real8) :: width_dp, nested_width_dp
    INTEGER (int4) :: zone_no
    CHARACTER(LEN=*), PARAMETER :: &
         subr_name = "grid_nest_routines::get_nested_borders"

    !*** range check region number
    CALL check(subr_name, regnum_val=region_no, nest_flag=YES)

    !*** calculate the north/south borders of zone
    zone_no = zone_number(region_no)
    CALL get_south_north_dp(zone_no, south_dp, north_dp)
    colat_north = north_dp
    colat_south = south_dp

    !*** calculate the east/west borders
    width_dp = region_width_dp(zone_no)
    nested_width_dp = nested_region_width_dp(zone_no)
    west_border_dp = STARTING_LONGITUDE + &
         (region_no - first_region(zone_no))*width_dp
    long_west = west_border_dp
    long_east = west_border_dp + nested_width_dp

    long_west = MODULO(long_west, 360.0)
    long_east = MODULO(long_east, 360.0)

    RETURN
  END SUBROUTINE get_nested_borders

  SUBROUTINE get_nested_centroid(region_no, colatitude, longitude)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: get_nested_centroid
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine returns the colatitude/longitude location for the
    !  centroid of a specified region within the nested grid.
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
    REAL (real4), INTENT(out)  :: colatitude, longitude
    REAL (real8) :: width_dp, nested_width_dp
    INTEGER      :: zone_no
    CHARACTER (LEN=*), PARAMETER :: &
         subr_name = "grid_nest_routines::get_nested_centroid"

    !*** range check region number
    CALL check(subr_name, regnum_val=region_no, nest_flag=YES)

    !*** colatitude of nested centroid
    zone_no = zone_number(region_no)
    IF (nested_centroid_colat(zone_no) == -1.0) THEN
       CALL calc_nested_centroid_colat
    END IF
    colatitude = nested_centroid_colat(zone_no)

    !*** longitude of nested centroid
    width_dp = region_width_dp(zone_no)
    nested_width_dp = nested_region_width_dp(zone_no)

    longitude = STARTING_LONGITUDE +                      &
         (region_no - first_region(zone_no))*width_dp +   &
         0.5_real8*nested_width_dp

    longitude = MODULO(longitude, 360.0)

    RETURN
  END SUBROUTINE get_nested_centroid

  FUNCTION last_nested_region(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: last_nested_region
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the region number of the last region within
    !  a latitudinal zone of a nested grid.
    !
    !  !Input Parameters:
    !  zone_no : zone number for which to find the last nested grid
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in) :: zone_no
    INTEGER (int4) :: last_nested_region
    INTEGER :: region_no

    region_no = last_region(zone_no)
    last_nested_region = nested_region_number(region_no)

    RETURN
  END FUNCTION last_nested_region

  FUNCTION nested_centroid_distance(region_no, colatitude, longitude) &
       result (angular_distance_deg)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_centroid_distance
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine calculates and returns the approximate earth central
    !  angle distance between a given colatitude/longitude location and the
    !  centroid of a specified region in the nested grid.
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
         subr_name = "grid_nest_routines::nested_centroid_distance"

    !*** range check the region number and colatitude
    CALL check(subr_name, regnum_val=region_no, nest_flag=YES, &
         colat_val=colatitude)

    !*** find region centroid location
    CALL get_nested_centroid(region_no, centroid_colatitude, centroid_longitude)

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

    RETURN
  END FUNCTION nested_centroid_distance

  FUNCTION nested_number(sequence_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_number
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the region region number of a given nested
    !  sequence number.
    !
    !  !Input Parameters:
    !  nested_no : nested region number
    !
    !  Implementation/Unique Features:
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE msg, only : WriteReport
    IMPLICIT NONE
    INTEGER, INTENT(in) :: sequence_no
    INTEGER :: nested_number
    INTEGER :: region_sum, zone, first
    INTEGER :: mnemonic, ivar1, ivar2
    CHARACTER (LEN=50) :: strvar
    CHARACTER (LEN=*), PARAMETER :: &
         funcname = "grid_nest_routines::nested_number"

    IF ((sequence_no < 1) .or. (sequence_no > NUM_NESTED_REGIONS)) THEN
       mnemonic = REFGRID_E_SEQNUMOUTOFBOUNDS
       strvar = ' Sequence number %i is not between 1 and %i.'
       ivar1 = sequence_no
       ivar2 = NUM_NESTED_REGIONS
       CALL WriteReport(mnemonic, funcname, strvar, ivar1, ivar2)
    END IF

    region_sum = 0
    DO zone = 1, NUM_ZONES
       region_sum = region_sum + number_nested_regions(zone)
       IF (region_sum >= sequence_no) THEN
          EXIT
       END IF
    END DO
    region_sum = region_sum - number_nested_regions(zone)
    first = first_region(zone)
    nested_number = first + (sequence_no - region_sum - 1)*NESTING_FACTOR(zone)

    RETURN
  END FUNCTION nested_number

  FUNCTION nested_region_number(region_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_region_number
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the nested region number for a given
    !  un-nested region number.
    !
    !  !Input Parameters:
    !  region_no : un-nested region number
    !
    !  Implementation/Unique Features:
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER, INTENT(in) :: region_no
    INTEGER :: nested_region_number
    INTEGER :: zone_no, nest_fact
    CHARACTER (LEN=*), PARAMETER :: &
         funcname = "grid_nest_routines::nested_region_number"

    CALL check(funcname, regnum_val=region_no)
    zone_no = zone_number(region_no)
    nest_fact = NESTING_FACTOR(zone_no)

    IF (nest_fact == 1) THEN
       nested_region_number = region_no
    ELSE
       nested_region_number = region_no - MOD(region_no-1, nest_fact)
    END IF

    RETURN
  END FUNCTION nested_region_number

  FUNCTION nested_region_number_alt(colatitude, longitude) result (region_num)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_region_number_alt
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the nested region number for a given
    !  colatitude/longitude location.
    !
    !  !Input Parameters:
    !  colatitude : colatitude location (degrees)
    !  longitude  : longitude location (degrees)
    !
    !  Implementation/Unique Features:
    !  For the non-nested grid, this function simply returns the
    !  region_number output.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    REAL (real4), INTENT(in) :: colatitude, longitude
    INTEGER :: region_num
    INTEGER :: region_no

    region_no = region_number(colatitude, longitude)
    region_num = nested_region_number(region_no)

    RETURN
  END FUNCTION nested_region_number_alt

  FUNCTION nested_region_area(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_region_area
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the approximate area (km**2) of regions within
    !  a specified latitudinal zone of a nested grid.
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
    REAL :: nested_region_area
    nested_region_area = region_area(zone_no) * NESTING_FACTOR(zone_no)
    RETURN
  END FUNCTION nested_region_area

  FUNCTION nested_region_width(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_region_width
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the angular width of regions within a specified
    !  latitudinal zone of a nested grid.
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
    REAL (real4) :: nested_region_width
    nested_region_width = region_width_dp(zone_no) * NESTING_FACTOR(zone_no)
    RETURN
  END FUNCTION nested_region_width

  FUNCTION nested_region_width_dp(zone_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: nested_region_width_dp
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the angular width (double precision) of regions
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
    REAL (real8) :: nested_region_width_dp
    nested_region_width_dp = region_width_dp(zone_no) * NESTING_FACTOR(zone_no)
    RETURN
  END FUNCTION nested_region_width_dp

  RECURSIVE FUNCTION number_nested_regions(zone_no) result (region_counter)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: number_nested_regions
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the number of regions within a specified
    !  latitudinal zone for a nested grid.  If no zone number is specified,
    !  it returns the total number of regions within the entire nested grid.
    !
    !  !Input Parameters:
    !  zone_no (optional) : zone number
    !
    !  !Output Parameters:
    !  not applicable
    !
    !  Implementation/Unique Features:
    !  This function is implemented as a recursive function so that the
    !  number of regions within each zone can be counted in order to get
    !  the total number of regions in the nested grid.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER (int4), INTENT(in), OPTIONAL :: zone_no
    INTEGER (int4) :: region_counter
    INTEGER :: zone

    IF (PRESENT(zone_no)) THEN

       !*** find number of regions in specific zone of nested grid
       region_counter = number_of_regions(zone_no)/NESTING_FACTOR(zone_no)

    ELSE

       !*** find total number of regions in nested zone
       region_counter = 0
       DO zone = 1, NUM_ZONES
          region_counter = region_counter + number_nested_regions(zone)
       END DO

    END IF

    RETURN
  END FUNCTION number_nested_regions

  FUNCTION region_reps(region_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: region_reps
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns a value indicating how many un-nested regions
    !  are represented by the inputted un-nested region. (The whole group
    !  represents a nested region)
    !
    !  !Input Parameters:
    !  region_no : inputted un-nested region number
    !
    !  Implementation/Unique Features:
    !  1. If the region is in an un-nested zone, then it just represents
    !     itself.
    !  2. If the region is the first region in a nested zone, then it
    !     represents the rest of regions in the zone.  In this case,
    !     region_reps equals the number of regions per nested regions.
    !  3. If the region is in a nested zone but is not the first region
    !     in the zone, then it isn't used.  Region_reps equals zero.
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER, INTENT(in) :: region_no
    INTEGER :: region_reps
    INTEGER :: zone_no, nest_fact
    CHARACTER (LEN=*), PARAMETER :: &
         funcname = "grid_nest_routines::region_reps"

    CALL check(funcname, regnum_val=region_no)

    zone_no = zone_number(region_no)
    nest_fact = NESTING_FACTOR(zone_no)

    IF (region_no == nested_region_number(region_no)) THEN
       region_reps = nest_fact
    ELSE
       region_reps = 0
    END IF

    RETURN
  END FUNCTION region_reps

  FUNCTION sequence_number(nested_no)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: sequence_number
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the region sequence number within the nested
    !  grid of a given nested region number.
    !
    !  !Input Parameters:
    !  nested_no : nested region number
    !
    !  Implementation/Unique Features:
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    INTEGER, INTENT(in) :: nested_no
    INTEGER :: sequence_number
    INTEGER :: zone_no, zone
    INTEGER :: sequence_in_zone, nested_sequence_in_zone

    !*** set sequence number to zero
    sequence_number = 0

    !*** find sequence number only for legitimate nested region numbers
    IF (nested_region_number(nested_no) == nested_no) THEN
       zone_no = zone_number(nested_no)

       sequence_in_zone = nested_no - first_region(zone_no)
       nested_sequence_in_zone = sequence_in_zone/NESTING_FACTOR(zone_no) + 1

       DO zone = 1, zone_no - 1
          sequence_number = sequence_number + number_nested_regions(zone)
       END DO
       sequence_number = sequence_number + nested_sequence_in_zone
    END IF

    RETURN
  END FUNCTION sequence_number

  FUNCTION sequence_number_alt(colatitude, longitude)
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: sequence_number_alt
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This function returns the region sequence number within the nested
    !  grid of a given colatitude/longitude pair.
    !
    !  !Input Parameters:
    !  colatitude
    !  longitude
    !
    !  Implementation/Unique Features:
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    IMPLICIT NONE
    REAL (real4), INTENT(in) :: colatitude, longitude
    INTEGER :: region_num
    INTEGER :: sequence_number_alt

    region_num = nested_region_number(colatitude, longitude)
    sequence_number_alt = sequence_number(region_num)

    RETURN
  END FUNCTION sequence_number_alt

  !**********************--------------------**********************
  !**********************  PRIVATE ROUTINES  **********************
  !**********************--------------------**********************

  SUBROUTINE calc_nested_centroid_colat
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !  !F90
    !  Name: calc_nested_centroid_colat
    !
    !  !Description
    !  Routine ID:
    !
    !  Purpose:
    !  This subroutine calculates the colatitude of the centroid for the
    !  regions in each zone.  It makes these calculations for a nested grid.
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
    !  4. The centroid colatitudes for each zone of a nested grid
    !     are stored within the nested_centroid_colat array.
    !  5. If the zones at the poles consist of a single region, then the
    !     centroid for those regions are the poles themselves (i.e.
    !     colat = 0.0 or 180.0 at polar zones consisting of a single region.)
    !
    !  !end
    !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    USE ceres_constants, only : &
         DEGREES_TO_RADIANS,    &
         EARTH_CIRCUM_MEAN


    IMPLICIT NONE
    REAL (real8) :: km_per_deg, km_per_deg_north, km_per_deg_south
    REAL (real8) :: north_colat, south_colat, north_colat_rad, south_colat_rad
    REAL (real8) :: north_width_km, south_width_km, min_width_km, delta_width_km
    REAL (real8) :: area, area_rectangle, area_triangle
    REAL (real8) :: zone_height_km, centroid_ratio
    REAL (real8) :: nested_width_dp
    REAL (real4) :: centroid_height
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

       !*---------------------*
       !* nested grid regions *
       !*---------------------*

       !*** calculate north, south, min, and delta widths in kilometers
       nested_width_dp = nested_region_width_dp(zone_no)
       north_width_km = nested_width_dp * km_per_deg_north
       south_width_km = nested_width_dp * km_per_deg_south
       min_width_km   = MIN(north_width_km, south_width_km)
       delta_width_km = ABS(north_width_km - south_width_km)

       !*** calc approx area of region by considering it as upright isosceles
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
          nested_centroid_colat(zone_no) = south_colat - centroid_height

       ELSE

          !*** ... for upside-down trapezoidal regions in southern hemisphere
          nested_centroid_colat(zone_no) = north_colat + centroid_height
       END IF

    END DO

    !*----------------------------------*
    !* nested grid regions at the poles *
    !*----------------------------------*
    IF (number_nested_regions(zone_no=1) == 1) THEN

       IF (NORTH_ZONE_NO < SOUTH_ZONE_NO) THEN
          nested_centroid_colat(1) = 0.0
          nested_centroid_colat(NUM_ZONES) = 180.0
       ELSE
          nested_centroid_colat(1) = 180.0
          nested_centroid_colat(NUM_ZONES) = 0.0
       END IF
    END IF

    RETURN
  END SUBROUTINE calc_nested_centroid_colat

  SUBROUTINE check(subr_name, regnum_val, zonum_val, colat_val, nest_flag)
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
    !  nest_flag (optional)  : nesting flag (=1 if validating a nested
    !                          grid region number.
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
    INTEGER, OPTIONAL :: nest_flag
    INTEGER :: nested_regnum_val
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

       IF (PRESENT(nest_flag)) THEN
          IF (nest_flag == 1) THEN
             nested_regnum_val = nested_region_number(regnum_val)
             IF (nested_regnum_val /= regnum_val) THEN
                mnemonic = REFGRID_E_NESTREGNUMERR
                strvar = &
                     ' Region #%i is not in the nested grid, but maps to #%i.'
                ivar1 = regnum_val
                ivar2 = nested_regnum_val
                CALL WriteReport(mnemonic, subr_name, strvar, ivar1, ivar2)
             END IF
          END IF
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

END MODULE grid_nest_routines
