MODULE TSI_TYPE_3x3_MOD

  USE ceres_defaults, ONLY : int4_dflt, int2_dflt, real4_dflt

  USE ceres_indices,  ONLY : num_landtyp, num_seatyp, opt_bins
  USE ceres_status,   ONLY : OK, BAD, DIRECT_FILE, DA_EOF, EOF
  USE ceres_time,      ONLY : HOURS_IN_DAY, MAX_DAYS_IN_MONTH
  USE f90_kind,       ONLY : int2, int4, real4, real8
  USE MSG,            ONLY : WriteReport, WriteStatus
  USE IO,             ONLY : OpenFile, CloseFile

  !!USES ALL OF THE EXISTING DATA STRUCTURES.
  !! THE ONLY DIFFERENCE IS THERE ARE 9 cld_cat MEMBERS ARRANGED IN 3X3 CONFIGURATION
  USE tsi_type_mod

  IMPLICIT NONE
  PRIVATE
  PUBLIC  :: TSI_3X3_TYPE

  INTEGER,PARAMETER            :: num_optical_depths        = 3
  INTEGER,PARAMETER            :: num_pressures             = 3

  !***                          TSI RECORD                         ***
  TYPE TSI_3X3_TYPE
     SEQUENCE
     TYPE (Time_and_location_data)       timeandloc
     TYPE (clr_sky_area_data)            clrsky_area_data
     TYPE (Cld_cat)                      cld_bin(num_pressures,num_optical_depths)
     TYPE (Fluxes_tsi)                   fluxestsi
     TYPE (Overlap_data)                 overlap
     TYPE (Surface_data)                 surface
     TYPE (aerosol_data)                 aerosol
  END TYPE TSI_3X3_TYPE


!   TYPE(TSI_3X3_TYPE), PARAMETER :: DFLT_TSI_RECORD =   &
!       & TSI_TYPE (dflt_timeandloc,dflt_clrsky,dflt_cld,dflt_Fluxes,dflt_overlap, &
!                  & dflt_surface,dflt_aerosol)
 
END MODULE TSI_TYPE_3x3_MOD


