module simple_tsi_mod
  implicit none

  integer, parameter :: num_surftyp = 5
  integer, parameter :: cld_layrs = 2
  integer, parameter :: num_overlap = 3

  ! Time and Location Data
  type :: Time_and_location_data
    integer :: region_num
    integer :: hour_box_num
    real    :: julhr
    real    :: sfc_alt
    real    :: cos_sol_zenith
    real    :: surf_perc_cov(num_surftyp)
  end type

  ! Cloud Layer
  type :: Cld_cat
    real :: areafrac_total
    real :: optdepth_mn
    real :: efftemp_mn
  end type

  ! Fluxes
  type :: Fluxes_tsi
    real :: total_sw
    real :: total_lw
  end type

  ! Overlap Data
  type :: Overlap_data
    real :: areafrac_ov(num_overlap)
  end type

  ! Surface Data
  type :: Surface_data
    real :: sfc_skin_temp
  end type

  ! Main TSI Record
  type :: TSI_TYPE
    type(Time_and_location_data) :: timeandloc
    type(Cld_cat)                :: cld_layer(cld_layrs)
    type(Fluxes_tsi)             :: fluxes
    type(Overlap_data)           :: overlap
    type(Surface_data)           :: surface
  end type

contains

  subroutine TSI_init(tsi)
    type(TSI_TYPE), intent(out) :: tsi
    integer :: i
    tsi%timeandloc%region_num = 0
    tsi%timeandloc%hour_box_num = 0
    tsi%timeandloc%julhr = 0.0
    tsi%timeandloc%sfc_alt = 0.0
    tsi%timeandloc%cos_sol_zenith = 0.0
    tsi%timeandloc%surf_perc_cov = 0.0
    do i = 1, cld_layrs
      tsi%cld_layer(i)%areafrac_total = 0.0
      tsi%cld_layer(i)%optdepth_mn = 0.0
      tsi%cld_layer(i)%efftemp_mn = 0.0
    end do
    tsi%fluxes%total_sw = 0.0
    tsi%fluxes%total_lw = 0.0
    tsi%overlap%areafrac_ov = 0.0
    tsi%surface%sfc_skin_temp = 0.0
  end subroutine

  subroutine TSI_print(tsi)
    type(TSI_TYPE), intent(in) :: tsi
    integer :: i
    print *, "Region:", tsi%timeandloc%region_num
    print *, "Hour Box:", tsi%timeandloc%hour_box_num
    print *, "Julian Hour:", tsi%timeandloc%julhr
    print *, "Surface Altitude:", tsi%timeandloc%sfc_alt
    print *, "Cosine Solar Zenith:", tsi%timeandloc%cos_sol_zenith
    print *, "Surface % Cover:", tsi%timeandloc%surf_perc_cov
    do i = 1, cld_layrs
      print *, "Cloud Layer", i, ": AreaFrac=", tsi%cld_layer(i)%areafrac_total, &
               "OptDepth=", tsi%cld_layer(i)%optdepth_mn, &
               "EffTemp=", tsi%cld_layer(i)%efftemp_mn
    end do
    print *, "Fluxes: SW=", tsi%fluxes%total_sw, " LW=", tsi%fluxes%total_lw
    print *, "Overlap AreaFrac:", tsi%overlap%areafrac_ov
    print *, "Surface Skin Temp:", tsi%surface%sfc_skin_temp
  end subroutine

  ! Subroutine to get record length for direct access files
  subroutine get_tsi_reclen(tsi_reclen)
    integer, intent(out) :: tsi_reclen
    type(TSI_TYPE) :: tsi
    inquire(iolength=tsi_reclen) tsi
  end subroutine

end module simple_tsi_mod
