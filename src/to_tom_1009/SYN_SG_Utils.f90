!==============================================================
!! THIS SUBROUTINE TRANSLATES FROM THE NESTED Longitude INDEX OF THE 
!! EQUAL AREA GRID into the Begining and Ending Longitude INDEX in the
!! 1-360 equal angle 1deg grid system.

SUBROUTINE synx ( ilat, ilon_nest ,&
             ibeg,iend,ierr)
    
!   
    INTEGER, INTENT(IN)::  ilat,ilon_nest
!ilat :: Latitude index (1 to 180) ( NP Zone to SP zone)
!ilon_nest :: longitude index of the CERES NESTED grid scheme for the latitude "ilat"

    INTEGER, INTENT(OUT):: ibeg,iend,ierr
!ibeg,iend :: Begining and Ending grid box number in the 360 Equal Angle  grid scheme
!ierr :: 0 if ilat and ilon_nest combination is valid ( -1 otherwise)

    INTEGER nlon_ea, ilons_ea(180),npg

    INTEGER icells(180)

    DATA icells / &
    1,  45,  45,  45,  45,  45,  45,  45,  45,  45,     &
    90,  90,  90,  90,  90,  90,  90,  90,  90 , 90,     &
    180, 180, 180, 180, 180, 180, 180, 180, 180, 180,     &
    180, 180, 180, 180, 180, 180, 180, 180, 180, 180,     &
    180, 180, 180, 180, 180, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 360, 360, 360, 360, 360,     &
    360, 360, 360, 360, 360, 180, 180, 180, 180, 180,     &
    180, 180, 180, 180, 180, 180, 180, 180, 180, 180,     &
    180, 180, 180, 180, 180, 180, 180, 180, 180, 180,     &
    90,  90,  90,  90,  90,  90,  90,  90,  90,  90,     &
    45,  45,  45,  45,  45,  45,  45,  45,  45,1/

    ibeg= -1
    iend= -1
    ierr=0

    IF ( ilon_nest > icells(ilat) ) THEN
      ierr=-1
      RETURN
    ENDIF

    npg = 360/icells(ilat)

    ibeg = (ilon_nest-1)*npg + 1 
    iend = (ilon_nest)*npg 

!!! BEGIN CORRECTION TO INDEX from -180 :180 in to (0:360 of sg maps)
if ( ilat == 1 .or. ilat == 180 ) then
ibeg = 1
iend = 360
return
endif

ibeg= ibeg+180
iend= iend+180
if ( ibeg > 360 ) ibeg = ibeg-360
if ( iend > 360 ) iend = iend-360
if ( ibeg > iend ) iend=360
if ( ibeg > iend ) Stop 'SYNXERR'
!END CORRECTION TO INDEX from -180 :180 in to (0:360 of sg maps)
 
END SUBROUTINE synx
!
! ******************************************************************
!
SUBROUTINE sg_average(imode,sg1,sgx)
!
    USE SYNSPECTRAL , only : SYNGRID,maxast,maxcld,omit
!
    
!
    INTEGER imode
    TYPE ( SYNGRID ) sg1, sgx
    TYPE ( SYNGRID ), SAVE :: sga
    TYPE ( SYNGRID ), SAVE:: sgc
    INTEGER, SAVE, DIMENSION(360) :: bba_sf,spec_sf
    INTEGER, SAVE :: nfill
!    INTEGER, DIMENSION(-1:4) :: lit
    INTEGER, DIMENSION(-1:16) :: lit
!
    IF ( imode == 0 ) THEN     ! INITALIZE
!      PRINT*, 'INITALIZE'
  sgc%sgr_long_sto  = 0
  sgc%ptv               = 0
  sgc%p17		= 0
  sgc%bba_untuned       = 0
  sgc%sfc_spectral_alb= 0
  sgc%sfc_broadband_alb= 0 
  sgc%bbainit_grid= 0 
  sgc%bbainit_ocean= 0
  sgc%bbainit_nonocean= 0 
  sgc%bbalut_clr= 0 
  sgc%bbalut_sno= 0 
  sgc%bbalut_sah= 0 
  sgc%sfc_spectral_emis= 0
!
  sga%sgr_long_sto  = 0
  sga%ptv    = 0  
  sga%p17		= 0
  sga%bba_untuned= 0
  sga%sfc_spectral_alb= 0
  sga%sfc_broadband_alb= 0 
  sga%bbainit_grid= 0 
  sga%bbainit_ocean= 0
  sga%bbainit_nonocean= 0 
  sga%bbalut_clr= 0 
  sga%bbalut_sno= 0 
  sga%bbalut_sah= 0 
  sga%sfc_spectral_emis= 0
!
  bba_sf  = omit
  spec_sf = omit
  nfill   = 0
!
    ELSEIF ( imode == 1 ) THEN ! FILL
!
!Transfer latest inputs NO AVERAGEING
  sga%u0        =sg1%u0
  sga%pw=sg1%pw
  sga%ncld=sg1%ncld
  sga%cldfrac=sg1%cldfrac
  sga%cldtau=sg1%cldtau
  sga%aertau=sg1%aertau
  sga%wind=sg1%wind
  sga%chlor=sg1%chlor
  sga%tice=sg1%tice
  sga%aice=sg1%aice
  sga%toa_tot=sg1%toa_tot
  sga%toa_clr=sg1%toa_clr
  sga%o3=sg1%o3
  sga%psfc=sg1%psfc
  sga%wssa=sg1%wssa
  sga%clearfrac=sg1%clearfrac
!
  WHERE ( sg1%ptv >= 0. .AND. sg1%ptv <= 100.) 
    sga%ptv= sga%ptv+ sg1%ptv
    sgc%ptv = sgc%ptv + 1
  END WHERE
  !
  WHERE ( sg1%sgr_long_sto >= 0. .AND. sg1%sgr_long_sto <= 10000. ) 
    sga%sgr_long_sto= sga%sgr_long_sto+ sg1%sgr_long_sto
    sgc%sgr_long_sto = sgc%sgr_long_sto + 1
  END WHERE
  
  
  
  IF (sg1%p17 >= 0. .AND. sg1%p17 <= 100. ) then
    sga%p17 = sga%p17  + sg1%p17
    sgc%p17 = sgc%p17  + 1
  END IF
  
!
  IF ( sg1%bba_untuned >= 0. .AND. sg1%bba_untuned <= 1.0) THEN
    sga%bba_untuned = sga%bba_untuned+ sg1%bba_untuned
    sgc%bba_untuned = sgc%bba_untuned + 1
  ENDIF
!
  WHERE ( sg1%sfc_spectral_alb >= 0. .AND. sg1%sfc_spectral_alb <= 1.0)
    sga%sfc_spectral_alb= sga%sfc_spectral_alb + sg1%sfc_spectral_alb
    sgc%sfc_spectral_alb= sgc%sfc_spectral_alb + 1
  END WHERE
!
  WHERE ( sg1%sfc_broadband_alb >= 0. .AND. sg1% sfc_broadband_alb <= 1.0)
    sga%sfc_broadband_alb= sga%sfc_broadband_alb   + sg1%sfc_broadband_alb
    sgc%sfc_broadband_alb= sgc%sfc_broadband_alb + 1
  END WHERE
!
  WHERE ( sg1%bbainit_grid >= 0. .AND. sg1%bbainit_grid <= 1.0 )
    sga%bbainit_grid= sga%bbainit_grid  + sg1%bbainit_grid
    sgc%bbainit_grid= sgc%bbainit_grid + 1
  END WHERE
!
  WHERE ( sg1%bbainit_ocean >= 0. .AND. sg1%bbainit_ocean <= 1.0 )
    sga%bbainit_ocean= sga%bbainit_ocean + sg1%bbainit_ocean
    sgc%bbainit_ocean= sgc%bbainit_ocean + 1
  END WHERE
!
  WHERE ( sg1%bbainit_nonocean >= 0. .AND. sg1%bbainit_nonocean <= 1.0 )
    sga%bbainit_nonocean= sga%bbainit_nonocean + sg1%bbainit_nonocean
    sgc%bbainit_nonocean= sgc%bbainit_nonocean + 1
  END WHERE
!
  IF  ( sg1%bbalut_clr >= 0. .AND. sg1%bbalut_clr <= 1.0 ) THEN
    sga%bbalut_clr= sga%bbalut_clr + sg1%bbalut_clr
    sgc%bbalut_clr= sgc%bbalut_clr + 1
  ENDIF
!
  IF  ( sg1%bbalut_sno >= 0. .AND. sg1%bbalut_sno <= 1.0 ) THEN
    sga%bbalut_sno= sga%bbalut_sno + sg1%bbalut_sno
    sgc%bbalut_sno= sgc%bbalut_sno + 1
  ENDIF
!
  IF  ( sg1%bbalut_sah >= 0. .AND. sg1%bbalut_sah <= 1.0 ) THEN
    sga%bbalut_sah= sga%bbalut_sah + sg1%bbalut_sah
    sgc%bbalut_sah= sgc%bbalut_sah + 1
  ENDIF
!
  WHERE ( sg1%sfc_spectral_emis >= 0. .AND. sg1%sfc_spectral_emis  <= 1.0 )
    sga%sfc_spectral_emis= sga%sfc_spectral_emis + sg1%sfc_spectral_emis
    sgc%sfc_spectral_emis= sgc%sfc_spectral_emis + 1
  END WHERE
!  
  nfill = nfill + 1
  IF ( nfill > 360 ) STOP ' redimension'
  bba_sf(nfill)  = sg1%bba_source_flag
  spec_sf(nfill) = sg1%spec_source_flag
!
    ELSEIF ( imode == 2 ) THEN ! AVERAGE
!
!      PRINT*, 'AVG--------------------------------------------------------------------'
 WHERE (sgc%sgr_long_sto >0 ) sga%sgr_long_sto = sga%sgr_long_sto/sgc%sgr_long_sto
  WHERE (sgc%ptv >0 ) sga%ptv = sga%ptv/sgc%ptv
  if ( sgc%p17 > 0 )  sga%p17 = sga%p17/sgc%p17
!
  IF( sgc%bba_untuned > 0  ) sga%bba_untuned = sga%bba_untuned /sgc%bba_untuned
!
  WHERE (sgc%sfc_spectral_alb >0 ) sga%sfc_spectral_alb= sga%sfc_spectral_alb/sgc%sfc_spectral_alb
  WHERE (sgc%sfc_broadband_alb >0 ) sga%sfc_broadband_alb= sga%sfc_broadband_alb/sgc%sfc_broadband_alb
  WHERE (sgc%bbainit_grid > 0 ) sga%bbainit_grid = sga%bbainit_grid /sgc%bbainit_grid
  WHERE (sgc%bbainit_ocean > 0 ) sga%bbainit_ocean = sga%bbainit_ocean /sgc%bbainit_ocean
  WHERE (sgc%bbainit_nonocean > 0 ) sga%bbainit_nonocean = sga%bbainit_nonocean /sgc%bbainit_nonocean
!
  IF (sgc%bbalut_clr > 0 ) sga%bbalut_clr = sga%bbalut_clr /sgc%bbalut_clr
!
  IF (sgc%bbalut_sno > 0 ) sga%bbalut_sno = sga%bbalut_sno /sgc%bbalut_sno
  IF (sgc%bbalut_sah > 0 ) sga%bbalut_sah = sga%bbalut_sah /sgc%bbalut_sah
  IF( sgc%bba_untuned == 0 ) sga%bba_untuned = omit
!
  IF (sgc%bbalut_clr == 0 ) sga%bbalut_clr = omit
  IF (sgc%bbalut_sno == 0 ) sga%bbalut_sno = omit
  IF (sgc%bbalut_sah == 0 ) sga%bbalut_sah = omit
!
  WHERE (sgc%sfc_spectral_emis > 0 ) sga%sfc_spectral_emis = sga%sfc_spectral_emis /sgc%sfc_spectral_emis
!
!  PRINT*,bba_sf(1:10)
!  PRINT*,spec_sf(1:10)
!  lit(0)=-777
!  lit(1)=count(bba_sf == 1)
!  lit(2)=count(bba_sf == 2)
!  lit(3)=count(bba_sf == 3)
!  lit(4)=count(bba_sf == 4)
!
!  lit(-1:-1)= maxloc ( lit(1:4) )

  lit(0:16)=0
  lit(0)=count(bba_sf == 0) ! Ocean
  lit(1)=count(bba_sf == 1) ! FLSA
  lit(2)=count(bba_sf == 2) ! Assalut
  lit(3)=count(bba_sf == 3) ! Pert SAH
  
  lit(5)=count(bba_sf == 5) !! SGR Retrieved
  lit(6)=count(bba_sf == 6) !! SGR OLD
    
    
  lit(10)=count(bba_sf == 10) ! Bad Ocean
  lit(11)=count(bba_sf == 11) ! Bad FLSA
  lit(12)=count(bba_sf == 12) ! BadAssalut
  lit(13)=count(bba_sf == 13) ! Bad Pert SAH
  
  lit(15)=count(bba_sf == 15) !! BadSGR Retrieved
  lit(16)=count(bba_sf == 16) !! BadSGR OLD

  
!  lit(14)=count(bba_sf  > 13 .or. bba_sf  < 0 .or. (bba_sf > 3 .and. bba_sf< 10) ) ! Catch Wierdness 
  lit(14)=count(bba_sf  > 16 .or. (bba_sf > 6 .and. bba_sf< 10) ) ! Catch Wierdness 
  lit(-1:-1)= maxloc ( lit(0:16) ) - 1

  sga%bba_source_flag = lit(-1)

  if (sga%bba_source_flag == 14) sga%bba_source_flag = 32767   !HG-20250922

  !!!HG-20250924
  !if (sga%bba_source_flag > 100 ) then
  !print *, "500 DEBUG: sga%bba_source_flag =", sga%bba_source_flag
  !stop
  !endif

!
  lit(0)=count(spec_sf == 0) !MCD43C1
  lit(1)=count(spec_sf == 1) !Ocean Jin
  lit(2)=count(spec_sf == 2) !Land Default
  lit(3)=count(spec_sf == 3) !Jin Snow
  lit(4)=count(spec_sf == 4) !Jin Ice 
  lit(5)=count(spec_sf == 5) !Jin Snow Retrieval
  lit(6)=count(spec_sf == 6) !OLD SGR REtrieval
  
  lit(-1:-1)= maxloc (lit(0:6 )) - 1
! 
      sga%spec_source_flag = lit(-1) 
!
  sgx = sga !! TRANSFER
!
  ELSE      !! BADMODE
    STOP
  ENDIF
!
END SUBROUTINE sg_average
!
! ************************************************************
!
SUBROUTINE SynGrid_Init (ZoneLat)
!
USE SYNSPECTRAL
USE SARB_IO_Utils
USE pcf
USE SynSARB_IO_Params
USE SARB_Var
USE CHLOR_CLIM
USE RDICEAGEMAP
!

CHARACTER*3 :: FName
INTEGER, INTENT(OUT) :: ZoneLat
INTEGER :: iostat
INTEGER :: IncPath_No = 0
!
 CALL Driver_Create
!
 CALL flsa_lut_loader_0404  !! Open and initalize Flsa LUT table
!
 CALL open_em_maps ! Open CLOUD WG emissivity maps (8.55,10.8,11.9) ,Single Lat per read
!
 CALL convert_sah_to_1x1deg ! Grid Avg 10' Surface Albedo history map to 1deg Cerese
!
 CALL open_igbp_zone ! Open IGBP map ,Single Lat per read
!
 CALL open_sarbcrh   ! Open IGBP CRH0.63,1.6, snowmap, ice map , ,Single Lat per read
!
 CALL open_chlor_clim ! Opens/reads chlorophyll data
!
IF ((DataYear == 2021 .AND. DataMonth >= 8) .OR. (DataYear > 2021)) THEN
 CALL get_afwa_ice_age_map
ELSE
! Use PRD maps prior to August 2021
 CALL get_ice_age_map ! Opens/reads/calculates age of ice
ENDIF


!
 CALL GetFileName (LID_ZONE,                           & ! I
                          FName, iostat,                        & ! O
                          incPath = IncPath_No )      ! I
!
 READ (FName(1:3), '(i3)') ZoneLat
 CALL syn_init(Zonelat)
!
END SUBROUTINE SynGrid_Init   
! 
 
