MODULE Syn_Spectral_Dat_18
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SAHA
!
! !Description:
!
!    Purpose - Contains spectral albedo tables and routines for Synoptic SARB
!
!
! !Input Parameters:
!   NONE
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Internal Routines:
!    get_emiss            -
!    syn_land_spec18      - 
!    colorme_to_broadband - 
!    dickensonx           - 
!    wangx                - 
!
!  Modules Used:
!   SWDNWGTS - Used by Sbr. colorme_to_broadband  
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!

 USE SWDNWGTS,only: get_swdnwgts,wgt18

 IMPLICIT NONE
 
 REAL, PARAMETER :: u0_diffuse = 0.602
 
 INTEGER, PARAMETER :: nigbp = 25,nband = 18

 REAL ,SAVE :: specalbigbp ( nband, nigbp) ! *** Spectral Reflectances

 DATA specalbigbp /		      &
!Band
!    1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16   17   18
!Evergreen Needleleaf (MODIS 1 deg avg where IGBP = 1)
 0.019,0.019,0.019,0.019,0.019,0.019,0.019,0.019,0.045,0.035,0.122,0.269,0.269,0.259,0.150,0.065,0.065,0.065 &
!
!Tropical Broadleaf   (MODIS 1 deg avg where IGBP = 2) 
,0.027,0.027,0.027,0.027,0.027,0.027,0.027,0.027,0.047,0.041,0.150,0.331,0.331,0.330,0.170,0.066,0.066,0.066 &
!
!Deciduous Needleleaf (MODIS 1 deg avg where IGBP = 3) 
,0.021,0.021,0.021,0.021,0.021,0.021,0.021,0.021,0.056,0.040,0.133,0.291,0.291,0.299,0.187,0.077,0.077,0.077 &
!
!Deciduous Forest     (MODIS 1 deg avg where IGBP = 4) 
,0.020,0.020,0.020,0.020,0.020,0.020,0.020,0.020,0.053,0.040,0.179,0.412,0.412,0.388,0.214,0.082,0.082,0.082 &
!
!Mixed Deciduous/Evergreen (MODIS 1 deg avg where IGBP = 5)
,0.024,0.024,0.024,0.024,0.024,0.024,0.024,0.024,0.051,0.038,0.142,0.316,0.316,0.299,0.167,0.068,0.068,0.068 &
!
!Closed Shrubland  (Modified UV) 
,0.010,0.010,0.010,0.015,0.017,0.020,0.036,0.045,0.154,0.156,0.330,0.350,0.350,0.343,0.239,0.101,0.101,0.101 &
!
!Open Shrub (Fallow Wheat, Ok CARE)
,0.095,0.095,0.095,0.095,0.095,0.095,0.098,0.104,0.122,0.156,0.177,0.224,0.244,0.284,0.341,0.311,0.150,0.150 &
!
!Woody Savannah      (MODIS 1 deg avg where IGBP = 8) 
,0.023,0.023,0.023,0.023,0.023,0.023,0.023,0.023,0.056,0.043,0.160,0.355,0.355,0.351,0.198,0.080,0.080,0.080 &
!
!Savanna     (Grass from Ok CARE w/ Modified UV)
,0.010,0.010,0.010,0.015,0.017,0.020,0.036,0.045,0.126,0.129,0.374,0.402,0.402,0.395,0.301,0.145,0.071,0.071 &
!
!Grasslands  (Grass from Ok CARE w/ Modified UV)
,0.010,0.010,0.010,0.015,0.017,0.020,0.036,0.045,0.126,0.129,0.374,0.402,0.402,0.395,0.301,0.145,0.071,0.071 &
!
!Permenant Wetland   (Average of water and crop)
,0.028,0.028,0.028,0.031,0.032,0.033,0.041,0.046,0.081,0.093,0.237,0.240,0.228,0.223,0.143,0.068,0.037,0.037 &
!
!Cropland (Soybean from Ok CARE w/ Modified UV)    
,0.010,0.010,0.010,0.015,0.017,0.020,0.036,0.045,0.115,0.099,0.407,0.442,0.442,0.431,0.271,0.122,0.059,0.059 &
!
!Urban (Same as Open Shrub from Pinker & Karnelli not CARE fallow wheat)
,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.066,0.104,0.114,0.298,0.319,0.319,0.260,0.258,0.258,0.258,0.258 &
!
!Cropland/Natural Veg(Grass & Soybean from Ok CARE) w/ Modified UV       
,0.010,0.010,0.010,0.015,0.017,0.020,0.036,0.045,0.090,0.083,0.347,0.377,0.377,0.370,0.273,0.141,0.110,0.110 &
!
!Permanant Snow/Glacial Ice  (Jin 1000um)   
,0.910,0.910,0.910,0.916,0.921,0.931,0.947,0.964,0.953,0.920,0.664,0.635,0.635,0.594,0.013,0.006,0.010,0.014 &
!
!Barren Desert (Pinker & Karnelli)
,0.144,0.144,0.144,0.144,0.144,0.144,0.144,0.179,0.263,0.331,0.402,0.410,0.410,0.391,0.390,0.390,0.390,0.390 &
!
!Water  
,0.046,0.046,0.046,0.046,0.046,0.046,0.046,0.046,0.046,0.086,0.066,0.037,0.014,0.014,0.014,0.014,0.014,0.014 &
!
!Tundra (Briegleb et al w/ modified UV )            
,0.010,0.010,0.010,0.015,0.017,0.020,0.036,0.045,0.113,0.115,0.233,0.247,0.247,0.248,0.265,0.265,0.265,0.265 &
!
!Fresh Snow    (jin 50um)      
,0.979,0.979,0.979,0.980,0.982,0.984,0.988,0.992,0.989,0.982,0.910,0.902,0.902,0.852,0.143,0.168,0.018,0.015 &
!
!Sea Ice  (Briegleb et al)           
,0.778,0.778,0.778,0.778,0.778,0.778,0.778,0.778,0.778,0.778,0.443,0.443,0.443,0.240,0.055,0.054,0.036,0.036 &
!
!Permanant Snow/Glacial Ice  (Jin 1000um) #21  
,0.910,0.910,0.910,0.916,0.921,0.931,0.947,0.964,0.953,0.920,0.664,0.635,0.635,0.594,0.013,0.006,0.010,0.014 &
!
!Fresh Snow    (jin 50um)  #22    
,0.979,0.979,0.979,0.980,0.982,0.984,0.988,0.992,0.989,0.982,0.910,0.902,0.902,0.852,0.143,0.168,0.018,0.015 &
!
!Barren Desert (Pinker & Karnelli) #23
,0.144,0.144,0.144,0.144,0.144,0.144,0.144,0.179,0.263,0.331,0.402,0.410,0.410,0.391,0.390,0.390,0.390,0.390 &
!
!Barren Desert (Pinker & Karnelli) #24
,0.144,0.144,0.144,0.144,0.144,0.144,0.144,0.179,0.263,0.331,0.402,0.410,0.410,0.391,0.390,0.390,0.390,0.390 &
!
!Barren Desert (Pinker & Karnelli) #25
,0.144,0.144,0.144,0.144,0.144,0.144,0.144,0.179,0.263,0.331,0.402,0.410,0.410,0.391,0.390,0.390,0.390,0.390 /


! Aprox Integral Energy Weights at SZA=60, at the surface
! 0.000 0.000 0.000 0.000 0.002 0.016 0.076 0.099 0.160 0.143 0.119 0.094 0.087 0.109 0.063 0.026 0.002 0.003	


! *** ls(swNIR)/ln(swVIS)

        REAL, PARAMETER :: specshape ( nigbp) =      &       !! ln ( swIR / swVIS)
        (/ 1.59, 1.60, 1.59, 1.69,  1.78, 1.15,  0.62, 1.55,  1.45,  1.45,    &
           1.82, 1.69, 1.22, 1.71, -0.17, 0.50, -0.21, 1.06, -0.14, -0.67,    & 
	   -0.17,-0.21,0.62, 0.50,   0.00 /)

! *** Scene dependent solar zenith adjustment factor
! 0= BroadBand, 1= Visible, 2 = NIR
 REAL,SAVE::  d3(0:2,nigbp)
 
 DATA d3 &
 /0.40, 0.40, 0.40,& ! ( 1) EVERGREEN NEEDLE FOR
  0.44, 0.44, 0.44,& ! ( 2) EVERGREEN BROAD FOR 
  0.32, 0.32, 0.32,& ! ( 3) DECIDUOUS NEEDLE FOR
  0.39, 0.39, 0.39,& ! ( 4) DECIDUOUS BROAD FOR
  0.22, 0.22, 0.22,& ! ( 5) MIXED FOREST
  0.28, 0.28, 0.28,& ! ( 6) CLOSED SHRUBS
  0.40, 0.40, 0.40,& ! ( 7) OPEN/SHRUBS
  0.15, 0.15, 0.15,& ! ( 8) WOODY SAVANNA
  0.27, 0.27, 0.27,& ! ( 9) SAVANNA
  0.22, 0.22, 0.22,& ! (10) GRASSLAND
  0.35, 0.35, 0.35,& ! (11) WETLAND
  0.24, 0.24, 0.24,& ! (12) CROPLAND (CAGEX-APR)
  0.10, 0.10, 0.10,& ! (13) URBAN
  0.12, 0.12, 0.12,& ! (14) CROP MOSAIC
  0.10, 0.10, 0.10,& ! (15) ANTARCTIC SNOW
  0.40, 0.40, 0.40,& ! (16) BARREN/DESERT
  0.41, 0.41, 0.41,& ! (17) OCEAN WATER
  0.58, 0.58, 0.58,& ! (18) TUNDRA
  0.10, 0.10, 0.10,& ! (19) FRESH SNOW
  0.10, 0.10, 0.10,& ! (20) SEA ICE
  0.10, 0.10, 0.10,& ! (21) Large Grain Snow
  0.10, 0.10, 0.10,& ! (22) Small Grain Snow
  0.40, 0.40, 0.40,& ! (23) Green Desert
  0.40, 0.40, 0.40,& ! (24) Barren Desert
  0.111,0.111,0.111/ ! (25) SPARE
   

! *** Longwave Emmissivities in 12 Fu bands
 
      REAL :: emisstbl ( 12, nigbp)   
       DATA emisstbl /&
               0.989, 0.989, 0.990, 0.991, 0.991, 0.990,  &
               0.990, 0.995, 1.000, 1.000, 1.000, 1.000,  & !( 1) everg
               0.989, 0.989, 0.990, 0.991, 0.991, 0.990,  &
               0.990, 0.995, 1.000, 1.000, 1.000, 1.000,  & !( 2) everg
               0.985, 0.986, 0.984, 0.983, 0.979, 0.980,  &
               0.973, 0.987, 1.000, 1.000, 1.000, 1.000,  & !( 3) decid
               0.985, 0.986, 0.984, 0.983, 0.979, 0.980,  &
               0.973, 0.987, 1.000, 1.000, 1.000, 1.000,  & !( 4) decid
               0.987, 0.987, 0.987, 0.987, 0.985, 0.985,  &
               0.982, 0.991, 1.000, 1.000, 1.000, 1.000,  & !( 5) mixed
               0.949, 0.970, 0.974, 0.971, 0.947, 0.958,  &
               0.966, 0.975, 0.984, 0.984, 0.984, 0.984,  & !( 6) close
               0.873, 0.934, 0.944, 0.939, 0.873, 0.904,  &
               0.936, 0.942, 0.951, 0.951, 0.951, 0.951,  & !( 7) open
               0.987, 0.990, 0.992, 0.993, 0.983, 0.975,  &
               0.985, 0.993, 1.000, 1.000, 1.000, 1.000,  & !( 8) woody
               0.987, 0.990, 0.992, 0.993, 0.983, 0.975,  &
               0.985, 0.993, 1.000, 1.000, 1.000, 1.000,  & !( 9) savan
               0.987, 0.990, 0.992, 0.993, 0.983, 0.975,  &
               0.985, 0.993, 1.000, 1.000, 1.000, 1.000,  & !(10) grass
               0.983, 0.987, 0.987, 0.988, 0.983, 0.981,  &
               0.987, 0.982, 0.986, 0.986, 0.986, 0.986,  & !(11) perma
               0.987, 0.990, 0.992, 0.993, 0.983, 0.975,  &
               0.985, 0.993, 1.000, 1.000, 1.000, 1.000,  & !(12) cropl
               1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
               1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  & !(13) urban
               0.987, 0.989, 0.989, 0.990, 0.984, 0.980,  &
               0.983, 0.992, 1.000, 1.000, 1.000, 1.000,  & !(14) mosai
               1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
               1.000, 0.999, 0.999, 0.999, 0.999, 0.999,  & !(15) snow
               0.835, 0.916, 0.934, 0.923, 0.835, 0.877,  &
               0.921, 0.926, 0.934, 0.934, 0.934, 0.934,  & !(16) barre
               0.979, 0.983, 0.982, 0.982, 0.984, 0.987,  &
               0.989, 0.972, 0.972, 0.972, 0.972, 0.972,  & !(17) water
               0.947, 0.967, 0.988, 0.979, 0.975, 0.977,  &
               0.992, 0.989, 0.989, 0.989, 0.989, 0.989,  & !(18) tundr
               0.988, 0.988, 0.988, 0.988, 0.988, 0.988,  &
               0.988, 0.988, 0.988, 0.988, 0.988, 0.988,  & !(19) FRESH SNOW
               0.979, 0.979, 0.979, 0.979, 0.979, 0.979,  &
               0.979, 0.979, 0.979, 0.979, 0.979, 0.979,  & !(20) SEA ICE
               1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  &
               1.000, 0.999, 0.999, 0.999, 0.999, 0.999,  & !(21) Large Grain Snow
               0.988, 0.988, 0.988, 0.988, 0.988, 0.988,  &
               0.988, 0.988, 0.988, 0.988, 0.988, 0.988,  & !(22) Small Grain Snow
               0.949, 0.970, 0.974, 0.971, 0.947, 0.958,  &
               0.966, 0.975, 0.984, 0.984, 0.984, 0.984,  & !(23) Green Desert
               0.835, 0.916, 0.934, 0.923, 0.835, 0.877,  &
               0.921, 0.926, 0.934, 0.934, 0.934, 0.934,  & !(24) Barren Desert
               1.000, 1.000, 1.000, 1.000, 1.000, 1.000,  & 
               1.000, 1.000, 1.000, 1.000, 1.000, 1.000   / !(25) Spare            


      REAL :: bb ( nigbp)

      CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE get_emiss (igbp,ems12) 
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   get_emiss
!
! !Description:
!    Purpose - Lookup emissivity for IGBP scene type
!
! 
! !Input Parameters:
!    igbp
!
! !Output Parameters:
!    ems12
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: igbp
      REAL,    INTENT (OUT) :: ems12(12)

      ems12 ( 1:12)  = emisstbl ( 1:12, igbp)

      END SUBROUTINE get_emiss
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE syn_land_spec18 (igbp, u0, pw, lntau,  salb18, bbalb)  
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   syn_land_spec18
!
! !Description:
!    Purpose - Assign spectral albedos for 18 bands
!
! 
! !Input Parameters:
!    igbp, u0, pw, lntau
!
! !Output Parameters:
!    salb18, bbalb
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN)  :: igbp
      REAL,    INTENT (IN)  :: u0,pw,lntau
      REAL,    INTENT (OUT) :: salb18(nband),bbalb
      INTEGER :: i
      REAL    :: zenadj(0:3) 
       
      bbalb           = -1.0
      salb18(1:nband) = -1.0
      
      IF ( u0 < 0 ) RETURN
      
      IF ( igbp > 12 .OR. igbp == 11 ) THEN
!      Use old D value method for cases NOT developed by Z.Wang 
!       zenadj(0) = ( 1.0 + d3(0,igbp)) / (1.0 + 2.0*d3(0,igbp)*u0) 
       zenadj(1) = ( 1.0 + d3(1,igbp)) / (1.0 + 2.0*d3(1,igbp)*u0) 
       zenadj(2) = ( 1.0 + d3(2,igbp)) / (1.0 + 2.0*d3(2,igbp)*u0) 
       salb18( 1:10) = specalbigbp (  1:10, igbp) * zenadj(1) !Vis
       salb18(11:18) = specalbigbp ( 11:18, igbp) * zenadj(2) !NIR 
      ELSE
!      Z.Wang 2 parameter method 20070807 
       zenadj(0) = wangx(1.0 , igbp, 0.5 ,u0) ! From cos(60) to actual
       salb18( 1:18) = specalbigbp (  1:18, igbp)* zenadj(0)
      ENDIF

      WHERE (salb18 > 1.0) salb18 = 1.0
      WHERE (salb18 < 0.0) salb18 = 0.0

      bbalb = colorme_to_broadband( igbp,pw,u0,lntau, salb18)

END SUBROUTINE syn_land_spec18
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
REAL FUNCTION colorme_to_broadband( igbp,pw,u0,lntau, salb18)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   colorme_to_broadband
!
! !Description:
!    Purpose - 
!
! 
! !Input Parameters:
!    igbp,pw,u0,lntau,salb18
!
! !Output Parameters:
!    
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT(IN) :: igbp
      REAL,    INTENT(IN) :: pw,u0,lntau,salb18(18)

!     CALL swdn_wgts_200601 (u0, pw,igbp,   wgts)
      
      IF ( igbp == 17 ) THEN
 	 wgt18%isid = 1 ! (Ocean(1), Crop(2),Desert(3) ,Snow(4)
      ELSEIF ( igbp == 16 .OR. igbp == 7 )THEN
 	 wgt18%isid = 3 
      ELSEIF ( igbp == 15 .OR. igbp == 19 .OR. igbp == 20 ) THEN
 	 wgt18%isid = 4
      ELSE
 	 wgt18%isid = 2 
      ENDIF
      
      wgt18%lnpw   = LOG(pw)  !lnPW
      wgt18%u0     = u0
      wgt18%lnctau = lntau
      
!     Ed2 swdown weight LUT

      CALL get_swdnwgts
 	     
      colorme_to_broadband = SUM (wgt18%wgt(1:18) * salb18(1:18) )

END FUNCTION colorme_to_broadband
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
REAL FUNCTION dickensonx(a1,dd,u1,u2)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   dickensonx
!
! !Description:
!    Purpose - 
!
! 
! !Input Parameters:
!    a1,dd,u1,u2
!
! !Output Parameters:
!    
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
 REAL, INTENT (IN) :: a1
 REAL, INTENT (IN) :: dd
 REAL, INTENT (IN) :: u1
 REAL, INTENT (IN) :: u2

 dickensonx = a1 *(1.0+2.0*dd*u1)/(1.0+2.0*dd*u2)

END FUNCTION dickensonx
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
REAL FUNCTION wangx(a1, igbp, u1,u2)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   wangx
!
! !Description:
!    Purpose - 
!
! 
! !Input Parameters:
!    a1, igbp, u1,u2
!
! !Output Parameters:
!    
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
!JGR Vol 102,D02103  Zhuo Wang 
! Moderate Resolution Imaging Spectroradiometer bidirectional 
! reflectance distribution function !based albedo parameterization 
! for weather and climate models 
! 20070806
! Only for IGBP types 1-10 &11

 IMPLICIT NONE

 REAL,    INTENT(IN) :: a1,u1,u2
 INTEGER, INTENT(IN) :: igbp

 REAL rad1,rad2
 REAL g1,g2
 REAL RRR,t
 REAL, DIMENSION(12) :: b1,b2

 DATA b1/0.68,0.63,0.64,0.56,0.65,0.56,0.45,0.62,0.60,0.57,-9. ,0.62/
 DATA b2/0.36,0.31,0.32,0.23,0.30,0.22,0.18,0.26,0.27,0.26,-9. ,0.27/

 g1(t) = -0.007574 - 0.070987*t**2.0 + 0.307588*t**3.0
 g2(t) = -1.284909 - 0.166314*t**2.0 + 0.041840*t**3.0

 rad1 = acos(u1)
 rad2 = acos(u2)

 RRR  = 1 + (  b1(igbp)*(g1(rad2)-g1(rad1) ) + &
               b2(igbp)*(g2(rad2)-g2(rad1) ) )

 wangx = a1*RRR

END FUNCTION wangx
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE Syn_Spectral_Dat_18

