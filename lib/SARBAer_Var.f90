MODULE SARBAer_Var
!
!***************************************************************************
!
! !F90
! 
! !Name:
!   SARBAer_Var
!
! !Description:
!   Routine ID -
!
!   Purpose - A module of variables and constants associated with the input
!             aerosol data sets accessed by the SARB Subsystems
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
!  Key Local Parameters:
!   AOT_Obs                - Observed aerosol from the primary input product
!   AOT_SSFA               - Array of SSFA aerosol depth values for each MODIS
!                            wavelngth
!   AOT_Stowe              - Observed Stowe aerosol from the primary input 
!                            product
!   collins_aots           - Aerosol Optical Depths for each type
!                            ( AEROD DSTODX01 DSTODX02 DSTODX03 DSTODX04 SO4OD  
!                              TAUSSLT BCPHIOD BCPHOOD OCPHIOD OCPHOOD )
!                           ( NOTE: DST  = dust
!                             BC   = black carbon
!                             OC   = organic carbon
!                             SSLT = sea salt
!                             SO4  = sulfate
!                             PHI  = hydrophilic
!                             PHO  = hydrophobic )
!                             AEROD(time, lat, lon); total aerosol AOD
!                             DSTODX01(time, lat, lon); dust AOD ( 0.01 - 1 mm)
!                             DSTODX02(time, lat, lon); dust AOD ( 1 - 10 mm)
!                             DSTODX03(time, lat, lon); dust AOD (10 - 20 mm)
!                             DSTODX04(time, lat, lon); dust AOD (20 - 50 mm)
!                             SO4OD(time, lat, lon); sulfate AOD
!                             TAUSSLT(time, lat, lon); sea-salt AOD
!                             BCPHIOD(time, lat, lon); hydrophilic black                                                       !                                                      carbon AOD
!                             BCPHOOD(time, lat, lon); hydrophobic black 
!                                                      carbon AOD
!                             OCPHIOD(time, lat, lon); hydrophilic organic 
!                                                      carbon AOD
!                             OCPHOOD(time, lat, lon); hydrophobic organic
!                                                      carbon AOD
!
!   collins_aots_regrouped ( I) - Collins aerosol regrouped to map into Fu-Liou
!                                 types
!                                  I = 0 ; Total Aerosol optical depth
!                                  I = 1 ; DUST small (4) 2km
!                                  I = 2 ; DUST large (6) 1km
!                                  I = 3 ; Sulfate (18) 5km
!                                  I = 4 ; Sea salt (1) 0.5km
!                                  I = 5 ; Black Carbons (11) 5km
!                                  I = 6 ; Soluble (10) 5km
!                                  I = 7 ; Insoluble (9) 5km
!   ipaer ( I, J, K)            - Array of MATCH data corresponding to the 
!                                 current time
!                                   I = Longitudinal index
!                                   J = Latitudinal index
!                                   K = Aerosol scene type index
!   itranityp_rgc               - COLLINS --> FULIOU TYPE FOR EACH CONSTITUENT
!   MATCH_Terra_RecLen          - Length of a Terra, or daily, MATCH
!                                 data record
!   MATCH_TRMM_RecLen           - Length of a TRMM, or six-hourly, MATCH
!                                 data record
!   nlat                        - MATCH data (ipaer array) latitudinal dimension
!   nlon                        - MATCH data (ipaer array) longitudinal 
!                                 dimension
!   ntime                       - MATCH data (ipaer array) .... array dimension
!   ntyp                        - Number of aerosol scene types included in 
!                                 the MATCH data set
!   NumConst_MATCH              - Number of MATCH aerosol constituencies used
!                                 by SARB
!   NumWL_SSFA                  - Number of wavelengths at which SSFA aerosol
!                                 optical depths are available
!   rlatca                      - Array of the latitudinal centers for the
!                                 MATCH grid boxes
!   sh_aer_rgc                  - Assigned Scale Hgt
!    
! !Revsion History:
!    Version 4.0   June 10, 2004 - SCCR #541 
!                  Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                    1. Moved parameters for using aerosol control file
!                       to Control_Mod
!    Version 3.6   January 16, 2004 - SCCR #498 
!                  Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                    1. Updated assigned scale heights (sh_aer_rgc) to:
!                       sh_aer_rgc / 3.0, 1.0, 3.6, 0.5, 3.5, 3.8, 3.8 /
!    Version 3.5   August 1, 2003 - SCCR #458 
!                  Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                    1. Added parameters for using aerosol control file
!    Version 3.4   December 7, 2001 - SCCR #315
!                  Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                    1. Break-out from Module Collins_assimilation
!    Version 3.1   June 22, 2001 - SCCR #267
!                  Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                    Changed:
!                       sh_aer_rgc / 2.0, 1.0, 5.0, 0.5, 5.0, 5.0, 5.0 /
!                     To:
!                       sh_aer_rgc / 3.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0 /
!    
! !END
!
!***************************************************************************
!
      USE f90_kind, ONLY: INT2
      
! *** MATCH parameters
!
      INTEGER,   PARAMETER :: nlon = 192
      INTEGER,   PARAMETER :: nlat = 94
      INTEGER,   PARAMETER :: ntime = 972
      INTEGER,   PARAMETER :: ntyp = 11
      INTEGER,   PARAMETER :: NumConst_MATCH = 7
!
      REAL,      SAVE      :: collins_aots ( 11) 
      REAL,      SAVE      :: collins_aots_regrouped ( 0:NumConst_MATCH) 
      INTEGER (INT2), SAVE :: ipaer ( nlon, nlat, ntyp)      
!      INTEGER,   PARAMETER :: itranityp_rgc ( NumConst_MATCH) =           &
!                                              (/ 4, 6, 18, 1, 11, 10, 9 /) 
!      INTEGER,   PARAMETER :: itranityp_rgc ( NumConst_MATCH) =           &
!                                              (/ 19, 21, 18, 1, 11, 10, 9 /) 
      INTEGER,   PARAMETER :: itranityp_rgc ( NumConst_MATCH) =           &
                                              (/ 24, 25, 18, 1, 11, 10, 9 /) 
      INTEGER,   PARAMETER :: MATCH_Terra_RecLen = nlon * nlat * ntyp * 2
      INTEGER,   PARAMETER :: MATCH_TRMM_RecLen  = nlon * nlat * 2
!
!
      REAL,      PARAMETER :: rlatca ( nlat) =                             &
      (/ -88.54195,-86.65317,-84.75323, -82.85078,-80.94736,-79.0434,      &
         -77.13935, -75.23505, -73.33066, -71.42619,-69.52167,-67.617,     &
         -65.71251, -63.8079, -61.90326, -59.99861,-58.09395,-56.1892,     &
         -54.2846, -52.37991, -50.47522, -48.57052, -46.66582,-44.7611,    &
         -42.8564, -40.95169, -39.04697, -37.14225, -35.23753,-33.3328,    &
         -31.42808, -29.52336, -27.61863, -25.7139, -23.80917,-21.9044,    &
         -19.99971,-18.09498,-16.19024,-14.28551, -12.38078,-10.4760,      &
          -8.571308, -6.666573, -4.761838, -2.857103, -0.952368,0.95236,    &
           2.857103, 4.761838, 6.666573,8.571308,10.47604,12.38078,14.285,  &
          16.19024, 18.09498, 19.99971,21.90444,23.80917,25.7139, 27.618,   &
          29.52336, 31.42808, 33.33281,35.23753,37.14225,39.04697,40.951,   &
          42.8564, 44.76111, 46.66582,48.57052,50.47522,52.37991, 54.284,   &  
          56.18928, 58.09395, 59.99861,61.90326,63.8079,65.71251, 67.617,   &
          69.52167, 71.42619, 73.33066,75.23505,77.13935,79.04349,80.947,   &
          82.85078, 84.75323, 86.65317,88.54195 /)
!
!
!      REAL,      PARAMETER :: sh_aer_rgc ( 7) =                             &
!                                       (/3.0, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0 /) 
      REAL,      PARAMETER :: sh_aer_rgc ( 7) =                             &
                                       (/3.0, 1.0, 3.6, 0.5, 3.5, 3.8, 3.8 /) 
!
!
! *** SSF/SSFA parameters
! 
      INTEGER, PARAMETER :: NumWL_SSFA = 7
      REAL,    SAVE      :: AOT_SSFA ( NumWL_SSFA)
!
! *** TISA aerosol parameters
! 
      INTEGER, PARAMETER :: NumWL_TISA = 7
      REAL,    SAVE      :: AOT_TISA ( NumWL_TISA)
!
      REAL,    SAVE      :: AOT_Obs
      REAL,    SAVE      :: AOT_Stowe
!
      REAL,    SAVE      :: TISA_AerConst_Ratio ( 7)
!
END MODULE SARBAer_Var
