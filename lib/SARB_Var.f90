MODULE SARB_Var
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    SARB_Var
!
! !Description:
!  Routine ID - G.5.1
!
!  Purpose - 
!
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NONE
!
! !Team Unique Header:
! 
!  Modules Used:
!   ceres_valregions - (CERESlib) Parameters: NumValReg_AWG
!   f90_kind         - (CERESlib) Parameters: INT2, INT4, REAL8
!   Profile_Params   - (SARBlib)  Parameters: MaxLev_Cld, MaxLev_Cld_2x, 
!                                             MaxLev_SARB, Num_ScnTyp,
!                                             NumLev_Start
!
!  Key Local Parameters:
!   AerClimSrc_Flag     - Source of the aerosol climatology flag used by the 
!                         current FOV
!   AerSrc_PrimInput    - Source of non-climatological aerosol data for a FOV
!   ap                  - TOA albedo
!   bbout ( I)          - Array of broadband albedo values adjusted by SARB
!                         for each pass through the Fu-Liou model
!   CDay                - Character version of data day
!   CHour               - Character version of data hour
!   CldBot ( I, J)      - Array of cloud bottom  heights (hPa) for each pass 
!                         through the Fu-Liou model
!                           I = Pass through the model index
!                           J = Cloud layer index
!   CldTop ( I, J)      - Array of cloud top heights (hPa) for each pass 
!                         through the Fu-Liou model
!                           I = Pass through the model index
!                           J = Cloud layer index
!   clwp ( I, J)        - Array of cloud liquid/ice water path values for each
!                         cloud layer for each pass through the Fu-Liou model
!                           I = Pass through the model index
!                           J = Cloud layer index
!   CMonth              - Character version of data month
!   Colat               - Colatitude of FOV at surface
!   Collins_AvailFlag   - Flag indicating the availability of Collins
!                         assimilated aerosol data for the current run of
!                         the subsystem
!   cw                  - Liquid/ice water paths derived by SARB from input 
!                         cloud optical depths
!   CYear               - Character version of data year
!   DataDay             - Day of month of data [1-31]
!   DataHour            - Hour of day of data [0-23]
!   DataMonth           - Month of year of data [1-12]
!   DataYear            - Year of data [1997-2032(???)]
!   DayNite             - Flag indicating whether the current FOV is considered
!                         day or night.  Defined as either Daytime or Nighttime 
!                         at the time of current FOV input data
!                         ingestion
!   DomAerPercent       - Percentage of dominant aerosol constituency
!   DomAerType          - Dominate aerosol scene type index
!   DrivTab_Unit        - Input unit numbers for derivative table ancillary 
!                         input data files
!   EarthSun_Sqr        - Earth-Sun distance, squared
!   EffTemp ( I, J)     - Array of effective temperatures for each cloud layer
!                         for each pass through the Fu-Liou model
!                           I = Pass through the model index
!                           J = Cloud layer index
!   Entropy             - Conditional to turn on or off entropy code
!   Flag_SpecErr        - Specifically identifies what input failed
!   GeoPt_Pro ( I, J)   - Geopotential height profile as a function of pressure
!                          I = Pass through the model index
!                          J = Geopotential Heights (hPa) for each level 
!                              in the vertical profile
!   GrDailyAvail        - Flag indicating whether or not the monthly file of
!                         daily gridded MODIS aerosol data is available
!   Ice_path ( I)       - Array of ice water path values for each cloud layer
!   igbp_prc            - Percentage of largest scene type constituent for 
!                         current FOV
!   igbp_sid            - Index of largest scene type constituent for current 
!                         FOV
!   IHour               - Integer version of data hour
!   Input_Time          - Julian time of current input record
!   imag_raz            - Imager relative azimuth angle over CERES FOV
!   imag_vzen           - Imager viewing zenith over CERES FOV
!   ipz                 - Array of cloud particle phase flags for each cloud 
!                         layer (NOTE: Different from Phase_mn input values
!                         from the primary input product)
!                          = 1; water
!                          = 2; ice
!   isc                 - ERBE scene identification index
!   isid                - Scene identification index for referencing sigma 
!                         files during the constrainment process
!                          = 0; ocean
!                          = 1; land
!   ISTAT_TUNE          - Describes tuning adjustment status.  Archived as
!                            QCFlags % Flag_Constrainment
!   Liquid_path         - Array of liquid water path values for each cloud layer
!   LW_TOA_flux         - CERES estimated LW flux at the TOA
!   LW_TOA_UnfiltRad    - CERES LW unfiltered radiance
!   Match_Vert_AvailFlag- Integer flag indicating availability of vertical MATCH data
!   MOA_FixLev          - Fixed levels, read from the MOA product header, for 
!                         MOA profile data
!   MOA_GrdIdx          - MOA product horizontal grid index, obtained from MOA 
!                         header (see MOA_IO.f90 for definitions)
!   MOA_PW              - Precipitable water obtained directly from MOA product 
!   MOA_SfcPres         - Surface pressure obtained directly from MOA product
!   MOA_Unit ( I)       - Array of Toolkit-assigned unit numbers for MOA 
!                         input files
!   NonClr_Frac         - Area fraction of FOV that is not clear sky
!   NumCld_FOV          - Number of cloud layers in current FOV
!   NumImagChan         - Number of imager channels for which data are
!                         on the input product
!   OptDepth_mn ( I, J) - Array of cloud optical depth means obtained from the 
!                         input product
!                           I = Pass through the model index
!                           J = Cloud layer index
!   OptDepthLin_mn ( I, J) - Array of linear cloud optical depth means obtained from the 
!                            input product
!                             I = Pass through the model index
!                             J = Cloud layer index
!   ozcm                - Column ozone for current FOV in cemtimeteres
!   part_rad ( I)       - Array of cloud particle size obtained from the 
!                         input product
!   Phase_mn ( I)       - Array of particle phase means obtained from the 
!                         input product
!                               Phase_mn = 1.0; All water
!                               Phase_mn = 2.0; All ice
!                         1.0 < Phase_mn > 2.0; water/ice mixture
!   Pristine            - Conditional to turn on or off pristine code
!   PW_Ratio_LowTrop    - Ratio of constrained lower tropospheric humidity to
!                         initial lower tropospheric humidity applied to 
!                         humidity profile values in the lower troposphere 
!                         prior to the constrained pass through the radiative
!                         transfer model
!   PW_Ratio_UpTrop     - Ratio of constrained upper tropospheric humidity to
!                         initial upper tropospheric humidity applied to 
!                         humidity profile values in the upper troposphere 
!                         prior to the constrained pass through the radiative
!                         transfer model
!   raz_toa             - CERES relative azimuth at surface
!   RelHum_UpTrop ( I)  - Array of upper tropospheric relative humidity for 
!                         each pass through the Fu-Liou model
!   RLat                - Latitude of FOV at surface
!   RLong               - Longitude of FOV at surface
!   SfcAirHumid         - Humidity profile value corresponding to the surface 
!                         air level
!   SfcAirTemp          - Temperature profile value corresponding to the surface
!                         air level
!   SfcAlt_mn           - Surface altitude
!   SfcArea ( I)        - Array of percent area coverages for the surface types
!                         obtained from primary input product for current FOV
!   SfcGeoPt            - Geopotential height profile value corresponding to
!                         the surface air level
!   SfcType ( I)        - Array of surface types obtained from primary input 
!                         product for current FOV
!   SfcUvect            - Wind speed u-vector component at the surface for the
!                         current FOV
!   SfcVvect            - Wind speed v-vector component at the surface for the
!                         current FOV
!   SfcWind             - Wind speed magnitude calculated from u and v vectors
!   SigTab_Unit         - Toolkit assigned unit number for the sigma tables 
!                         ancilliary input data files
!   SigTabVer           - Version number of the sigma table ancillary input data
!                         files
!   SkinTemp            - Surface skin temperature (SLATE FOR REMOVAL)
!   SnowIce_Flag        - Flag indicating presence of snow/ice for current FOV
!                         (SLATE FOR REMOVAL)
!   SolarZen_angle      - Solar zenith angle for current FOV
!   Sunglint_Percent    - Percentage of sunglint recorded for the current FOV
!   SW_TOA_flux         - CERES estimated SW flux at the TOA
!   Tuning              - Conditional to turn on or off tuning code
!   t_clwc              - Constrained cloud liquid water/ice content for each 
!                         cloud layer
!   ur                  - Cosine of the viewing zenith angle at the TOA
!   uth ( I)            - Upper troposheric humidity value used in the initial 
!                         pass through the radiative transfer model
!                           I = Pass through the model index
!   u0                  - Cosine of the solar zenith angle
!   ValOut_Flag         - Flag indicating whether or not current run is in 
!                         subset mode. Defined as either ValOut_OFF or ValOut_ON
!                         at the time of subsystem initialization.
!   ValReg_Flag         - Flag indicating whether or not current FOV corresponds
!                         to a CERES Validation Region.  Defined as either 
!                         ValReg_NO or ValReg_YES at the time of FOV input data
!                         ingestion.
!   vzen_toa                - Satellite viewing zenith angle for current FOV
!   WN_TOA_FiltRad      - Window channel filtered TOA radiance from the 
!                         primary input product
!   WN_TOA_flux         - CERES estimated WN channel flux at the TOA
!   WN_TOA_UnfiltRad    - CERES WN channel unfiltered radiance
!   WN_Width            - Window channel width of the CERES instrument used
!                         for the current run (microns)
!   WNflx_High          - Maximum value for window flux values written to 
!                         the archival output product, based on the width of 
!                         the individual instrument's window channel
!   WNflx_Low           - Minimum value for window flux values written to 
!                         the archival output product, based on the width of 
!                         the individual instrument's window channel
!   wssa                - Constituent weighted aerosol optical
!   wv                  - Column water vapor (precipitable water)
!   wv_LowerTrop ( I)   - Array of lower tropospheric precipitable water 
!                         for each pass through the Fu-Liou model
!   wv_UpperTrop ( I)   - Array of upper tropospheric precipitable water 
!                         for each pass through the Fu-Liou model
!
! !Revision History:
!  Version 4.2   May 27, 2020 - SCCR #1657
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Added entropy, pristine, and tuning logicals 
!  Version 4.1   June 4, 2014 - SCCR #1017
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Added variable solar constant provided by TISA group 
!  Version 4.0  June 17, 2004 - SCCR #541 
!               Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Addition of 
!                    Match_Vert_AvailFlag      MOA_PW          SfcWind
!  Version 3.5  August 1, 2003 - SCCR #458 
!               Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Transfer of QC_Unit parameter to Mod. InstSARB_IO_Params
!  Version 3.4  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Transfer of parameters to Mod. SARBInput_Params:
!                    AerRatio_Init      Nighttime         ValOut_ON
!                    Chan11             Tol_FracAtm       ValReg_NO
!                    ClrIdx             UTH_LowerBound    ValReg_YES
!                    Daytime            UTH_UpperBound
!                    MaxAdjust_Aer      ValOut_ON
!                2. Addtion of
!                    AOT_Stowe
!                    GrDailyAvail
!                    wssa
!  Version 3.2  December 24, 2001 - SCCR #315  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Transfered to Module SARB_OutVar
!                       hPa200_Idx
!                       hPa500_Idx
!                       hPa70_Idx
!                       TOALev_Idx
!                  2. Elimination of unused variables:
!                       DnFlux
!                       UpFlux
!                  3. Changed SigTabVer parameter to saved variable set in 
!                     sigma table file namelist
!   4. Removed CldBot_input parameter
!  Version 3.1   June 22, 2001 - SCCR #267  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removal of variable altitude, which was redundant
!                     with the variable SfcAlt_mn
!                  2. Removal of no longer used variables bbin, salb_in, 
!                     salb_out, tlat, tlong
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Addition of instrument index variable, Instr_Idx
!                  2. Addition of variable NumImagChan, the number of imager
!                     channels of data on the SSF
!                  3. Changed Variable Chan11 to a parameter
!                  4. Addition of variables CDay, CHour, CMonth, CYear, 
!                     and IHour
!                  5. Addition of variables DrivTab_Unit, MOA_Unit, QC_Unit,
!                     SigTab_Unit
!                  6. Addition of ValOut_Flag, ValOut_OFF, ValOut_ON,
!                     ValReg_Flag, ValReg_NO, ValReg_YES
!                  7. Addition of AOT_Obs
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Addition of:
!                      LW unfiltered radiance, TOA
!                      WN channel radiances, filtered and unfiltered, TOA
!                      ur, cosine of the viewing zenith angle, TOA 
!                      NumCld_FOV
!                      t_uth
!                      tlat
!                      tlong
!                      uthi
!                  2. Generic clean up and maintenance
!                  3. SigTabVer value increased to 3
!  Version 2.0   November 21, 1997  SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Release 2 code
!
! !END
!
!*****************************************************************************
!
!
      USE ceres_valregions, ONLY: NumValReg_AWG
      USE Profile_Params
      USE f90_kind
!
!
      INTEGER,           SAVE :: AerClimSrc_Flag
      INTEGER,           SAVE :: AerSrc_PrimInput
      REAL,              SAVE :: ap
      REAL,              SAVE :: bbout ( NumPass)
      CHARACTER (LEN=2), SAVE :: CDay
      CHARACTER (LEN=2), SAVE :: CHour
      REAL,              SAVE :: CldBot ( NumPass, MaxLev_Cld)
      REAL,              SAVE :: CldTop ( NumPass, MaxLev_Cld)
      REAL,              SAVE :: clwp ( MaxLev_Cld)
      CHARACTER (LEN=2), SAVE :: CMonth
      REAL,              SAVE :: Colat
      INTEGER,           SAVE :: Collins_AvailFlag
      REAL,              SAVE :: cw ( MaxLev_Cld)
      CHARACTER (LEN=4), SAVE :: CYear
      INTEGER,           SAVE :: DataDay
      INTEGER,           SAVE :: DataHour
      INTEGER,           SAVE :: DataMonth
      INTEGER,           SAVE :: DataYear
      INTEGER,           SAVE :: DayNite
      REAL,              SAVE :: DomAerPercent
      INTEGER,           SAVE :: DomAerType
      INTEGER,           SAVE :: DrivTab_Unit
      REAL,              SAVE :: EarthSun_Sqr
      REAL,              SAVE :: EffTemp ( NumPass, MaxLev_Cld)
      LOGICAL,           SAVE :: Entropy
      INTEGER,           SAVE :: Flag_OP
      INTEGER,           SAVE :: Flag_SpecErr
      REAL,              SAVE :: GeoPt_Pro ( NumPass, MaxLev_SARB)
      INTEGER,           SAVE :: GrDailyAvail
      REAL,              SAVE :: Ice_path ( MaxLev_Cld)
      INTEGER,           SAVE :: igbp_prc
      INTEGER,           SAVE :: igbp_sid
      REAL,              SAVE :: imag_raz 
      REAL,              SAVE :: imag_vzen
      INTEGER,           SAVE :: IHour
      REAL (REAL8),      SAVE :: Input_Time
      INTEGER,           SAVE :: ipz ( MaxLev_Cld)
      INTEGER,           SAVE :: isc
      INTEGER,           SAVE :: isid
      INTEGER,           SAVE :: ISTAT_TUNE
      REAL,              SAVE :: Liquid_path ( MaxLev_Cld)
      REAL,              SAVE :: LW_TOA_Flux
      REAL,              SAVE :: LW_TOA_UnfiltRad
      INTEGER,           SAVE :: NumDays_Mon
      INTEGER,           SAVE :: Match_Vert_AvailFlag
      REAL,              SAVE :: MOASkinTemp_Hourly
      REAL,              SAVE :: MOA_FixLev ( 58)
      INTEGER,           SAVE :: MOA_GrdIdx
      REAL,              SAVE :: MOA_PW
      REAL,              SAVE :: MOA_SfcPres
      INTEGER,           SAVE :: MOA_Unit ( 2)
      REAL,              SAVE :: NonClr_Frac
      INTEGER,           SAVE :: NumCld_FOV
      INTEGER,           SAVE :: NumImagChan
      REAL,              SAVE :: OptDepth_mn ( NumPass, MaxLev_Cld)
      REAL,              SAVE :: OptDepthLin_mn ( NumPass, MaxLev_Cld)
      REAL,              SAVE :: ozcm
      REAL,              SAVE :: part_rad ( MaxLev_Cld)
      REAL,              SAVE :: Phase_mn ( MaxLev_Cld)
      LOGICAL,           SAVE :: Pristine
      REAL,              SAVE :: PW_Ratio_LowTrop
      REAL,              SAVE :: PW_Ratio_UpTrop
      REAL,              SAVE :: raz_toa
      REAL,              SAVE :: RelHum_UpTrop ( NumPass)
      REAL,              SAVE :: RLat                  
      REAL,              SAVE :: RLong
      CHARACTER (LEN=5), SAVE :: SatName
      REAL,              SAVE :: SfcAirHumid
      REAL,              SAVE :: SfcAirTemp
      REAL,              SAVE :: SfcAlt_mn
      INTEGER (INT2),    SAVE :: SfcArea ( Num_SfcTyp)
      REAL,              SAVE :: SfcGeoPt
      INTEGER (INT2),    SAVE :: SfcType ( Num_SfcTyp)
      REAL,              SAVE :: SfcUvect
      REAL,              SAVE :: SfcVvect
      REAL,              SAVE :: SfcWind
      INTEGER,           SAVE :: SigTabVer 
      INTEGER,           SAVE :: SigTab_Unit
      REAL,              SAVE :: SkinTemp
      REAL,              SAVE :: SnowIce_Flag
      REAL,              SAVE :: SolarZen_angle
!      REAL,              SAVE :: Solar_Constant(31)
      REAL,              SAVE :: Solar_Constant(2050,12,31)
      INTEGER (INT2),    SAVE :: Sunglint_Percent
      REAL,              SAVE :: SW_TOA_Flux
      INTEGER,           SAVE :: SYN_Subset_Flag
      REAL,              SAVE :: t_clwc ( MaxLev_Cld)
      LOGICAL,           SAVE :: Tuning
      REAL,              SAVE :: uth ( NumPass)
      INTEGER,           SAVE :: ValOut_Flag
      INTEGER,           SAVE :: ValReg_Flag
      REAL,              SAVE :: vzen_toa
      REAL,              SAVE :: WN_TOA_FiltRad
      REAL,              SAVE :: WN_TOA_Flux
      REAL,              SAVE :: WN_TOA_UnfiltRad
      REAL,              SAVE :: WN_Width
      REAL,              SAVE :: WNflx_High
      REAL,              SAVE :: WNflx_Low
      REAL,              SAVE :: wssa
      REAL,              SAVE :: wv ( NumPass)
      REAL,              SAVE :: wv_LowerTrop ( NumPass)
      REAL,              SAVE :: wv_UpperTrop ( NumPass)
!
END MODULE SARB_Var
