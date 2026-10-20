MODULE SARBInput_Params
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SARBInput_Params
!
! !Description:
!   Routine ID -
!
!   Purpose - To provide a module of parameters such as flags, indicies, and
!             limits for use in the ingestion of input data
!
!
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!   AerConst_Angexp       - Array of aerosol angstrom exponents for each 
!                           aersosol scene type
!   Aer_Continental       - Parameter indicating that the aerosol scene type
!                           is continental
!   Aer_Maritime          - Parameter indicating that the aerosol scene type
!                           is maritime
!   Aer_Urban             - Parameter indicating that the aerosol scene type
!                           is urban
!   AerSrc_ClimOnly       - Parameter indicating that aerosol data are only
!                           available from the climatological sources
!   AerSrc_Collins        - Parameter for indicating that the source of the
!                           total aerosol optical depth data input to the  
!                           radiation transfer model for the current FOV is  
!                           the Collins assimilated aerosol data set
!   AerSrc_CollinsBackup  - Parameter for indicating that the source of the
!                           total aerosol optical depth data input to the  
!                           radiation transfer model for the current FOV is  
!                           the MODIS MATCH assimilated aerosol data set
!   AerSrc_GFDL           - Parameter for indicating that the source of the
!                           aerosol optical depth data input to the radiation 
!                           transfer model for the current FOV is the GFDL 
!                           aerosol climatology
!   AerSrc_GridTerraMODIS - Parameter for indicating that the source of the
!                           aerosol optical depth data input to the radiation 
!                           transfer model for the current FOV is the daily
!                           gridded MODIS aerosol data set
!   AerSrc_FOVTerraMODIS  - Parameter for indicating that the source of the
!                           aerosol optical depth data input to the radiation 
!                           transfer model for the current FOV is the 
!                           concurrent MODIS aerosol data that has been through 
!                           the CERES convolution process
!   AerSrc_StoweFOV       - Parameter for indicating that the source of the
!                           aerosol optical depth data input to the radiation 
!                           transfer model for the current FOV is the aerosol 
!                           data from the primary input product that is a
!                           result of the Stowe algorithm
!   CentWL_63             - Central wavelength of 0.55 microns
!   CERCRS                - Flag value indicating the requested output
!                           product is the CERES CRS
!   CERCRSDG              - Flag value indicating the requested output
!                           product is the CERES CRSDG
!   CERSYN                - Flag value indicating the requested output
!                           product is the CERES SYN 
!   CERES_FM1             - CERES Terra FM1 instrument
!   CERES_FM2             - CERES Terra FM2 instrument
!   CERES_FM3             - CERES Aqua FM3 instrument
!   CERES_FM4             - CERES Aqua FM4 instrument
!   CERES_FM5             - CERES FM5 instrument
!   CERES_PFM             - CERES TRMM PFM instrument
!   Chan11                - Parameter used in determining which imager channel 
!                           from the primary input product corresponds to the  
!                           11 micron channel
!   ClrIdx                - Index into cloud property arrays for clear sky 
!                           conditions
!   Collins_Available     - Parameter for indicating that the Collins aerosol 
!                           assimilation is available for the current run
!   Collins_Unavailable   - Parameter for indicating that the Collins aerosol 
!                           assimilation is unavailable for the current run 
!   COSDayNite_Terminator - Value of the solar zenith angle cosine that 
!                           separates daytime and nighttime 
!   Daytime               - Parameter indicating current FOV is daytime
!                            (Solar zenith angle <= to 90 degrees)
!   DesertIdx             - Array of IGBP scene type indicies that correspond 
!                           to the ERBE Desert geographic scene type
!   ERBECoast             - Parameter for indicating the ERBE Coastal geographic
!                           scene type
!   ERBEDesert            - Parameter for indicating the ERBE Desert geographic
!                           scene type
!   ERBELand              - Parameter for indicating the ERBE Land geographic
!                           scene type
!   ERBEOcean             - Parameter for indicating the ERBE Ocean 
!                           geographic scene type
!   ERBESnow              - Parameter for indicating the ERBE Snow 
!                           geographic scene type
!   ERBEUnknown           - Parameter for indicating that the ERBE 
!                           geographic scene type is unknown
!   GrDailyAvail_NO       - Parameter for indicating that the monthly file of
!                           daily gridded MODIS aerosol data is not available
!   GrDailyAvail_YES      - Parameter for indicating that the monthly file of
!                           daily gridded MODIS aerosol data is available
!   IcePhase              - Parameter indicating ice phase for a cloud layer
!   IN2                   - Parameter indicating two-byte integer type
!   LandIdx               - Array of IGBP scene type indicies that correspond 
!                           to the ERBE land geographic scene type
!   LiqPhase              - Parameter indicating liquid phase for a cloud layer
!   MATCHname_6hr         - Character string within the six-hourly MATCH input 
!                           file name
!   Match_Vert_Available  - Flag indicating vertical MATCH aerosols are available
!   Match_Vert_Unavailable- Flag indicating vertical MATCH aerosols are unavailable
!   MaxAdjust_Aer         - Maximum allowable adjustment to the aerosol optical
!                           depth value
!   MaxAerOptDepth        - Maximum total aerosol optical depth value for 
!                           input to the Fu-Liou radiation transfer model
!   MaxIce_size           - Maximum ice particle size that can be input to the 
!                           Fu-Liou model 
!   MaxLiq_Phase          - Maximum cloud layer phase value that can be 
!                           considered liquid for input to the Fu-Liou 
!                           radiation transfer model
!   MaxLiq_size           - Maximum liquid particle size that can be input 
!                           to the Fu-Liou model 
!   MinAerOptDepth        - Minimum total aerosol optical depth value for 
!                           input to the Fu-Liou radiation transfer model
!   MinCoast              - The minimum value of the summed ERBE coast scene
!                           type fractions necessary to classify the entire
!                           CERES FOV as ERBE coast
!   MinDesert             - The minimum value of the summed ERBE desert scene
!                           type fractions necessary to classify the entire
!                           CERES FOV as ERBE desert
!   MinLand               - The minimum value of the summed ERBE land scene
!                           type fractions necessary to classify the entire
!                           CERES FOV as ERBE land
!   MinIce_size           - Minimum ice particle size that can be input to the 
!                           Fu-Liou model
!   MinLiq_size           - Minimum liquid particle size that can be input to
!                           the Fu-Liou model
!   MinOcean              - The minimum value of the summed ERBE ocean scene
!                           type fractions necessary to classify the entire
!                           CERES FOV as ERBE ocean
!   MinSnow               - The minimum value of the summed ERBE snow scene
!                           type fractions necessary to classify the entire
!                           CERES FOV as ERBE snow
!   Nighttime             - Parameter indicating current FOV is nighttime
!                            (Solar zenith angle > to 90 degrees)
!   NumAerConst_Src       - Array containing the number of possible  
!                           scene-type constituencies for each of the  
!                           aerosol climatologies that could be used
!   NumAerSrc             - Number of possible sources for aerosol optical 
!                           depth data 
!   NumAerWL_Terra        - Number of Terra MODIS wavelengths with aerosol
!                           optical depth data
!   NumDesertTyp          - Number of IGBP scene types that correspond to the
!                           ERBE desert scene type
!   NumLandTyp            - Number of IGBP scene types that correspond to the
!                           ERBE land scene type
!   NumOceanTyp           - Number of IGBP scene types that correspond to the
!                           ERBE ocean scene type
!   NumSnowTyp            - Number of IGBP scene types that correspond to the
!                           ERBE snow scene type
!   OceanIdx              - IGBP scene type index that corresponds to the ERBE
!                           ocean geographic scene type
!   RL2                   - Parameter indicating two-byte real type
!   SceneNumCERES         - Maximum number of CERES scene type fractions 
!                           included on the primary input product
!   SceneNumERBE          - Number of ERBE geographic scene types
!   SfcAlbSrc_HuCoxMunk   - Parameter for indicating that the source of the
!                           initial surface albedo data input to the radiation 
!                           transfer model for the current FOV is the HuCoxMunk
!                           algorithm
!   SfcAlbSrc_PrimInput   - Parameter for indicating that the source of the
!                           initial surface albedo data input to the radiation 
!                           transfer model for the current FOV is the primary
!                           input product
!   SfcAlbSrc_IGBP        - Parameter for indicating that the source of the
!                           initial surface albedo data input to the radiation 
!                           transfer model for the current FOV is the IGBP
!                           default value stored on the Monthly Surface Albedo 
!                           History map
!   SfcAlbSrc_SSF         - Parameter for indicating that the source of the
!                           initial surface albedo data input to the radiation 
!                           transfer model for the current FOV is the SSF-based
!                           value stored on the Monthly Surface Albedo 
!                           History map
!   SfcAlbSrc_ZJin        - Parameter for indicating that the source of the
!                           initial surface albedo data input to the radiation 
!                           transfer model for the current FOV is the Zhonghai 
!                           Jin algorithm 
!   SnowIdx               - Array of IGBP scene type indicies that correspond 
!                           to the ERBE snow geographic scene type
!   SunGlint_Max          - Maximum allowable percentage of sun glint within 
!                           a FOV for that FOV to be processed through the 
!                           SARB algorithms
!   Tol_FracAtm           -
!   UTH_LowerBound        - Lower altitude limit (hPa) of the upper troposphere
!                           for purposes of calculating the upper tropospheric 
!                           relative humidity
!   UTH_UpperBound        - Upper altitude limit (hPa) of the upper troposphere 
!                           for purposes of calculating the upper tropospheric 
!                           relative humidity
!   ValOut_OFF            - Flag value indicating that current run of subsystem
!                           is not in subset mode
!   ValOut_ON             - Flag value indicating that current run of subsystem
!                           is in subset mode
!   ValReg_NO             - Flag value indicating that current FOV does not 
!                           correspond to a CERES Validation Region
!   ValReg_YES            - Flag value indicating that current FOV does 
!                           correspond to a CERES Validation Region
!
!  Modules Used: 
!    ceres_constants - (CERESlib) Params: FM1, FM2, FM3, FM4, FM5, PFM,
!                                         SOLAR_CONST
!    f90_kind        - (CERESlib) Params: INT2, REAL8
!
! !Revision History:
!
!  Version 4.0  October 12, 2004
!               Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!               1. Changed AerConst_Angexp ( 18) to AerConst_Angexp ( 25)
!               2. Added necessary values for initilization 
!  Version 4.0  June 17, 2004 - SCCR #541 
!               Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!               1. Addition of parameters: CERSYN, Match_Vert_Available, Match_Vert_UnAvailable
!               2. Changed value of CentWL_63 from 0.63 to 0.55
!  Version 3.6   January 16, 2004 - SCCR #498 
!               Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Addition of parameters: MaxIce_size, MaxLiq_size
!  Version 3.5  April 22, 2003 - SCCR #437
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Addition of parameters: RL8
!  Version 3.4  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Addition of parameters moved from Mod. SARB_Var:
!                    AerRatio_Init      MaxAdjust_Aer     ValOut_OFF    
!                    AerSrc_PrimInput   Nighttime         ValOut_ON
!                    Chan11             Tol_FracAtm       ValReg_NO
!                    ClrIdx             UTH_LowerBound    ValReg_YES
!                    Daytime            UTH_UpperBound   
!                    GrDailyAvail_NO
!                    GrDailyAvail_YES                       
!                2. Addition of parameter SolCon, defined equal to CERESlib
!                   Mod. ceres_constants parameter SOLAR_CONST.
!                3. Addition of:
!                    AerSrc_ClimOnly
!                    AerSrc_GridTerraMODIS
!                    AerSrc_FOVTerraMODIS
!                    AerSrc_StoweFOV
!                    CERES_FM1
!                    CERES_FM2
!                    CERES_FM3
!                    CERES_FM4
!                    CERES_FM5
!                    CERES_PFM
!                    MATCHname_6hr
!                    NumAerWL_Terra
!  Version 3.3  April 19, 2001 - SCCR #337
!               Thomas Caldwell, SAIC, t.e.caldwell@nasa.larc.gov
!                 1. Changed vlaue of SunGlint_Max from 0 to 90
!  Version 3.2  December 24, 2001 - SCCR #315  
!               Thomas Caldwell, SAIC, t.e.caldwell@nasa.larc.gov
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Changed OceanIdx from an array to a scalar
!                2. Addition of:
!                    AerConst_Angexp, MaxLiq_Phase, SfcAlbSrc_ZJin
!                3. Removal of:
!                    NightIce_size   NightLiq_size   NightOptDepth
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Addition of:
!                    AerSrc_Collins        NumAerConst_Src
!                    AerSrc_GFDL           NumAerSrc
!                    AerSrc_GFDL           SfcAlbSrc_HuCoxMunk
!                    AerSrc_PrimInput      SfcAlbSrc_PrimInput
!                    Collins_Available     SfcAlbSrc_IGBP
!                    Collins_Unavailable   SfcAlbSrc_SSF
!                    MinIce_size
!  Version 2.4  March 15, 1999 - SCCR #132
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!*****************************************************************************
!
! 
    USE ceres_constants, ONLY: FM1, FM2, FM3, FM4, FM5, FM6, PFM, SOLAR_CONST
    USE f90_kind, ONLY: INT1, INT2, REAL4, REAL8
!
    IMPLICIT NONE
!
!
!
      INTEGER,            PARAMETER :: NumAerSrc = 6
      INTEGER,            PARAMETER :: NumDesertTyp = 2
      INTEGER,            PARAMETER :: NumLandTyp   = 14
      INTEGER,            PARAMETER :: NumOceanTyp  = 1
      INTEGER,            PARAMETER :: NumSnowTyp   = 3
!
      INTEGER,            PARAMETER :: Aer_Continental = 1
      REAL,               PARAMETER :: AerConst_Angexp ( 25) = (/ 0.5, 2.0, 2.0, &
                                                                  1.0, 0.5, 0.5, &
                                                                  0.5, 0.5, 0.5, &
                                                                  2.0, 2.0, 0.5, &
                                                                  0.5, 2.0, 0.5, &
                                                                  0.5, 0.5, 2.0, &
                                                                  1.0, 0.5, 0.5, &
                                                                  0.5, 0.5, 1.0, 0.5/)
                                   
      INTEGER,            PARAMETER :: Aer_Maritime = 2
      REAL,               PARAMETER :: AerRatio_Init = 1.0
      INTEGER,            PARAMETER :: Aer_Urban = 3
      INTEGER,            PARAMETER :: AerSrc_ClimOnly = 0
      INTEGER,            PARAMETER :: AerSrc_Collins = 2
      INTEGER,            PARAMETER :: AerSrc_CollinsBackup = 6
      INTEGER,            PARAMETER :: AerSrc_GFDL = 3
      INTEGER,            PARAMETER :: AerSrc_GridTerraMODIS = 5
      INTEGER,            PARAMETER :: AerSrc_FOVTerraMODIS = 4
      INTEGER,            PARAMETER :: AerSrc_StoweFOV = 1
      REAL,               PARAMETER :: CentWL_63 = 0.55
      INTEGER,            PARAMETER :: CERCRS = 1
      INTEGER,            PARAMETER :: CERCRSDG = 2
      INTEGER,            PARAMETER :: CERSYN = 3
      INTEGER,            PARAMETER :: CERES_FM1 = FM1
      INTEGER,            PARAMETER :: CERES_FM2 = FM2
      INTEGER,            PARAMETER :: CERES_FM3 = FM3
      INTEGER,            PARAMETER :: CERES_FM4 = FM4
      INTEGER,            PARAMETER :: CERES_FM5 = FM5
      INTEGER,            PARAMETER :: CERES_FM6 = FM6
      INTEGER,            PARAMETER :: CERES_PFM = PFM
      INTEGER,            PARAMETER :: Chan11 = 11
      INTEGER,            PARAMETER :: ClrIdx = 0
      INTEGER,            PARAMETER :: Collins_Available = 1
      INTEGER,            PARAMETER :: Collins_Unavailable = 0
      REAL,               PARAMETER :: COSDayNite_Terminator = 0.0
      INTEGER,            PARAMETER :: Daytime = 1
      INTEGER,            PARAMETER :: DesertIdx ( NumDesertTyp) = (/ 7, 16/)
      INTEGER,            PARAMETER :: ERBECoast   = 5
      INTEGER,            PARAMETER :: ERBEDesert  = 4
      INTEGER,            PARAMETER :: ERBELand    = 2
      INTEGER,            PARAMETER :: ERBEOcean   = 1
      INTEGER,            PARAMETER :: ERBESnow    = 3
      INTEGER,            PARAMETER :: ERBEUnknown = 12
      INTEGER,            PARAMETER :: GrDailyAvail_NO = 0
      INTEGER,            PARAMETER :: GrDailyAvail_YES = 1
      REAL,               PARAMETER :: IcePhase = 2.0
      INTEGER,            PARAMETER :: IN1 = INT1
      INTEGER,            PARAMETER :: IN2 = INT2
      INTEGER,            PARAMETER :: LandIdx ( NumLandTyp) =               &
                                                 (/ 1, 2, 3, 4, 5, 6, 8, 9,  &
                                                   10, 11, 12, 13, 14, 18 /)
      REAL,               PARAMETER :: LiqPhase = 1.0
      CHARACTER (LEN=15), PARAMETER :: MATCHname_6hr = "1998TRMM_Ver3.0"
      INTEGER,            PARAMETER :: Match_Vert_Available = 1
      INTEGER,            PARAMETER :: Match_Vert_Unavailable = 0
      REAL,               PARAMETER :: MaxAdjust_Aer = 2.0
      REAL,               PARAMETER :: MaxAerOptDepth = 10.0
      REAL,               PARAMETER :: MaxIce_size = 300.0
      REAL,               PARAMETER :: MaxLiq_Phase = 1.5
      REAL,               PARAMETER :: MaxLiq_size = 30.0
      REAL,               PARAMETER :: MinAerOptDepth = 0.0
      REAL,               PARAMETER :: MinCoast  = 67.0
      REAL,               PARAMETER :: MinDesert = 50.0
      REAL,               PARAMETER :: MinLand   = 67.0
      REAL,               PARAMETER :: MinIce_size = 20.0
      REAL,               PARAMETER :: MinLiq_size = 4.0
      REAL,               PARAMETER :: MinOcean  = 67.0
      REAL,               PARAMETER :: MinSnow   = 50.0
      INTEGER,            PARAMETER :: NumAerConst_Src ( NumAerSrc) =      &
                                                         (/ 7, 7, 5, 7, 7, 7 /)
      INTEGER,            PARAMETER :: NumAerWL_Terra = 7
      INTEGER,            PARAMETER :: Nighttime = 2
      INTEGER,            PARAMETER :: OceanIdx = 17
      INTEGER,            PARAMETER :: RL4 = REAL4
      INTEGER,            PARAMETER :: RL8 = REAL8
      INTEGER,            PARAMETER :: SceneNumCERES = 20
      INTEGER,            PARAMETER :: SceneNumERBE  = 5
      INTEGER,            PARAMETER :: SfcAlbSrc_HuCoxMunk = 3
      INTEGER,            PARAMETER :: SfcAlbSrc_PrimInput = 2
      INTEGER,            PARAMETER :: SfcAlbSrc_IGBP = 1
      INTEGER,            PARAMETER :: SfcAlbSrc_SSF = 0
      INTEGER,            PARAMETER :: SfcAlbSrc_ZJin = 4
      INTEGER,            PARAMETER :: SnowIdx ( NumSnowTyp) = (/ 15, 19, 20 /)
!      REAL,               PARAMETER :: SolCon = SOLAR_CONST
      INTEGER (INT2),     PARAMETER :: SunGlint_Max = 90
      REAL,               PARAMETER :: Tol_FracAtm = 0.01
      REAL,               PARAMETER :: UTH_LowerBound = 500.0
      REAL,               PARAMETER :: UTH_UpperBound = 200.0
      INTEGER,            PARAMETER :: ValOut_OFF = 0
      INTEGER,            PARAMETER :: ValOut_ON = 1
      INTEGER,            PARAMETER :: ValReg_NO = 0
      INTEGER,            PARAMETER :: ValReg_YES = 1
!
!
END MODULE SARBInput_Params
