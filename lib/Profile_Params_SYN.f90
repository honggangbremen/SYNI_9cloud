MODULE Profile_Params
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    Profile_Params
!
! !Description:
!
!  Routine ID -
!
!  Purpose - Provides type declarations and values for constants used 
!            throughout the CERES Synoptic SARB Subsystem.
!
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NONE
!
! !Team Unique Header:
!
!  Key Local Parameters:
!    Cld_Id1            - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the first cloud
!                         layer contained on the primary input product
!    Cld_Id2            - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the second cloud
!                         layer contained on the primary input product
!    Cld_Id3            - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the third cloud
!                         layer contained on the primary input product
!    Cld_Id4            - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the fourth cloud
!                         layer contained on the primary input product
!    CldHt_SfcLimit     - Minimun allowable height (hPa) above surface for
!                         a cloud
!    Cldlyr_Aer         - Array used for mapping cloud layers with aerosols
!    Cldlyr_NoAer       - Array used for mapping cloud layers without aerosols
!    Cloud_Inconsist    - Code for failures due to cloud inconsistancies
!    ClrSky             - Index into the modeled vertical profile arrays
!                         for the clear-sky profiles
!    ConstrPass         - Index into the modeled vertical profile arrays
!                         for the profiles produced by the constrained 
!                         pass through the radiation transfer model
!    CS_NoAer           - Index into the modeled vertical profile arrays
!                         for the cloudy sky with no aerosol profiles
!    Delta_CldPro       - Maximum difference (in hPa) between a cloud top  
!                         pressure and a fixed level pressure for the cloud 
!                         top pressure to not be considered a new level in
!                         the vertical profile
!    DnDir              - Index into the modeled vertical profile arrays
!                         for the downward profiles
!    hPa70_Idx          - Index into the flux profile arrays to the value 
!                         corresponding to the 70 hPa level
!    hPa200_Idx         - Index into the flux profile arrays to the value 
!                         corresponding to the 200 hPa level
!    hPa500_Idx         - Index into the flux profile arrays to the value 
!                         corresponding to the 500 hPa level
!    InitHt             - Initial value for height profile array elements
!    InitPass           - Index into the modeled vertical profile arrays
!                         for the profiles produced by the initial pass
!                         through the radiation transfer model
!    Input_File         - Name of main input file to model
!    Invalid_Cld_Props  - Code for failures due to bad cloud properties
!    Invalid_SSF_Data   - Code for failures due to bad ssf data
!    Invalid_SunGlint   - Code for failures due to bad sun glint percentages
!    Invalid_TOA_Flux   - Code for failures due to bad TOA fluxes
!    MaxLev_Cld         - Maximum number of cloud layers that could be  
!                         contained on the primary input product
!    MaxLev_Cld_2x      - Maximum number of cloud levels that can be inserted
!                         into the SARB profile
!                         2 * MaxLev_Cld )
!    MaxLev_SARB        - Maximum number of levels in a SARB profile
!                         MaxLev_Cld + NumLev_Fix)
!    NoAer_Cld1         - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the first cloud
!                         layer contained on the primary input product but
!                         assuming no aerosol data
!    NoAer_Cld2         - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the second cloud
!                         layer contained on the primary input product but
!                         assuming no aerosol data
!    NoAer_Cld3         - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the third cloud
!                         layer contained on the primary input product but
!                         assuming no aerosol data
!    NoAer_Cld4         - Index into the modeled vertical profile arrays
!                         for the profiles calculated for the fourth cloud
!                         layer contained on the primary input product but
!                         assuming no aerosol data
!    Num_BndSA          - Number of surface spectral albedo bands
!    Num_BndSfcEm       - Number of surface emissivity bands
!    Num_SfcTyp         - Number of possible surface types available from the 
!                         primary input product
!    NumDir             - Number of flux directions, such as upward and  
!                         downward, for which data are stored in the 
!                         vertical flux profiles
!    NumLev_Delta       - Number of levels in the arrays of flux deltas
!    NumLev_Fix         - Number of fixed levels in the SARB profile (includes
!                         number of floating levels)
!    NumLev_Float       - Number of floating levels above the surface to be 
!                         included in the SARB profile
!    NumLev_Start       - Maximum number of height levels in the vertical  
!                         profile before the heights of the clouds are added
!    NumLevArc          - Number of height levels from the vertical profiles
!                         that are archived on the primary output product
!    NumPass            - Number of passes through the radiation 
!                         transfer model allowed
!    NumSkyCond         - Number of cloud conditions, such as clear sky 
!                         and total sky, for which vertical flux profiles 
!                         are archived on the primary output product
!    NumSkyCond_NewFu   - Array used for mapping cloud conditions from Fu model
!                         variables, such as clear sky and total sky, 
!                         for which vertical flux profiles 
!                         are archived on the primary output product
!    Pres_Pnt ( I)      - Array of pointers to the elements of the MOA  
!                         pressure profile that will be used by SARB
!    Pres_Act ( I)      - Array of actual heights (hPa) of output levels
!    Pres_Out ( I)      - Array of requested heights (hPa) of output levels
!    PrsSky             - Index into the modeled vertical profile arrays
!                         for the pristine sky profiles
!    SfcDelta_Idx       - Index into flux delta arrays for the surface level
!    TOADelta_Idx       - Index into flux delta arrays for the TOA level
!    TOALev_Idx         - Index into the modeled vertical profile arrays  
!                         for the TOA level
!    TotSky             - Index into the modeled vertical profile arrays
!                         for the total sky profiles
!    CS_NoAer           - Index into the modeled vertical profile arrays
!                         for the cloudy sky with no aerosol profiles
!    UpDir              - Index into the modeled vertical profile arrays
!                         for the upward profiles
!
! !Revision History:
!  Version 4.0   August 16, 2004 - SCCR #550
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Addition of cloud layer parameter for mapping Fu
!                     outputs into SYN variables
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Initial version of Release 3 code
!  Version 1.0   8/21/95   
!                Lisa Coleman, LESC
!                Initial version of code
!
! !END
!
!*****************************************************************************
!
!
      REAL,    PARAMETER :: CldHt_SfcLimit = 20.0
      REAL,    PARAMETER :: Delta_CldPro = 2.0
      REAL,    PARAMETER :: InitHt = 1100.0
      INTEGER, PARAMETER :: Num_BndSfcEm = 12
      INTEGER, PARAMETER :: Num_BndSA = 18    !!!! 15 Ed2 to Ed3 Fu
      INTEGER, PARAMETER :: Num_SfcTyp = 8
!
! ****************************
!     Runtime Error codes
! ****************************
!
      INTEGER, PARAMETER :: Invalid_TSI_Data = 3010
      INTEGER, PARAMETER :: Cloud_Inconsist = 3020
      INTEGER, PARAMETER :: Invalid_Cld_Props = 3030
      INTEGER, PARAMETER :: Invalid_TOA_Flux = 3040
      INTEGER, PARAMETER :: Invalid_SunGlint = 3050
!  
! ****************************
! Vertical Profile Information
! ****************************
!
      INTEGER, PARAMETER :: MaxLev_Cld = 9 !4 HG-20250731
      INTEGER, PARAMETER :: MaxLev_Cld_2x = 18 !8 HG-2025-0731
      !INTEGER, PARAMETER :: MaxLev_SARB = 36
      INTEGER, PARAMETER :: MaxLev_SARB = 50 !HG-20260922, change from 36 to 50
      INTEGER, PARAMETER :: NumLev_Fix = 23
      INTEGER, PARAMETER :: NumLev_Float = 3
      INTEGER, PARAMETER :: NumLev_Start = 27 
      INTEGER, PARAMETER :: NumLev_Delta = 2
!
      INTEGER, PARAMETER :: Pres_Pnt ( NumLev_Fix + NumLev_Float) =                     &
     (/ 58, 50, 48, 44, 43, 39, 35, 33, 31, 29, 28, 27, 25,                  &
        23, 22, 20, 18, 16, 14, 12, 10,  8,  6,  3,  2,  1 /)
!
      INTEGER, PARAMETER :: NumLev_Arc = 5
      INTEGER, PARAMETER :: TOALev_Idx = 1
      INTEGER, PARAMETER :: hPa70_Idx = 2
      INTEGER, PARAMETER :: hPa200_Idx = 3
      INTEGER, PARAMETER :: hPa500_Idx = 4
      INTEGER, PARAMETER :: SfcDelta_Idx = 2
      INTEGER, PARAMETER :: TOADelta_Idx = 1
!
      REAL,    SAVE      :: Pres_Act ( NumLev_Arc)
      REAL,    SAVE      :: Pres_Out ( NumLev_Arc)
! 
!
! ************************
! Flux Directional Indexes
! ************************
!
      INTEGER, PARAMETER :: NumDir = 2
      INTEGER, PARAMETER :: DnDir = 1
      INTEGER, PARAMETER :: UpDir = 2
!
!
! **********************************************************
! Sky Condition Indexes for Fu-Liou flux and radiance output
! **********************************************************
!
      INTEGER, PARAMETER :: NumSkyCond_NewFu (12) = &
      (/ 3, 1, 2, 0, 0, 0, 0, 4, 0, 0, 0, 0/) 
      INTEGER, PARAMETER :: NumSkyCond = 12                  
      INTEGER, PARAMETER :: PrsSky = 1
      INTEGER, PARAMETER :: ClrSky = 2
      INTEGER, PARAMETER :: TotSky = 3
      INTEGER, PARAMETER :: Cld_Id1 = 4
      INTEGER, PARAMETER :: Cld_Id2 = 5
      INTEGER, PARAMETER :: Cld_Id3 = 6
      INTEGER, PARAMETER :: Cld_Id4 = 7
      INTEGER, PARAMETER :: CS_NoAer = 8
      INTEGER, PARAMETER :: NoAer_Cld1 = 9
      INTEGER, PARAMETER :: NoAer_Cld2 = 10
      INTEGER, PARAMETER :: NoAer_Cld3 = 11
      INTEGER, PARAMETER :: NoAer_Cld4 = 12
!
      INTEGER, PARAMETER :: Cldlyr_Aer(7) = (/0,0,0,1,2,3,4/)
      INTEGER, PARAMETER :: Cldlyr_NoAer(12) = (/0,0,0,0,0,0,0,0,1,2,3,4/)
!
!
! ******************
! Model Pass Indexes
! ******************
!
      INTEGER, PARAMETER :: NumPass = 2
      INTEGER, PARAMETER :: MetIn = 0
      INTEGER, PARAMETER :: InitPass = 1
      INTEGER, PARAMETER :: ConstrPass1 = 2
!
!
! ************************************
! Subsystem-Dependent QC Report Labels
! ************************************
!
      CHARACTER (LEN=7),  PARAMETER :: HRes_Sing_Left  = 'REGION '
      CHARACTER (LEN=8),  PARAMETER :: HRes_Mult_Left  = 'REGIONS '
      CHARACTER (LEN=7),  PARAMETER :: HRes_Sing_Right = ' REGION'
      CHARACTER (LEN=8),  PARAMETER :: HRes_Mult_Right = ' REGIONS'
      CHARACTER (LEN=13), PARAMETER :: QCHead_SSID = 'Synoptic SARB'
      CHARACTER (LEN=3),  PARAMETER :: Input_File = 'TSI'
!
!
END MODULE Profile_Params
