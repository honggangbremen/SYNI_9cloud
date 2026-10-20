MODULE MonMODISAer_Params 
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   MonMODISAer_Params
!
! !Description:
!   Routine ID -
!
!   Purpose - A module of type declarations for parameters used throughout
!             the Instantaneous SARB Subsystem Aerosol History Pre-Processor
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
!    Aerosol_Header   - Header type to be written to monthly MODIS aerosol file
!    idat             - Array containing the daily gridded MODIS aerosol data
!    ilnd             - Array of land aerosol data for a region
!    IMonth           - Current data month of the year
!    InLn_AX          - idat array index to the land angstrom exponent 
!                       [0.47 and 0.66] 
!    InLn_Rfl047      - idat array index to the mean reflectance land all QA66
!                       mean average of reflectance after cloud screening at
!                       0.47um using QA for 0.66: Mean
!    InLn_Rfl055      - idat array index to the mean reflectance land all QA66
!                       mean average of reflectance after cloud screening at
!                       0.55um using QA for 0.66: Mean
!    InLn_Rfl210      - idat array index to the mean reflectance land all QA66
!                       mean average of reflectance after cloud screening at
!                       2.10um using QA for 0.66: Mean
!    InLnOc_055       - idat array index to the optical depth land and ocean
!                       mean 0.55
!    InOc_AX1         - idat array index to the angstrom exponent #1 
!                       [0.550 and 0.865] 
!    InOc_AX2         - idat array index to the angstrom exponent #2
!                       [0.865 and 2.130]
!    InOc_047         - idat array index to the effective optical depth average  
!                       ocean mean for wavelength = 0.47
!    InOc_055         - idat array index to the effective optical depth average 
!                       ocean mean for wavelength = 0.55
!    InOc_066         - idat array index to the effective optical depth average 
!                       ocean mean for wavelength = 0.66
!    InOc_0865        - idat array index to the effective optical depth average  
!                       ocean mean for wavelength = 0.865
!    InOc_124         - idat array index to the effective optical depth average 
!                       ocean mean for wavelength = 1.24
!    InOc_164         - idat array index to the effective optical depth average 
!                       ocean mean for wavelength = 1.64
!    InOc_213         - idat array index to the effective optical depth average 
!                       ocean mean for wavelength = 2.13
!    isea             - Array of sea aerosol data for a region
!    IUnit_GrMODIS    - ............
!    IUnit_MonGridAer - Monthly aerosol history file unit number
!    IYear            - Current data year
!    I2_Dflt          - Default value for 2-byte integers
!    I4_Dflt          - Default value for 4-byte integers
!    LID_GrMODIS      - PCF LID for the daily gridded MODIS aerosol input files
!    LID_MonGridAer   - PCF LID for the monthly aerosol history file
!    LID_Month        - PCF LID for the month parameter
!    LID_VerMODIS     - PCF LID for MODIS file version number
!    LID_Year         - PCF LID for the year parameter
!    LndSeaMode_Dflt  - Default value for land/sea mode flag
!    LndSeaMode_Land  - Value indicating land for land/sea mode flag
!    LndSeaMode_Sea   - Value indicating sea for land/sea mode flag
!    MaxDay           - Maximum number of days possible in a month
!    MBlock           - Array of subset aerosol data accumulated over a month 
!                       for each region
!                        MBlock ( I, J, K, L), where:
!                          I = Day of month
!                          J = Number of subset parameters
!                          K = Longitudinal index
!                          L = Latitudinal index
!    MODIS_Dflt       - Default value for MODIS aerosol maps
!    nin              - Number of wavelengths at which the input aerosol data
!                       are provided
!    naerout          - Number of wavelengths at which SARB code requires 
!                       aerosol data
!    NDay             - Number of days in current data month
!    NLnd             - Array of the number of daily subset samples over land
!    nout             - Dimension for arrays associated with subseted data
!    NSea             - Array of the number of daily subset samples over ocean
!    NTot             - Array of the total number of daily subset samples
!    NumMODIS         - Number of MODIS parameters for a region
!    R2_Dflt          - Default value for 2-byte reals
!    R4_Dflt          - Default value for 4-byte reals
!    SbLn_AX          - ilnd array index to the land angstrom exponent  
!                       [0.47 and 0.66]
!    SbLn_Rfl047      - ilnd array index to the mean reflectance land all QA66
!                       mean average of reflectance after cloud screening at
!                       0.47 um using QA for 0.66: Mean
!    SbLn_Rfl055      - ilnd array index to the mean reflectance land all QA66
!                       mean average of reflectance after cloud screening at
!                       0.55um using QA for 0.66: Mean
!    SbLn_Rfl210      - ilnd array index to the mean reflectance land all QA66
!                       mean average of reflectance after cloud screening at
!                       2.10um using QA for 0.66: Mean
!    SbLnOc_055       - ilnd array index to the optical depth land and ocean
!                       mean 0.55
!    SbOc_AX1         - isea array index to the angstrom exponent #1 
!                       [0.550 and 0.865]
!    SbOc_AX2         - isea array index to the angstrom exponent #2
!                       [0.865 and 2.130]
!    SbOc_047         - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 0.47
!    SbOc_055         - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 0.55
!    SbOc_066         - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 0.66
!    SbOc_0865        - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 0.865
!    SbOc_124         - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 1.24
!    SbOc_164         - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 1.64
!    SbOc_213         - isea array index to the effective optical depth average 
!                       ocean mean for wavelength = 2.13
!    wlin             - Array of wavelengths at which the gridded MODIS aerosol
!                       data are input
!    wlout            - Array of wavelengths to which the gridded MODIS aerosol
!                       data are normalized for SARB processing
!    ZBlock           - Array of subset aerosol data for the current day 
!                       for each region
!                        ZBlock ( I, J, K), where:
!                          I = Number of subset parameters
!                          J = Longitudinal index
!                          K = Latitudinal index
!
!  Modules Used:
!    ceres_defaults - (CERESlib) Params: INT2_DFLT, INT4_DFLT, REAL4_DFLT
!    f90_kind       - (CERESlib) Params: INT2
!
! !Revision History:
!    Version 3.6   January 16, 2004 - SCCR #498 
!                  Tom Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   Removed parameters for land optical depths, land aerosol
!    constituents, and logic flag for MODIS land aerosol format. 
!     Added parameters for use of land angstrom exponent.
!    Version 3.5   R4 Date - SCCR# nnn
!                  Tom Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   Added parameters for MODIS file version number and
!    logic flags for MODIS land aerosol format.  Added 
!       header type for monthly aerosol map.
!    Version 3.4   R4 Date - SCCR# nn
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Tom Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      USE ceres_defaults, ONLY: INT2_DFLT, INT4_DFLT, REAL4_DFLT
      USE f90_kind, ONLY: INT2
!
! *** Constants
!
      INTEGER (INT2), PARAMETER :: I2_Dflt = INT2_DFLT
      INTEGER,        PARAMETER :: I4_Dflt = INT4_DFLT
      INTEGER,        PARAMETER :: LID_GrMODIS = 36
      INTEGER,        PARAMETER :: LID_VerMODIS = 9996
      INTEGER,        PARAMETER :: LID_MCF_MODISAer = 290
      INTEGER,        PARAMETER :: LID_META_MODISAer = 13501
      INTEGER,        PARAMETER :: LID_MonGridAer = 135
      INTEGER,        PARAMETER :: LID_Month = 145
      INTEGER,        PARAMETER :: LID_Year = 144
      INTEGER (INT2), PARAMETER :: LndSeaMode_Land = 2
      INTEGER (INT2), PARAMETER :: LndSeaMode_Sea = 1
      INTEGER,        PARAMETER :: MaxDay = 31
      INTEGER (INT2), PARAMETER :: MODIS_Dflt = -9999
      INTEGER,        PARAMETER :: naerout = 7
      INTEGER,        PARAMETER :: nin = 7
      INTEGER,        PARAMETER :: NLat = 180
      INTEGER,        PARAMETER :: NLong = 360
      INTEGER,        PARAMETER :: NOut = 16
      INTEGER,        PARAMETER :: NumMODIS = 24
      REAL,           PARAMETER :: R2_Dflt = 32767.0
      REAL,           PARAMETER :: R4_Dflt = REAL4_DFLT
      REAL,           PARAMETER :: wlin ( nin) =                            &
                                  (/ 0.47, 0.55, 0.66, 0.865, 1.24, 1.64, 2.13/)
      REAL,           PARAMETER :: wlout ( naerout) =                       &
                                  (/ 0.47, 0.55, 0.66, 0.865, 1.24, 1.64, 2.13/)
!
! *** Subset array pointers
!
      INTEGER, PARAMETER :: InLn_AX     =  2
      INTEGER, PARAMETER :: InLn_Rfl047 = 22
      INTEGER, PARAMETER :: InLn_Rfl055 = 23
      INTEGER, PARAMETER :: InLn_Rfl066 = 24
      INTEGER, PARAMETER :: InLnOc_055  =  1
      INTEGER, PARAMETER :: InLn_047    =  2
      INTEGER, PARAMETER :: InLn_066    =  3
      INTEGER, PARAMETER :: InOc_AX1    = 20
      INTEGER, PARAMETER :: InOC_AX2    = 21
      INTEGER, PARAMETER :: InOc_047    = 13 
      INTEGER, PARAMETER :: InOc_055    = 14 
      INTEGER, PARAMETER :: InOc_066    = 15
      INTEGER, PARAMETER :: InOc_0865   = 16
      INTEGER, PARAMETER :: InOc_124    = 17
      INTEGER, PARAMETER :: InOc_164    = 18
      INTEGER, PARAMETER :: InOc_213    = 19
      INTEGER, PARAMETER :: SbLn_AX     =  3
      INTEGER, PARAMETER :: SbLn_Rfl047 = 14
      INTEGER, PARAMETER :: SbLn_Rfl055 = 15
      INTEGER, PARAMETER :: SbLn_Rfl066 = 16
      INTEGER, PARAMETER :: SbLn_047    =  3
      INTEGER, PARAMETER :: SbLnOc_055  =  2
      INTEGER, PARAMETER :: SbLn_066    =  4
      INTEGER, PARAMETER :: SbOc_AX1    =  9
      INTEGER, PARAMETER :: SbOc_AX2    = 10
      INTEGER, PARAMETER :: SbOc_047    =  2
      INTEGER, PARAMETER :: SbOc_055    =  3
      INTEGER, PARAMETER :: SbOc_066    =  4
      INTEGER, PARAMETER :: SbOc_0865   =  5
      INTEGER, PARAMETER :: SbOc_124    =  6
      INTEGER, PARAMETER :: SbOc_164    =  7
      INTEGER, PARAMETER :: SbOc_213    =  8
!
!
! *** Variables
!
      INTEGER (INT2), SAVE :: idat (NLong, NLat, NumMODIS)
      INTEGER (INT2), SAVE :: ilnd ( NOut)
      INTEGER,        SAVE :: IMonth
      INTEGER (INT2), SAVE :: isea ( NOut)
      INTEGER,        SAVE :: IUnit_GrMODIS
      INTEGER,        SAVE :: IUnit_MonGridAer 
      INTEGER,        SAVE :: IYear                                           
!      LOGICAL,        SAVE :: Land_Aer_Flag
      INTEGER (INT2), SAVE :: MBlock ( MaxDay, NOut, NLong, NLat)
      INTEGER,        SAVE :: NDay
      INTEGER,        SAVE :: NLnd ( MaxDay)
      INTEGER,        SAVE :: NSea ( MaxDay)
      INTEGER,        SAVE :: NTot ( MaxDay)
      REAL,           SAVE :: rlat0 ( NLong, NLat)
      REAL,           SAVE :: rlon0 ( NLong, NLat)
      INTEGER (INT2), SAVE :: ZBlock ( NOut, NLong, NLat)
!
      TYPE Aerosol_Header
         SEQUENCE
            INTEGER (KIND=int2) :: MODIS_ver
            CHARACTER (LEN=19)  :: Prod_date
      END TYPE Aerosol_Header
!
      TYPE (Aerosol_Header), SAVE :: AerHeader
!
!
END MODULE MonMODISAer_Params
