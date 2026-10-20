MODULE SARBInput_Utils
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    SARBInput_Utils
!
! !Description:
!    Routine ID - MG5
!
!    Purpose - To provide a module of routines to verify and prepare input 
!              data for SARB use
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
!  Modules Used:
!    FL_IO_Params         - (SARBlib)  Used by Sbr. CldPropCheck, TOA_Alb_Calc, 
!                                                   TOAInput_Process, 
!                                                   TopHt_Check
!    MOA_IO               - (CERESlib) Used by Sbr. MOAInput_Process
!    Profile_Params       - (SARBlib)  Used by Sbr. CldPropCheck, PhaseCheck,
!                                                   TopHt_Check
!    SARBInput_Params     - (SARBlib)  Used by Sbr. CERESScene_to_ERBEScene,
!                                                   CERESScene_ERBEScene_INT, 
!                                                   CERESScene_ERBEScene_REAL, 
!                                                   CldPropCheck,
!                                                   DayNite_Define, PhaseCheck,
!                                                   TOAInput_Process 
!    SARB_OutVar          - (SARBlib)  Used by Sbr. MOAInput_Process, 
!                                                   TOAInput_Process
!    SARB_Var             - (SARBlib)  Used by Sbr. CERESScene_to_ERBEScene,
!                                                   CldPropCheck,
!                                                   DayNite_Define,
!                                                   MOAInput_Process, 
!                                                   PhaseCheck, TOA_Alb_Calc,
!                                                   TOAInput_Process,
!                                                   TopHt_Check
!
!  Internal Routines:
!    CERESScene_to_ERBEScene   - Determines ERBE geographic scene type based
!                                on percentages of CERES scene types
!    CERESScene_ERBEScene_INT  - Interfaces with Sbr. CERESScene_to_ERBEScene
!                                for scene percentage input data of type INTEGER
!    CERESScene_ERBEScene_REAL - Interfaces with Sbr. CERESScene_to_ERBEScene
!                                for scene percentage input data of type REAL
!    CldPropCheck              - Ensures that all required input cloud  
!                                property values are properly defined for the 
!                                current FOV
!    DayNite_Define            - Sets the values for the day/night flag 
!                                according to the value of the cosine of the 
!                                solar zenith angle
!    MOAInput_Process          - Loads MOA regional data into parameters used 
!                                by SARB algorithms
!    PhaseCheck                - Ensures that cloud partical sizes meet the  
!                                minimum size requirements for the Fu-Liou model
!    TOA_Alb_Calc              - Calculates the TOA albedo based on the input 
!                                SW TOA flux
!    TOAInput_Process          - Verifies that the values of the input TOA flux
!                                values for the current FOV and initiate 
!                                calculation of the TOA albedo
!    TopHt_Check               - Determines whether or not the input cloud 
!                                property data for the current FOV are adequate 
!                                for processing
!
! !Revision History:
!  Version 4.0   June 17, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Modified Sbr. MOAInput_Process
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Modified Sbr. PhaseCheck
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Modified Sbr. TopHt_Check
!                  2. Modified Sbr. AerosolScene_Define 
!                  3. Modified Sbr. TOAInput_Process
!                  4. Modified Sbr. TOA_Alb_Calc
!                  5. Added Sbr. CldPropCheck
!                  6. Modified Sbr. DayNite_Define
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Modified Sbr. PhaseCheck
!                  2. Modified Sbr. TopHt_Check
!                  3. Modified Sbr. MOAInput_Process
!                  4. Modified Sbr. AerosolScene_Define 
!  Version 3.1   June 22, 2001 - SCCR #267
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Modified Sbr. MOAInput_Process
!                  2. Modified Sbr. AerosolScene_Define
!                  3. General Cleanup
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Added Sbr. AerosolScene_Define
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Breakout of logic common to both the Instantaneous and
!                     Synoptic SARB Subsystems                
!
! !END
!
!*****************************************************************************
!
!
   USE FL_IO_Params
   USE MOA_IO_NESTED, ONLY: MOA_TYPE
   USE Profile_Params
   USE SARBInput_Params
   USE SARB_OutVar
   USE SARB_Var
!
!
   IMPLICIT NONE
!
!
      INTERFACE Map_CERESScene_ERBEScene
          MODULE PROCEDURE CERESScene_ERBEScene_REAL, CERESScene_ERBEScene_INT
      END INTERFACE
!
   CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE CERESScene_ERBEScene_REAL (InputFrac_REAL,             & ! I
                                                            ERBEScene)    ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   CERESScene_ERBEScene_REAL
!
! !Description:
!    Routine ID - 7.2.2.1.2.1a
!
!    Purpose - Interfaces with Sbr. CERESScene_to_ERBEScene for scene 
!              percentage input data of type REAL
!
!
! !Input Parameters:
!    InputFrac_REAL - Input scene type percentages
!
! !Output Parameters:
!    ERBEScene - ERBE geographic scene type
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!    SARBInput_Params - (SARBlib) Parameters: SceneNumCERES
!
!  Internal Routines Used:
!    CERESScene_to_ERBEScene
!
! !Revision History:
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (OUT) :: ERBEScene
      REAL,    INTENT (IN)  :: InputFrac_REAL ( SceneNumCERES)
!
!
! *** Determine ERBE geopgraphic scene type
!
      CALL CERESScene_to_ERBEScene (InputFrac_REAL,                      & ! I
                                                    ERBEScene)             ! O
!
!
      END SUBROUTINE CERESScene_ERBEScene_REAL
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE CERESScene_ERBEScene_INT (InputFrac_INT,                & ! I
                                                          ERBEScene)       ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   CERESScene_ERBEScene_INTEGER
!
! !Description:
!    Routine ID - 7.2.2.1.2.1b
!
!    Purpose - Interfaces with Sbr. CERESScene_to_ERBEScene for scene 
!              percentage input data of type INTEGER
!
!
! !Input Parameters:
!    InputFrac_INT - Input scene type percentages
!
! !Output Parameters:
!    ERBEScene - ERBE geographic scene type
!
! !Team-Unique Header:
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    SARBInput_Params - (SARBlib) Parameters: SceneNumCERES
!
!  Internal Routines Used:
!    CERESScene_to_ERBEScene
!
! !Revision History:
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (OUT) :: ERBEScene
      INTEGER, INTENT (IN)  :: InputFrac_INT ( SceneNumCERES)
!
      REAL :: InputFrac_REAL ( SceneNumCERES)
!
!
! *** Convert integer values to real values as required by 
!     Sbr. CERESScene_to_ERBEScene
!
      InputFrac_REAL ( 1:SceneNumCERES) =REAL (InputFrac_INT ( 1:SceneNumCERES))
!
!
! *** Determine ERBE geographic scene type
!
      CALL CERESScene_to_ERBEScene (InputFrac_REAL,                   & ! I
                                                    ERBEScene)          ! O
!
!
      END SUBROUTINE CERESScene_ERBEScene_INT
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE CERESScene_to_ERBEScene (InputFrac,                  & ! I
                                                     ERBEScene)         ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   CERESScene_to_ERBEScene
!
! !Description:
!    Routine ID - 7.2.2.1.2.2
!
!    Purpose - To determine the ERBE geographic scene type bases on input
!              CERES scene type percentages
!
!
! !Input Parameters:
!    InputFrac     - Input scene type percentages
!    SceneNumCERES - Number of CERES scene types
!
! !Output Parameters:
!    ERBEScene - ERBE geographic scene type
!
! !Team-Unique Header:
!
!  Modules Used:    (Parameters followed by "*" are defined by this routine)
!    SARBInput_Params - (SARBlib) Parameters: DesertIdx, ERBECoast, ERBEDesert,
!                                             ERBELand, ERBEOcean, ERBESnow, 
!                                             ERBEUnknown, LandIdx, MinCoast,
!                                             MinDesert, MinLand, MinOcean, 
!                                             MinSnow, NumDesertTyp, NumLandTyp,
!                                             NumSnowTyp, OceanIdx,
!                                             SceneNumCERES, SceneNumERBE, 
!                                             SnowIdx
!    SARB_Var         - (SARBlib) Parameters: isc*
!
!  Algorithm Description:
!    The algorithm in this routine follows the algorithm provided in the
!    SSF collection guide for mapping CERES scene type percentages into
!    an ERBE geographic scene type
!
! !Revision History:
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      REAL,    INTENT (IN)  :: InputFrac ( SceneNumCERES)
      INTEGER, INTENT (OUT) :: ERBEScene
!
      REAL    :: DesertFrac ( NumDesertTyp)
      INTEGER :: J
      REAL    :: LandFrac ( NumLandTyp)
      REAL    :: OceanFrac 
      REAL    :: SnowFrac ( NumSnowTyp)
      REAL    :: SceneFrac ( SceneNumERBE)
!
!
! *** Begin by computing percentage of desert scene types
!
      DesertFrac = (/(InputFrac ( DesertIdx ( J)), J=1,NumDesertTyp)/)
      SceneFrac ( ERBEDesert) =  SUM (DesertFrac)
!
      IF (SceneFrac ( ERBEDesert) > MinDesert) THEN
!     *** Scene type is desert.
!
          isc = ERBEDesert
!
      ELSE 
!     *** Scene type not desert.
!
          SnowFrac = (/(InputFrac ( SnowIdx ( J)), J=1,NumSnowTyp)/)
          SceneFrac ( ERBESnow) = SUM (SnowFrac)
!
          IF (SceneFrac ( ERBESnow) > MinSnow) THEN
!         *** Scene type is snow.
!
              isc = ERBESnow
!
          ELSE 
!         *** Scene type is not snow.
!
              OceanFrac = InputFrac ( OceanIdx)
              SceneFrac ( ERBEOcean) = OceanFrac
!
              IF (SceneFrac ( ERBEOcean) > MinOcean) THEN 
!             *** Scene type is ocean.
!
                  isc = ERBEOcean
!
              ELSE 
!             *** Scene type is not ocean.
!
                  LandFrac = (/(InputFrac ( LandIdx ( J)), J=1,NumLandTyp)/)
                  SceneFrac ( ERBELand) = SUM (LandFrac)
!
                  IF ( SceneFrac ( ERBELand) > MinLand) THEN
!                 *** Scene type is land.
                      isc = ERBELand
                  ELSE
!                 *** Scene type is not land.
                      SceneFrac ( ERBECoast) =   SceneFrac ( ERBELand)     &
                                               + SceneFrac ( ERBEDesert)   &
                                               + SceneFrac ( ERBESnow)
!
                      IF (SceneFrac ( ERBECoast) > MinCoast) THEN
!                     *** Scene type is land/ocean mix.
                          isc = ERBECoast
                      ELSE 
!                     *** Scene type cannot be determined.
                          isc = ERBEUnknown
                      END IF
!
                  END IF
!
              END IF
!
          END IF
!
      END IF
!
!
      END SUBROUTINE CERESScene_to_ERBEScene
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE DayNite_Define 
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   DatNite_Define
!
! !Description:
!    Routine ID - 5.2.1.2.1
!
!    Purpose - To set the day/night flag according to the value of
!              the cosine of the solar zenith angle
!
! !Input Parameters:
!    NONE
!
! !Output Parameter:
!    NONE
!
! !Team-Unique Heading:
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    SARBInput_Params - (SARBlib) Parameters: COSDayNite_Terminator, Daytime,
!                                             Nighttime
!    SARB_Var         - (SARBlib) Parameters: DayNite*, u0
!
! !Revision History:
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Removed assignment statement for nighttime FOV counter
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Use of parameters as opposed to hardcoded values
!
! !END
!
!*****************************************************************************
!
!
      DayNite = Daytime
      IF (u0 <= COSDayNite_Terminator) THEN
!     *** Nighttime
!
          DayNite = Nighttime
!
      END IF
!
!
      END SUBROUTINE DayNite_Define
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE TOAInput_Process (NoProcess_SunGlint, NoProcess_TOA)    ! O
!
!***************************************************************************
! 
! !F90
!
! !Name:
!    TOAInput_Process
!
! !Description:
!    Routine ID - 5.3.1.5.1
!
!    Purpose - To verify the values of the input TOA flux values for the 
!              current FOV and initiate calculation of the TOA albedo
!
!
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NoProcess_SunGlint - Flag indicating whether or not the sunglint 
!                         percentage exceeds the maximum percentage allowed 
!                         for processing
!                          = 0; Percentage value good
!                          = 1; Percentage value exceeds the maximum 
!                               percentage allowed
!    NoProcess_TOA      - Flag indicating whether or not all the the TOA 
!                         values obtained from the input product are within 
!                         the allowable range for processing
!                          = 0; All TOA values within range
!                          = 1; All TOA values not within range
!
! !Team-Unique Header:
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params     - (SARBlib)  Parameters: Default_INT4_FL
!    SARB_OutVar      - (SARBlib)  Parameters: LW_High, LW_Low, SW_High, SW_Low,
!                                              UpDir
!    SARB_Var         - (SARBlib)  Parameters: LW_TOA_flux, 
!                                              SW_TOA_flux, WN_TOA_flux, WN_Width,
!                                              WNflx_High, WNflx_Low
!    SARBInput_Params - (SARBlib)  Parameters: SunGlint_Max, Sunglint_Percent
!
!  Internal Routines Used:
!    TOA_Alb_Calc
!
! !Revision History:

!  FM6 changes    added flux_low and flux_high

!  Version 4.0   May 27, 2020 - SCCR #1657 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Added variables Flux_Low and Flux_High to allow
!                  different flux limits for window channel for FM6 processing.
!                  FM6 contains longwave data in the window channel.
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Redefined CERES Observed SW flux to 0.0 if input
!                     value is default.  This will allow processing of 
!                     FOVs in cases where the LW value is good but the
!                     SW value is not.
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Switched from using default values from CERESlib 
!                     Mod. ceres_defaults to using values from Mod. FL_IO_Params
!                  2. Changes to NoProcess Flag:
!                     a. Divided into two flags - NoProcess_SunGlint and 
!                        NoProcess_TOA
!                     b. Changes intent from in/out to out only
!                  3. Moved QC Report counter assignment statements out
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Use of parameters as opposed to hardcoded values
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (OUT) :: NoProcess_SunGlint
      INTEGER, INTENT (OUT) :: NoProcess_TOA
!
      REAL :: Flux_Low, Flux_High
!
      NoProcess_SunGlint = 0
      NoProcess_TOA = 0
!
      IF ( SW_TOA_flux == Default_REAL4_FL) SW_TOA_flux = 0.0   
!
      IF (Instr_Idx == CERES_FM6) THEN
       Flux_Low = LW_Low
       Flux_High = LW_High( UpDir)
      ELSE
       Flux_Low = WNflx_Low
       Flux_High = WNflx_High
      ENDIF



      IF (      LW_TOA_flux >= LW_Low  .AND.  LW_TOA_flux <= LW_High ( UpDir)  &
          .AND. SW_TOA_flux >= SW_Low  .AND.  SW_TOA_flux <= SW_High           &
          .AND. WN_TOA_flux >= Flux_Low  .AND.  WN_TOA_flux <= Flux_High)    THEN
!
!     *** Calculate TOA albedo
!
          CALL TOA_Alb_Calc
!
          IF (       Sunglint_Percent > SunGlint_Max                         &
                .AND. Sunglint_Percent /= Default_INT4_FL) THEN 
!         *** Unallowable sunglint percentage value
!
              NoProcess_SunGlint = 1
!
          END IF
!
!
      ELSE
!     *** Invalid CERES TOA Flux data
!
          NoProcess_TOA = 1
!
      END IF
!
!
      END SUBROUTINE TOAInput_Process
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE TOA_Alb_Calc
!
!***************************************************************************
! 
! !F90
!
! !Name:
!   TOA_Alb_Calc
!
! !Description:
!   Routine ID - 5.2.1.5.1.1
!
!   Purpose - To calculate the TOA albedo based on the input SW TOA flux
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
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params - (SARBlib)  Parameters: SolCon, u0
!    SARB_Var     - (SARBlib)  Parameters: ap*, EarthSun_Sqr, SW_TOA_flux
!
! !Revision History:
!  Version 3.5   June 4, 2014 - SCCR #1017
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Added variable solar constant provided by TISA group 
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Switched from using solar constant from CERESlib 
!                    Mod. ceres_constants to using value from Mod. FL_IO_Params
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Use of parameters as opposed to hardcoded values
!
! !END
!
!***************************************************************************
! 
!
! *** Calculate TOA albedo
!
!  print *, "EarthSun_Sqr =", EarthSun_Sqr
!  print *, "u0 =", u0
  IF (DayNite == Nighttime .or. u0 < 0.001) THEN
    ap = Default_REAL4_FL
  ELSE
    ap = SW_TOA_Flux / ((Solar_Constant(DataYear, DataMonth, DataDay)/EarthSun_Sqr) * u0)
  ENDIF
!
!
      END SUBROUTINE TOA_Alb_Calc
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
      SUBROUTINE TopHt_Check ( NoProcess)                      ! I/O
!          
!***************************************************************************
! 
! !F90
!
! !Name:
!    TopHt_Check
!
! !Description:
!   Routine ID -
!
!   Purpose - To determine whether or not the input cloud property data
!             for the current FOV are adequate for processing
!
! 
! !Input Parameters:
!    NONE
!
!  Input/Output Parameters:
!    NoProcess   - Flag indicating whether or not to continue processing
!                  the current FOV
!                  = 0; Continue processing
!                  = 1; Set output values to default and go to next FOV
!
! !Output Parameters:
!    None
!
! !Team-Unique Header:
!
!  Modules Used   (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params   - (SARBlib)  Parameters: CF, CldTop, Default_REAL4_FL 
!    Profile_Params - (SARBlib)  Parameters: InitPass, MaxLev_Cld
!    SARB_QC_Params - (SARBlib)  Parameters: ICnt_NoPres*
!    SARB_Var       - (SARBlib)  Parameters: DayNite
!
! !Revision History:
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Switched from using default values from CERESlib 
!                     Mod. ceres_defaults to using values from Mod. FL_IO_Params
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Removed Flag_DetCld output variable
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removal of logic to set flag indicating that SARB needs 
!                     to calculate nighttime cloud heights
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Use of parameters as opposed to hardcoded values
!
! !END
!
!***************************************************************************
! 
!
      INTEGER, INTENT (INOUT) :: NoProcess
!
      INTEGER :: I
!
!
! *** Check each indicated cloud layer
!
      DO I = 1, MaxLev_Cld
          IF (  CF ( InitPass, I) > 0.0                                   &
               .AND. CldTop ( InitPass, I) == Default_REAL4_FL) THEN
!         *** Input data for this FOV are not usable.
!
!!!                  ICnt_NoPres ( DayNite) = ICnt_NoPres ( DayNite) + 1
                  NoProcess = 1
!
          END IF
      END DO
!
!
      END SUBROUTINE TopHt_Check
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE PhaseCheck 
!
!
!***************************************************************************
! 
! !F90
!
! !Name:
!    PhaseCheck
!
! !Description:
!   Routine ID - 5.2.1.6.3
!
!   Purpose - Ensures that cloud partical sizes meet the size 
!             requirements for the Fu-Liou model
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
!  Modules Used   (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params     - (SARBlib)  Parameters: CF, Default_REAL4_FL 
!    Profile_Params   - (SARBlib)  Parameters: InitPass, MaxLev_Cld
!    SARBInput_Params - (SARBlib)  Parameters: IcePart_size*,  
!                                              LiqPart_size*, LiqPhase,
!                                              MaxLiq_Phase, MaxIce_size,
!                                              MaxLiq_size,  MinIce_size,
!                                              MinLiq_size 
!    SARB_Var         - (SARBlib)  Parameters: Phase_mn 
!
! !Revision History:
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Added comparisons to ensure that particle sizes 
!                     do not exceed maximum values
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Replaced comparisons against the parameters
!                     IcePhase and LiqPhase with comparisons against the
!                     parameter MaxLiq_Phase
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removal of logic to define nighttime cloud properties
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Initial version of code
!
! !END
!
!***************************************************************************
! 
!
      INTEGER :: I
!
!
! *** Ensure particle size is within acceptable limits
!
      DO I = 1, MaxLev_Cld
!
          IF (CF ( InitPass, I) > 0.0                                  &
             .AND. CF ( InitPass, I) /= Default_REAL4_FL) THEN
!         *** Cloud fraction value indicates an existing cloud layer.
!
              IF (Phase_mn ( I) <= MaxLiq_Phase                              &
                  .AND. Phase_mn ( I) /= Default_REAL4_FL) THEN
!             *** Cloud layer is predominately liquid 
!
                  IF (LiquidPart_size ( InitPass, I) < MinLiq_size)  THEN
                      LiquidPart_size ( InitPass, I) = MinLiq_size
                  END IF
!
                  IF (LiquidPart_size ( InitPass, I) > MaxLiq_size)  THEN
                      LiquidPart_size ( InitPass, I) = MaxLiq_size
                  END IF
!
              ELSE IF (Phase_mn ( I) > MaxLiq_Phase                          &
                       .AND. Phase_mn ( I) /= Default_REAL4_FL) THEN
!             *** Cloud layer is predominately ice 
!
                  IF (IcePart_size ( InitPass, I) < MinIce_size) THEN
                      IcePart_size ( InitPass, I) = MinIce_size
                  END IF
!
                  IF (IcePart_size ( InitPass, I) > MaxIce_size) THEN
                      IcePart_size ( InitPass, I) = MaxIce_size
                  END IF
!
              END IF
!
          END IF
!
      END DO
!
!
      END SUBROUTINE PhaseCheck
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE CldPropCheck (NoProcess)                           ! O
!
!***************************************************************************
! 
! !F90
!
! !Name:
!    CldPropCheck
!
! !Description:
!   Routine ID - 5.2.1.6.3
!
!   Purpose - Ensures that all required input cloud property values are
!             properly defined for the current FOV
!
! 
! !Input Parameters:
!    NONE 
!
! !Output Parameters:
!    NoProcess   - Flag indicating whether or not to continue processing
!                  the current FOV
!                  = 0; Continue processing
!                  = 1; Set output values to default and go to next FOV
!
! !Team-Unique Header:
!
!  Modules Used   (Parameters followed by "*" are defined by this routine)
!    FL_IO_Params     - (SARBlib)  Params: CF, CldTop, Default_REAL4_FL,
!                                          IcePart_size, LiquidPart_size,
!    Profile_Params   - (SARBlib)  Params: InitPass, MaxLev_Cld
!    SARBInput_Params - (SARBlib)  Params: IcePart_size,  
!                                          LiqPart_size, MaxLiq_Phase 
!    SARB_Var         - (SARBlib)  Params: EffTemp, Ice_path, Liquid_path,  
!                                          OptDepth_mn, Phase_mn 
!
! !Revision History:
!   March 2004, scott zentz added check of ice/liquid particle
!               to 0.0 as opposed to just default value  
!   Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!***************************************************************************
!
!
      INTEGER, INTENT (OUT) :: NoProcess
!
      INTEGER :: I
!
!
      NoProcess = 0
!
!
      DO I = 1, MaxLev_Cld
!
          IF (CF ( InitPass, I) > 0.0                                  &
             .AND. CF ( InitPass, I) /= Default_REAL4_FL) THEN
!         *** Cloud fraction value indicates an existing cloud layer.
!             Verify that other required cloud properties are validly defined.
!
              IF (CldTop ( InitPass, I) == Default_REAL4_FL) NoProcess = 1
!
              IF (EffTemp ( InitPass, I) == Default_REAL4_FL) NoProcess = 1
!
              IF (OptDepth_mn ( InitPass, I) == Default_REAL4_FL) NoProcess = 1
!
              IF (Phase_mn ( I) == Default_REAL4_FL) THEN
!
                  NoProcess = 1
!
              ELSE IF ( Phase_mn ( I) <= MaxLiq_Phase) THEN
!             *** Cloud layer is predominately liquid 
!
                  IF (LiquidPart_size ( InitPass, I) == Default_REAL4_FL .OR. &
                      LiquidPart_size ( InitPass, I) == 0.0 ) THEN
                      NoProcess = 1
                  ELSE
                      part_rad ( I) = LiquidPart_size ( InitPass, I)
                      IcePart_size ( InitPass, I) = 0.0
                  END IF
!
                  IF (Liquid_path ( I) == Default_REAL4_FL) NoProcess = 1
!
              ELSE IF (Phase_mn ( I) > MaxLiq_Phase) THEN
!             *** Cloud layer is predominately ice 
!
                  IF (IcePart_size ( InitPass, I) == Default_REAL4_FL .OR. &
                      IcePart_size ( InitPass, I) == 0.0 ) THEN
                      NoProcess = 1
                  ELSE
                      part_rad ( I) = IcePart_size ( InitPass, I)
                      LiquidPart_size ( InitPass, I) = 0.0
                  END IF
!
                  IF (Ice_path ( I) == Default_REAL4_FL) NoProcess = 1
!
              END IF
!
          END IF
!
      END DO
!
!
  END SUBROUTINE CldPropCheck
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE MOAInput_Process (MOA)
!
!
!***************************************************************************
!
! !F90
!
! !Name:
!   MOA_Ingest
!
! !Description:
!   Routine ID - 5.2.1.3.1
!
!   Purpose - To load MOA regional data into parameters used by
!             SARB algorithms
!
!
! !Input Parameters:
!   MOA - MOA regional data
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!   (Parameters followed by "*" are defined by this routine)
!    MOA_IO      - (CERESlib) Structures: MOA_TYPE
!    SARB_OutVar - (SARBlib)  Parameters: hPa70_Idx, hPa200_Idx, hPa500_Idx, 
!                                         NumLev_Arc (comment only), Pres_Out*, 
!                                         TOALev_Idx
!    SARB_Var    - (SARBlib)  Parameters: AerOptDepth*, MOA_SfcPres*, MOA_PW*, ozcm*,  
!                                         SfcAirHumid*, SfcAirTemp*, SfcGeoPt*,
!                                         SfcUvect*, SfcVwind*, SfcWind* 
!
! !Revision History:
!  Version 4.0   June 17, 2004 - SCCR #541  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Added MOA_PW and SfcWind assignments 
!  Version 3.2   December 24, 2001 - SCCR #315  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Changed lowest pressure for vertical flux profile 
!                 archival from 1.0 hPa to 0.1 hPa 
!  Version 3.1   June 22, 2001 - SCCR #267 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Modified Pres_Out assignment statements to reference
!                    level index parameters contained in Module SARB_Var 
!  Version 3.0   March 9, 2001 - SCCR #246 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Addition of adding 200 hPa to the output vertical profiles
!                      Pres_Out ( VertLev_TOA + 2) = 200.0
!                 2. Use of parameter VertLev_TOA
!                 3. Addition of parameter SfcVvect
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!***************************************************************************
!
!
      TYPE (MOA_TYPE), INTENT (IN) :: MOA
!
!
! *** Surface data
!
      SfcAirTemp  = MOA % Temp_MP ( 1)
      SfcAirHumid = MOA % Humid_MP ( 1)
      SfcGeoPt    = MOA % SfcGeoPt
      MOA_SfcPres = MOA % SfcPres
      SfcUvect    = MOA % SfcUwind
      SfcVvect    = MOA % SfcVwind
      SfcWind     = SQRT (SfcUvect**2 + SfcVvect**2)
      MOA_PW      = MOA % Precip_H2O 
!
!
! *** SARB needs column ozone in centimeters.
!
!###      oz = MOA % CO3 
      ozcm = (MOA % CO3) / 1000.
!
!
! *** Assign heights (hPa) for archiving the output vertical flux profiles 
!
!     NOTE:  The FOV Surface Pressure, Pres_Out ( NumLev_Arc), is defined 
!            in Module No_Cloud, Sbr. FixFloat_Sort
!
      Pres_Out ( TOALev_Idx) =   0.1
      Pres_Out ( hPa70_Idx)  =  70.0
      Pres_Out ( hPa200_Idx) = 200.0
      Pres_Out ( hPa500_Idx) = 500.0
!
!
! *** Initialize aerosol optical depth value to value on MOA
!
!###      AerOptDepth = MOA % CA_OptDepth
!
!
      END SUBROUTINE MOAInput_Process
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
END MODULE SARBInput_Utils

