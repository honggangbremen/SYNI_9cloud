MODULE SARBAer_Utils
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SARBAer_Utils
!
! !Description:
!    Routine ID - M5.2.1.4.1
!
!    Purpose - A module of routines to determine the aerosol constituency 
!              makeup required by the radiative transfer model
!
! !Input Parameters:
!    NONE
!
! !Output Parameter:
!    NONE
!
! !Team-Unique Heading:
!
!  Internal Routines:
!    AerosolScene_Define  - Drives the determination of the aerosol   
!                           constituency makeup required for a FOV by the 
!                           radiative transfer model
!    Aer_MultiWaves_MODIS - Determines the aerosol constituency makeup for
!                           a FOV based on multi-wavelength aerosol input data
!    Aer_OneWave_Stowe    - Determines the aerosol constituency makeup for
!                           a FOV based on sinle-wavelength aerosol input data
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    COLLINS_ASSIMILATION - (SARBlib) Used by Sbr. AerosolScene_Define
!    FL_IO_Params         - (SARBlib) Used by Sbr. AerosolScene_Define
!    gfdl_aer_clim        - (SARBlib) Used by Sbr. AerosolScene_Define
!    MonMODISAer_Params   - (SARBlib) Used by Sbr. Aer_MultiWaves_MODIS
!    RadParams            - (SARBlib) Used by Sbr. AerosolScene_Define, 
!                                                  Aer_MultiWaves_MODIS,
!                                                  Aer_OneWave_Stowe
!    SARB_OutVar          - (SARBlib) Used by Sbr. AerosolScene_Define
!    SARB_Var             - (SARBlib) Used by Sbr. AerosolScene_Define,
!                                                  Aer_OneWave_Stowe
!    SARBAer_Var          - (SARBlib) Used by Sbr. AerosolScene_Define,
!    SARBInput_Params     - (SARBlib) Used by Sbr. AerosolScene_Define
!    wssacomp             - (SARBlib) Used by Sbr. AerosolScene_Define
!
! !Revision History:
!  Version 4.0   June 10, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  1. Moved Aer_Rule_Select subroutine to Control_Mod
!                  2. Modified AerosolScene_Define
!  Version 3.5   August 1, 2003 - SCCR #458 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                  Added Aer_Rule_Select subroutine
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Initial version of module
!
! !END
!
!***************************************************************************
!
!
       USE FL_IO_Params
!       USE gfdl_aer_clim
       USE MonMODISAer_Params
       USE SARB_OutVar
       USE SARB_Var
       USE SARBAer_Var
       USE SARBInput_Params
       USE wssacomp
!
!
    IMPLICIT NONE
!
    CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE AerosolScene_Define (Aer_Unit, AOD_CentWL_Obs,         & ! I
                                      WL_Central,                       & ! I
                                                AOD_CentWL_Clim)          ! O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   AerosolScene_Define
!
! !Description:
!    Routine ID - 5.2.1.4.1
!
!    Purpose - Drives the determination of the aerosol constituency makeup 
!              required for a FOV by the radiative transfer model
!
! !Input Parameters:
!    Aer_Unit       - Collins aerosol climatology ancillary input data file
!                     unit
!    AOD_CentWL_Obs - Observed total aerosol optical depth at 63um from the 
!                     primary input product
!    WL_Central     - Central wavelength
!
! !Output Parameter:
!    AOD_CentWL_Clim - Climatological total aerosol optical depth
!
! !Team-Unique Heading:
!
!  Internal Routines Called:
!    Aer_MultiWaves_MODIS 
!    Aer_OneWave_Stowe    
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    COLLINS_ASSIMILATION - (SARBlib)  Sbr.: get_collins
!    FL_IO_Params         - (SARBlib)  Param: AerRatio, Default_INT4_FL, 
!                                             Default_REAL4_FL
!    gfdl_aer_clim        - (SARBlib)  Sbr.: get_gfdl_aer
!                                      Param: a_taus, clim_aots, clim_aot1,
!                                             itps*
!    RadParams            - (SARBlib)  Param: a_taus_temp*, a_wlis*, 
!                                             itranityp, mxac, n_atau*, nac, 
!                                             n_atau, sh_aer
!    SARB_OutVar          - (SARBlib)  Param: AerConstFlag*, AerSrc_Flag*
!    SARB_Var             - (SARBlib)  Param: AerClimSrc_Flag,
!                                             DomAerPercent, DomAerType,
!              RLat, RLong
!    SARBAer_Var          - (SARBlib)  Param: collins_aots_regrouped*, 
!                                             itranityp_rgc, NumWL_SSFA, 
!                                             sh_aer_rgc, SSFA_AOT
!    SARBInput_Params     - (SARBlib)  Param: AerOptDepth*, AerSrc_ClimOnly,
!                                             AerSrc_Collins,
!                                             AerSrc_GFDL, AerSrc_PrimInput, 
!                                             MinAerOptDepth, 
!                                             NumAerConst_Src
!    wssacomp             - (SARBlib)  Func.: getwssa
!
! !Revision History:
!  Version 4.0   June 10, 2004 - SCCR #541
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  Added local parameter IACPCT to correct for cases where 
!                  setting the aerosol constituency ratio to 1 (to prevent
!                  division by zero in Tune_Drive) caused the aerosol const
!                  flag to increment to invalid values.
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                  This routine was removed and Mod. SARBInput_Utils
!                  and completely changed.
!
! !END
!
!***************************************************************************
!
!
!      USE COLLINS_ASSIMILATION
!
      INTEGER, INTENT (IN)  :: Aer_Unit
      REAL,    INTENT (OUT) :: AOD_CentWL_Clim
      REAL,    INTENT (IN)  :: AOD_CentWL_Obs
      REAL,    INTENT (IN)  :: WL_Central
!
!
      REAL    :: AerConst_Ratio
      INTEGER :: IAC
      INTEGER :: IACPCT
      REAL    :: Scale_Height ( mxac)
!
!
      a_taus_temp ( 1:mxat, 1:mxac) = 0.0
!
!
! *** Define scalar value indicating the number of aerosol
!     constituencies used by the model
!
      nac = NumAerConst_Src ( AerClimSrc_Flag)
!
!
      AerConstFlag ( 1:mxac) = Default_INT4_FL
      DomAerPercent = 0.0
      DomAerType = 0
      itps ( 1:mxac) = 0
      Scale_Height ( 1:mxac) = 0.0
      
      
!###      a_wlis ( 1:n_atau, 1:nac) = WL_Central
!
!      IF (AerClimSrc_Flag == AerSrc_Collins .OR.&
!        AerClimSrc_Flag == AerSrc_CollinsBackup) THEN
!
!     *** Use Collins climatology
!
!          CALL get_collins (RLat, RLong,                                 & ! I
!                                        collins_aots_regrouped)            ! O
!          itps ( 1:mxac) = itranityp_rgc ( 1:mxac)
!          Scale_Height ( 1:mxac) = sh_aer_rgc ( 1:mxac) 
!          AOD_CentWL_Clim = collins_aots_regrouped ( 0)
!
!      ELSE IF (AerClimSrc_Flag == AerSrc_GFDL) THEN
!     *** Use GFDL climatology
!
!           CALL get_gfdl_aer (RLat, RLong)
!           itps ( 1:nac) = itranityp ( 1:nac)
!           Scale_Height ( 1:nac) = sh_aer ( 1:nac) 
!          AOD_CentWL_Clim = clim_aot1
!
!      END IF
!
!
!      Constituents: DO IAC = 1, NumAerConst_Src ( AerClimSrc_Flag)
!
!          IF (AerClimSrc_Flag == AerSrc_Collins .OR.&
 !           AerClimSrc_Flag == AerSrc_CollinsBackup) THEN
!
!           IF (collins_aots_regrouped ( 0) == 0.0) THEN
 !           AerConst_Ratio = 0.0
 !           IF (IAC == 1) AerConst_Ratio = 1
 !           ELSE
 !              AerConst_Ratio =   collins_aots_regrouped ( iac)  & ! Constituent
 !                               / collins_aots_regrouped ( 0)      ! Total
 !          ENDIF
!
!          ELSE IF (AerClimSrc_Flag == AerSrc_GFDL) THEN
!           IF (clim_aot1 == 0.0) THEN
 !           AerConst_Ratio = 0.0
 !           IF (IAC == 1) AerConst_Ratio = 1
 !          ELSE
 !           AerConst_Ratio = clim_aots ( iac) / clim_aot1              
 !          ENDIF
!
!          END IF
!
 !         IF (AerSrc_PrimInput == AerSrc_FOVTerraMODIS                 &
!              .OR.  AerSrc_PrimInput == AerSrc_GridTerraMODIS) THEN
!         *** MODIS aerosol data corresponding to the current FOV is 
!             available
!
!              IF (FLAG_OP == CERSYN) THEN
!               AerConst_Ratio = TISA_AerConst_Ratio(iac)              
!              ENDIF
!
!              CALL Aer_MultiWaves_MODIS (IAC, NumWL_SSFA,            & ! I
!                                         AerConst_Ratio, AOT_SSFA)     ! I
!              n_atau = 7
 !             AerSrc_Flag = AerSrc_PrimInput
!
!          ELSE IF (AerSrc_PrimInput == AerSrc_StoweFOV) THEN
!         *** Observed Stowe aerosol for the current FOV is available
!
!              CALL Aer_OneWave_Stowe (iac, AerConst_Ratio,            & ! I
!                                      AOD_CentWL_Obs)                    ! I
!              AerSrc_Flag = AerSrc_StoweFOV
!              n_atau = 1
!              a_wlis ( 1:n_atau, 1:nac) = WL_Central
!
!          ELSE IF (AerSrc_PrimInput == AerSrc_ClimOnly) THEN
!         *** Must use climatological values
!
!              IF (AerClimSrc_Flag == AerSrc_Collins .OR.&
!                AerClimSrc_Flag == AerSrc_CollinsBackup) THEN
!
!             *** Use climatology values
!
!                  a_taus_temp ( 1, iac) = collins_aots_regrouped ( iac)
 !                 AerSrc_Flag = AerSrc_Collins
!
!              ELSE IF (AerClimSrc_Flag == AerSrc_GFDL) THEN
!             *** Use GFDL climatology values
!
!                  a_taus_temp ( 1, iac) = clim_aots ( iac)
!                  AerSrc_Flag = AerSrc_GFDL
!
 !             END IF
!
!              n_atau = 1
!              a_wlis ( 1:n_atau, 1:nac) = WL_Central
!
!          END IF
!
!          IACPCT = NINT (AerConst_Ratio * 1000.0) * 1000

!          IF (IACPCT >= 1000000) IACPCT = 999000
!          IF (IACPCT < 1000) IACPCT = 0
   
!          AerConstFlag ( IAC) =   itps ( IAC) * 1000000                   &
!                                + IACPCT          &
!                                + NINT (Scale_Height ( IAC) * 10.0)
!
!          IF (AerConst_Ratio > DomAerPercent) THEN
!
!              DomAerPercent = AerConst_Ratio
!              DomAerType = itps ( IAC)
!   
!          END IF
!
!      END DO Constituents
!
!
      wssa = getwssa (mxac, a_taus_temp ( 1, 1:mxac), itps)
!
!
      END SUBROUTINE AerosolScene_Define
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Aer_MultiWaves_MODIS (Idx_AerConst, NumWL,            & ! I
                                       AerConst_Ratio, MODISAOD_perWL)   ! I
!
!*****************************************************************************80
!
! !F90
!
! !Name:
!   Aer_MultiWaves_MODIS
!
! !Description:
!   Routine ID -
!
!   Purpose - Determines the aerosol constituency makeup for
!             a FOV based on multi-wavelength aerosol input data
!
!
! !Input Parameters:
!   AerConst_Ratio - Ratio of climatological aerosol optical depth for the 
!                    current constituency to the climatological total aerosol 
!                    optical depth
!   Idx_AerConst   - Aerosol constituency array index 
!   NumWL          - Number of wavelengths with corresponding aerosol optical 
!                    depth values
!   MODISAOD_perWL - Array of MODIS aerosol optical depth values for the 
!                    different MODIS wavelengths
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!   MonMODISAer_Params - (SARBlib) Param: wlin
!   RadParams          - (SARBlib) Param: a_taus_temp*, a_wlis*
!
! !Revision History:
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Initial version of code
!
! !END
!
!*****************************************************************************80
!
!
      REAL,    INTENT (IN) :: AerConst_Ratio
      INTEGER, INTENT (IN) :: Idx_AerConst
      INTEGER, INTENT (IN) :: NumWL
      REAL,    INTENT (IN) :: MODISAOD_perWL ( NumWL)
!
!
      a_wlis ( 1:NumWL, Idx_AerConst) = wlin ( 1:NumWL)
      a_taus_temp ( 1:NumWL, Idx_AerConst) =   MODISAOD_perWL (1:NumWL)    &
                                             * AerConst_Ratio
!
!
      END SUBROUTINE Aer_MultiWaves_MODIS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Aer_OneWave_Stowe (Idx_AerConst, AerConst_Ratio,       & ! I
                                    AOD_CentWL_Obs)                       ! I
!
!*****************************************************************************80
!
! !F90
!
! !Name:
!   Aer_OneWave_Stowe
!
! !Description:
!   Routine ID -
!
!   Purpose - Determines the aerosol constituency makeup for
!             a FOV based on single-wavelength aerosol input data
!
!
! !Input Parameters:
!   AerConst_Ratio - Ratio of climatological aerosol optical depth for the 
!                    current constituency to the climatological total aerosol 
!                    optical depth
!   AOD_CentWL_Obs - Observed aerosol optical depth at 63um from the primary
!                    input product
!   Idx_AerConst   - Aerosol constituency array index 
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used:   (Parameters followed by "*" are defined by this routine)
!    RadParams - (SARBlib) Param: a_taus_temp* 
!    SARB_Var  - (SARBlib) Param: AerSrc_Flag*, AerSrc_PrimInput
!
! !Revision History:
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!*****************************************************************************80
!
!
      REAL,    INTENT (IN) :: AerConst_Ratio
      INTEGER, INTENT (IN) :: Idx_AerConst
      REAL,    INTENT (IN) :: AOD_CentWL_Obs
!
!
! *** Observed aerosol from the primary input product is available
!
      a_taus_temp ( 1, Idx_AerConst) = AOD_CentWL_Obs * AerConst_Ratio
      AerSrc_Flag = AerSrc_PrimInput
!
!
      END SUBROUTINE Aer_OneWave_Stowe
!
!
END MODULE SARBAer_Utils
