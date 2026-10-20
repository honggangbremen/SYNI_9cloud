MODULE SARB_OutVar
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!
! !Description:
!   Routine ID -
!
!   Purpose - This module provides type declarations for SARB variables that
!             will be permanently archived on, or used to calculate the 
!             values of variables archived on, EITHER the CERES CRS or the SYN.
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
!    Profile_Params - (SARBlib)  Parameters: MaxLev_Cld, NumDir,   
!                                            NumPass, NumSkyCond
!
!  Key Local Parameters:
!    a_aer                - Adjustment to the total aerosol optical depth after
!                           the initial pass through the radiation transfer
!                           model
!    a_CldFrac            - Array of adjustments to the cloud area fraction 
!                           means after the initial pass through the  
!                           radiation transfer model
!    a_CldTemp            - Array of adjustments to the cloud effective  
!                           temperature means after the initial pass through
!                           the radiation transfer model
!    a_OptDepth           - Array of adjustments to the cloud optical depth 
!                           means after the initial pass through the  
!                           radiation transfer model
!    a_Precip_H2O         - Adjustment to the precipitable water after the
!                           initial pass through the radiation transfer model
!    a_pts                - Adjustment to the skin temperature after the
!                           initial pass through the radiation transfer model
!    a_salb               - Adjustment to the surface albedo after the
!                           initial pass through the radiation transfer model
!    a_TotLWrad           - Adjustment to the Total LW radiance after the
!                           initial pass through the radiation transfer model
!    a_UpTrop_RelHum      - Adjustment to the upper tropospheric relative 
!                           humidity after the initial pass through the 
!                           radiation transfer model
!    a_UpTropHum          - Adjustment to the upper tropospheric specific 
!                           humidity after the initial pass through the 
!                           radiation transfer model
!    a_WNrad_SatEm        - Adjustment to the satellite-emulated WN radiance
!                           after the initial pass through the radiation 
!                           transfer model
!    a_WNflx_SatEm        - Adjustment to the satellite-emulated WN flux  
!                           after the initial pass through the radiation 
!                           transfer model
!    AerSrc_Flag          - Source of the total aerosol optical depth for
!                           the FOV that is broken down into the different 
!                           constituencies
!    DirDiff_High         - Maximum allowable non-default value for the 
!                           direct/diffuse ratio
!    DirDiff_Low          - Minimum allowable non-default value for the 
!                           direct/diffuse ratio
!    hPa70_Idx            - Index into the flux profile arrays to the value 
!                           corresponding to the 70 hPa level
!    hPa200_Idx           - Index into the flux profile arrays to the value 
!                           corresponding to the 200 hPa level
!    hPa500_Idx           - Index into the flux profile arrays to the value 
!                           corresponding to the 500 hPa level
!    LW_Delta ( I, J, K)  - Array of the differences in the LW vertical flux
!                           profiles between the constrained and the initial 
!                           passes through the radiation transfer model
!                             I = Sky condition index
!                             J = Flux direction index
!                             K = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude
!    LW_High ( I)         - Array of maximum allowable non-default values
!                           for each direction of LW flux profile values
!                             I = 1 ; Downward LW flux upper limit
!                               = 2 ; Upward LW flux upper limit
!    LW_Low               - Minimum allowable non-default value for LW flux 
!                           profile values
!    LWProf               - Vertical longwave flux profile at archival levels
!                             I = Pass through the model index
!                             J = Sky condition index
!                             K = Flux direction index
!                             L = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude
!    LWurad_High          - Maximum allowable non-default value for the 
!                           modeled LW unfiltered radiance
!    LWurad_Low           - Minimum allowable non-default value for the 
!                           modeled LW unfiltered radiance  
!    PAR_High             - Maximum allowable non-default value for the
!                           modeled PAR value
!    PAR_Low              - Minimum allowable non-default value for the
!                           modeled PAR value
!    SfcAlbSrc_Flag       - Flag indicating the source of the surface albedo
!                           data input to the initial pass through the model
!                           for the FOV
!    SW_Delta ( I, J, K)  - Array of the differences in the SW vertical flux
!                           profiles between the constrained and the initial 
!                           passes through the radiation transfer model
!                             I = Sky condition index
!                             J = Flux direction index
!                             K = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude
!    SW_High              - Maximum allowable non-default value for SW  
!                           flux profile values
!    SW_Low               - Minimum allowable non-default value for SW 
!                           flux profile values
!    SWProf               - Vertical shortwave flux profile at archival levels
!                             I = Pass through the model index
!                             J = Sky condition index
!                             K = Flux direction index
!                             L = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude
!    WN_Delta ( I, J, K)  - Array of the differences in the WN vertical flux
!                           profiles between the constrained and the initial
!                           passes through the radiation transfer model
!                             I = Sky condition index
!                             J = Flux direction index
!                             K = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude
!    WNProf               - Vertical window band flux profile at archival levels
!                             I = Pass through the model index
!                             J = Sky condition index
!                             K = Flux direction index
!                             L = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude
!    WNrad_High           - Maximum allowable non-default value for the 
!                           modeled WN filtered radiance
!    WNrad_Low            - Minimum allowable non-default value for the 
!                           modeled WN filtered radiance
!
! !Revision History:
!  Version 3.3   December 20, 2002 - SCCR #408  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Transfered to Module Profile_Params:
!                      ClrSky         MaxPass
!                      ConstrPass     NumDir
!                      DnDir          NumSkyCond
!                      hPa70_Idx      PrsSky
!                      hPa200_Idx     TOALev_Idx
!                      hPa500_Idx     TotSky
!                      InitPass       UpDir
!                  2. Transfered to Module FL_IO_Params and renamed:
!                      DirDiff_Init     DirDIff_Tune     t_optdepth
!                      LWProf_Constr    SWProf_Constr    WNProf_Constr
!                      LWProf_Init      SWProf_Init      WNProf_Init
!                  3. Transfered to Module FL_IO_Params:
!                      AerConstFlag
!  Version 3.2   R3 Date - SCCR #408  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Transfered to Module SARB_OutVar:
!                      TOA_LW_Up      TOA_LW_Dn
!                      TOA_SW_Up      TOA_SW_Dn
!                      TOA_WN_Up      TOA_WN_Dn
!                  2. Transfered to Module SARB_QC_Params:
!                      LW_sfc_dna    SW_sfc_dna
!                      LW_sfc_dnb    SW_sfc_neta
!                                    SW_sfc_netb
!                  3. Transfered from Module SARB_Var:
!                      hPa70_Idx
!                      hPa200_Idx
!                      hPa500_Idx
!                      TOALev_Idx
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Addition of:
!                  1. SW_sfc_dna
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Addition of variables for window channel flux values
!                Ed Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                 1. Addition of variables for adjustments to tuning variables 
!  Version 2.0   November 21, 1997  SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Release 2 code
!
! !END
!
!*****************************************************************************
!
  USE Profile_Params
!
  IMPLICIT NONE
!
!
! *****************************************
! Vertical flux profiles at archival levels
! *****************************************
!
!##      REAL, SAVE :: LWProf_Arc ( NumPass, NumSkyCond, NumDir, NumLev_Arc)
!##      REAL, SAVE :: SWProf_Arc ( NumPass, NumSkyCond, NumDir, NumLev_Arc)
!##      REAL, SAVE :: WNProf_Arc ( NumPass, NumSkyCond, NumDir, NumLev_Arc)
!
!
! ******************************************************************
! Adjustments to input data between passes through the Fu-Liou model 
! ******************************************************************
!
      REAL,    SAVE      :: a_aer
      REAL,    SAVE      :: a_CldFrac ( MaxLev_Cld)
      REAL,    SAVE      :: a_CldTemp ( MaxLev_Cld)
      REAL,    SAVE      :: a_OptDepth ( MaxLev_Cld)
      REAL,    SAVE      :: a_Precip_H2O
      REAL,    SAVE      :: a_pts
      REAL,    SAVE      :: a_salb
      REAL,    SAVE      :: a_UpTrop_RelHum
      REAL,    SAVE      :: a_UpTropHum
!
! ******************************************************
! Differences in output between passes through the model
! ******************************************************
!
      REAL,    SAVE  :: a_TotLWrad
      REAL,    SAVE  :: a_WNrad_SatEm
      REAL,    SAVE  :: a_WNflx_SatEm
      REAL,    SAVE  :: LW_Delta ( NumSkyCond, NumDir, NumLev_Delta) 
      REAL,    SAVE  :: SW_Delta ( NumSkyCond, NumDir, NumLev_Delta) 
      REAL,    SAVE  :: WN_Delta ( NumSkyCond, NumDir, NumLev_Delta) 
!
! ***********************
! Input data source flags
! ***********************
!
      INTEGER, SAVE  :: AerSrc_Flag
      INTEGER, SAVE  :: SfcAlbSrc_Flag
!
! *******************
! Output value limits
! *******************
!
      REAL,    PARAMETER :: DirDiff_Low = 0.0
      REAL,    PARAMETER :: DirDiff_High = 30.0
      REAL,    PARAMETER :: LW_Low = 0.0
      REAL,    PARAMETER :: LW_High ( NumDir) = (/ 700.0, 850.0 /)
      REAL,    PARAMETER :: LWurad_Low = 0.0
      REAL,    PARAMETER :: LWurad_High = 300.0
      REAL,    PARAMETER :: PAR_Low = 0.0
      REAL,    PARAMETER :: PAR_High = 780.0
      REAL,    PARAMETER :: SW_Low = 0.0
      REAL,    PARAMETER :: SW_High = 1500.0
      REAL,    PARAMETER :: WNrad_Low = 0.0
      REAL,    PARAMETER :: WNrad_High = 150.0
!
!
END MODULE SARB_OutVar
