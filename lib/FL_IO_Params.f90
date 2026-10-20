MODULE FL_IO_Params
!
!******************************************************************************
!
! !F90
!
! !Name:
!    FL_IO_Params
!
! !Description:
!    Routine ID -
!
!    Purpose - To provide F90 type declarations and parameter definitions
!              for inputs to and outputs from the Fu-Liou radiative 
!              transfer model
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
!  Modules Used:
!   ceres_defaults - (CERESlib) Parameters: INT2_DFLT, INT4_DFLT, REAL4_DFLT
!   f90_kind       - (CERESlib) Parameters: INT2
!   Profile_Params - (SARBlib)  Parameters: MaxLev_Cld, MaxLev_SARB,
!                                           Num_BndSA, Num_BndSfcEm, NumDir,
!                                           NumPass, NumSkyCond
!
!  Key Local Parameters:
!   ActFlx_Sfc ( I, J)   - Surface actinic flux     
!                             I = Pass through the model index
!                             J = Sky condition index
!   Aer ( I)             - Array of total column aerosol optical depth 
!                          values for each pass through the radiative 
!                          transfer model
!                             I = Pass through the model index
!   AerConstFlag         - Array of flags indicating the aerosol 
!                          constituencies, along with their corresponding 
!                          content percentages and scale heights, that are
!                          contained within the current FOV
!   AerRatio ( I)        - Array of ratio applied to aerosol constituencies
!                          for each pass through the radiative transfer model 
!                          to maintain consistency with the total aerosol
!                          optical depth in the event of an adjustment to the  
!                          input total optical depth value
!                             I = Pass through the model index
!   aprof_match_vert     - Array to hold values of vertical match aerosols
!   a_taus               - Array of optical depths for each wavelength and 
!                          constiuent
!   a_taus_temp          - Temporary array of optical depths for each wavelength and 
!                          constiuent 
!   a_wlis               - Array of optical depth wavelengths 
!   CCLyr_Idx ( I, J, K) - Array containing the index into the ModLev array
!                          corresponding to each cloud top and bottom 
!                             I = Pass through the model index
!                             J = Cloud layer index
!                             K = 1 ; Cloud top index
!                               = 2 ; Cloud bottom index
!   CF ( I, J)           - Array containing the cloud area fractions for clear
!                          skies and each possible cloud layer for the 
!                          current FOV for each pass through the Fu-Liou 
!                          radiative transfer model
!                             I = Pass through the model index
!                             J = Cloud layer index (for clear skies, J=0)
!   Default_INT2_FL      - Default value for two-byte integers, defined  
!                          during subsystem initialization
!   Default_INT4_FL      - Default value for four-byte integers, defined  
!                          during subsystem initialization
!   Default_REAL4_FL     - Default value for four-byte reals, defined  
!                          during subsystem initialization
!   Diff ( I, J)         - Array of the diffuse component for calculation of 
!                          the direct/diffuse ratio at the surface
!                             I = Pass through the model index
!                             J = Sky condition index
!   Dir ( I, J)          - Array of the direct component for calculation of 
!                          the direct/diffuse ratio at the surface 
!                             I = Pass through the model index
!                             J = Sky condition index
!   Dir_SW_Sfc           - Array of the Direct Broadband SW at the Surface
!                             I = Pass through the model index
!                             J = Sky condition index
!   Diff_SW_Sfc           - Array of the Diffuse Broadband SW at the Surface
!                             I = Pass through the model index
!                             J = Sky condition index
!   DirDiff ( I, J)      - Array of the direct/diffuse ratio at the surface
!                             I = Pass through the model index
!                             J = Sky condition index
!   DirDiff_PAR_Sfc ( I, J) - Surface PAR direct to PAR diffuse ratio
!                             I = Pass through the model index
!                             J = Sky condition index
!   DirDiff_ChlorA_Sfc ( I, J) - Surface Chloraphyll_A Action Spectrum direct to PAR diffuse ratio
!                             I = Pass through the model index
!                             J = Sky condition index
!   DirDiff_Par_Purv ( I, J) - Surface Purves Action Spectrum  direct to diffuse ratio
!                             I = Pass through the model index
!                             J = Sky condition index
!   DirDiff_UVA_Sfc ( I, J) - Surface downwelling UVA direct to UVA diffuse ratio  
!                             I = Pass through the model index
!                             J = Sky condition index
!   DirDiff_UVB_Sfc ( I, J) - Surface downwelling UVB direct to UVB diffuse ratio
!                             I = Pass through the model index
!                             J = Sky condition index
!   DnPAR_TOA ( I, J)    - TOA downwelling PAR  
!                             I = Pass through the model index
!                             J = Sky condition index
!   DnUVA_Sfc ( I, J)    - Surface downwelling UVA       
!                             I = Pass through the model index
!                             J = Sky condition index
!   DnUVB_Sfc ( I, J)    - Surface downwelling UVB        
!                             I = Pass through the model index
!                             J = Sky condition index
!   DnUVA_TOA ( I, J)    - TOA downwelling UVB
!                             I = Pass through the model index
!                             J = Sky condition index
!   DnUVB_TOA ( I, J)    - TOA downwelling UVB
!                             I = Pass through the model index
!                             J = Sky condition index
!   FL_TOA_LW ( I, J)    - Longwave TOA flux (0.1 hPa) returned for each 
!                          sky condition from each pass through the 
!                          Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Sky condition index
!   FL_TOA_SW ( I, J)    - Shortwave TOA flux (0.1 hPa) returned for each 
!                          sky condition from each pass through the 
!                          Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Sky condition index
!   FL_TOA_WN ( I, J)    - Window channelTOA flux (0.1 hPa) returned for each 
!                          sky condition from each pass through the 
!                          Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Sky condition index
!   Flag_CldSky          - Flag indicating whether or not to call the Fu-Liou  
!                          model for cloudy-sky conditions for the current pass
!   Flag_ClrSky          - Flag indicating whether or not to call the Fu-Liou 
!                          model for clear-sky conditions for the current pass
!   Flag_ProfLW          - Flag indicating whether or not to calculate  
!                          longwave flux and radiance data with the current  
!                          pass through the model
!   Flag_ProfSW          - Flag indicating whether or not to calculate   
!                          shortwave flux and radiance data with the current  
!                          pass through the model
!   Flag_ProfWN          - Flag indicating whether or not to calculate window  
!                          channel and radiance data with the current pass 
!                          through the model
!   Flag_PrsSky          - Flag indicating whether or not to call the Fu-Liou  
!                          model for pristine-sky conditions for the current
!                          pass
!   Flag_SkyCond_NO      - Flag indicating that vertical flux profiles 
!                          for the specified sky condition, i.e., clear,
!                          cloudy, pristine, etc., calculated by the 
!                          Fu-Liou model are not requested
!   Flag_SkyCond_YES     - Flag indicating that vertical flux profiles 
!                          for the specified sky condition, i.e., clear,
!                          cloudy, pristine, etc., calculated by the 
!                          Fu-Liou model are requested
!   Flag_WLProc_NO       - Flag indicating that vertical flux profiles 
!                          for the specified wavelength, i.e., longwave,  
!                          shortwave, window, etc., calculated by the  
!                          Fu-Liou model are not requested
!   Flag_WLProc_YES      - Flag indicating that vertical flux profiles
!                          for the specified wavelength, i.e., longwave,  
!                          shortwave, window, etc., calculated by the
!                          Fu-Liou model are requested
!   IcePart_size ( I, J) - Array of ice water particle diameters for each
!                          cloud layer for the current FOV for each pass 
!                          through the Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Cloud layer index
!   IdxUV_Sfc ( I, J)    - Surface UV-Index  
!                             I = Pass through the model index
!                             J = Sky condition index
!   Instr_Idx            - Index of instruments, defined according to the 
!                          primary input product header. CERES instrument   
!                          index values are consistent with the values in  
!                          CERESlib Module ceres_constants 
!   itp_scene            - Aerosol scene classification for the current FOV
!   itps                 - Array containing aerosol scene types for each constiuent
!   itranityp            - Array containing fuliou types for each constiuent
!   IW_content ( I)      - Array containing the ice water content for each
!                          possible cloud layer for each pass 
!                          through the Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Cloud layer index
!   LiquidPart_size ( I) - Array of radii of liquid particles for each cloud
!                          for each cloud layer for each pass 
!                          through the Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Cloud layer index
!   LW_content ( I)      - Array containing the liquid water  
!                          content for each possible cloud layer for each pass 
!                          through the Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Cloud layer index
!   LWProf ( I, J, K, L) - Array of LW vertical flux profiles calculated
!                          by the radiative transfer model for each sky 
!                          condition, flux direction, and pass through the  
!                          radiative transfer model
!                             I = Pass through the model index
!                             J = Sky condition index
!                             K = Flux direction index
!                             L = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude.
!   ModLev ( I, J)       - Array of heights (hPa) of the levels in the  
!                          current FOV's vertical profile for each pass
!                          through the radiative transfer model
!                             I = Pass through the model index
!                             J = Heights (hPa) for each level in 
!                                 the vertical profile
!   ModLev_Cnt ( I)      - Number of levels in the current FOV's vertical
!                          profile for each pass through the radiative 
!                          transfer model
!                             I = Pass through the model index
!   ModLev_ProO3 ( I, J) - Array containing the vertical profile of ozone
!                          mixing ratios for the heights contained in the 
!                          ModLev array for each pass throgh the radiative
!                          transfer model
!                             I = Pass through the model index
!                             J = Ozone mixing ratio for each level in 
!                                 the vertical profile
!   ModLev_SpHum ( I, J) - Array containing the vertical specific humidity
!                          profile for the heights contained in the ModLev 
!                          array for each pass through the radiative 
!                          transfer model
!                             I = Pass through the model index
!                             J = Specific humidity values for each level in 
!                                 the vertical profile
!   ModLev_Temp ( I, J)  - Array containing the vertical temperature profile 
!                          for the heights contained in the ModLev array
!                          for each pass through the radiative transfer model
!                             I = Pass through the model index
!                             J = Temperature values for each level in 
!                                 the vertical profile
!   mxac                 - Maximum number of aerosol constituents
!   mxat                 - Maximum number of aerosol wavelengths
!   n_atau               - Number of aerosol wavelengths 
!   nac                  - Number of aerosol constiuents
!   PAR ( I, J)          - Array of photosynthetically active radiation
!                             I = Pass through the model index
!                             J = Sky condition index
!   Par_ChlorA_SW ( I, J) - Array of Chloraphyll_A Action Spectrum SW 
!                             I = Pass through the model index
!                             J = Sky condition index 
!   Par_Purve_SW ( I, J) - Array of Purves Action Spectrum SW 
!                             I = Pass through the model index
!                             J = Sky condition index
!   PAR_Sfc ( I, J)      - Surface all-sky PAR        
!                             I = Pass through the model index
!                             J = Sky condition index
!   PristineAer          - Total aerosol optical thickness for pristine 
!                          sky conditions
!   pts ( I)             - Array of surface skin temperature for each pass
!                          through the Fu-Liou Radiative transfer model
!                             I = Pass through the model index
!   SfcEmiss ( I, J)     - Array containing the infrared surface emissivities
!                          for the longwave bands indicated by the Fu-Liou 
!                          radiative transfer model for each pass through the
!                          model
!                             I = Pass through the model index
!                             J = Longwave band index
!   sh_aer               - Array containing aerosol scale heights for each constiuent  
!   SpSfcAlb ( I, J, K)  - Array containing the spectral surface albedo
!                          for each cloud layer for the shortwave
!                          bands indicated by the Fu-Liou model
!                             I = Pass through the model index
!                             J = Shortwave band index
!                             K = Cloud layer index
!   ss                   - Solar constant corrected by Earth-Sun distance
!   SWProf ( I, J, K, L) - Array of SW vertical flux profiles calculated
!                          by the radiative transfer model for each sky 
!                          condition, flux direction, and pass through the  
!                          radiative transfer model
!                            I = Pass through the model index
!                            J = Sky condition index
!                            K = Flux direction index
!                            L = Vertical profile level index, with level 1
!                                indicating the level at the highest altitude.
!   SW_TOAUP_UNCOR       - Upward SW flux at TOA from Fu-Liou model
!   SW_SFCDN_UNCOR       - Downward SW flux at surface from Fu-Liou model
!   SW_TOAUP_CORR        - Corrected upward SW flux at TOA 
!   SW_SFCDN_CORR        - Corrected downward SW flux at surface  
!   TotLWrad ( I, J)     - Array of upward TOTAL LW radiance at TOA calculated 
!                          by the Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Sky condition index
!   ur                   - Cosine of the viewing zenith angle at the TOA
!                          for the current FOV
!   u0                   - Cosine of the solar zenith angle for the current FOV
!   WNflx_SatEm ( I, J)  - Array of raw 3.7 micron window channel TOA flux 
!                          calculated by the Fu-Liou radiative transfer 
!                          model and emulated to the width of the CERES
!                          instrument's window channel
!                             I = Pass through the model index
!                             J = Sky condition index
!   WNProf ( I, J, K, L) - Array of WN vertical flux profiles calculated
!                          by the radiative transfer model for each sky 
!                          condition, flux direction, and pass through the  
!                          radiative transfer model
!                             I = Index to the pass through the model
!                             J = Sky condition index
!                             K = Flux direction index
!                             L = Vertical profile level index, with level 1
!                                 indicating the level at the highest altitude.
!   WNrad_RAW ( I, J)    - Array of raw 3.7 micron window radiance as 
!                          calculated by the Fu-Liou radiative transfer model
!                             I = Pass through the model index
!                             J = Sky condition index
!   WNrad_SatEm ( I, J)  - Array of raw 3.7 micron window filtered radiance 
!                          calculated by the Fu-Liou radiative transfer 
!                          model and emulated to the width of the CERES
!                          instrument's window channel
!                             I = Pass through the model index
!                             J = Sky condition index
!   WNurad_SatEm ( I, J) - Array of raw 3.7 micron window unfiltered radiance 
!                          calculated by the Fu-Liou radiative transfer 
!                          model and emulated to the width of the CERES
!                          instrument's window channel
!                             I = Pass through the model index
!                             J = Sky condition index
!
! !Revision History:
!  Version 4.0   August 18, 2006 - SCCR #634 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Changed IdxUV_Sfc variable to be REAL instead of INT4 
!  Version 4.0   August 7, 2004 - SCCR #550 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Added parameters to hold values for new UV parameters recorded by 
!                   Synoptic product  
!                2. Added parameter to hold values for vertical MATCH aerosols used in
!                   Synoptic product  
!  Version 3.6   January 16, 2004 - SCCR #498 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                 Transferred the following parameters from RadParams module:
!                 mxat, mxac, itranityp, sh_aer, a_taus, a_wlis
!                 a_taus_temp, n_atau, nac, itps  
!  Version 3.5   August 1, 2003 - SCCR #458
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Added Flag_CldNoAer array
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code 
!
! !END
!
!******************************************************************************
!
!
      USE ceres_defaults, ONLY : INT2_DFLT, INT4_DFLT, REAL4_DFLT
      USE f90_kind, ONLY : INT2
      USE Profile_Params
!
      IMPLICIT NONE
!
!
! ******************
! Processing Control
! ******************
!
! *** Parameters
!
      INTEGER, PARAMETER :: Flag_SkyCond_NO = 0
      INTEGER, PARAMETER :: Flag_SkyCond_YES = 1
      INTEGER, PARAMETER :: Flag_WLProc_NO = 0
      INTEGER, PARAMETER :: Flag_WLProc_YES = 1
!
!
! *** Variables that must be defined during processing, using above parameters
!
      INTEGER, PARAMETER :: Flag_ClrSky ( NumPass) = (/ Flag_SkyCond_YES,    &
                                                        Flag_SkyCond_YES /)
!
      INTEGER, PARAMETER :: Flag_CldSky ( NumPass) = (/ Flag_SkyCond_YES,    &
                                                        Flag_SkyCond_YES /)
!
      INTEGER, PARAMETER :: Flag_PrsSky ( NumPass) = (/ Flag_SkyCond_YES,    &
                                                        Flag_SkyCond_YES /)
!
      INTEGER, PARAMETER :: Flag_CldNoAer ( NumPass) = (/ Flag_SkyCond_YES,  &
                                                          Flag_SkyCond_YES /)
!
!
      INTEGER, PARAMETER :: Flag_ProfLW ( NumPass) = (/ Flag_WLProc_YES,     &
                                                        Flag_WLProc_YES /)
!
      INTEGER, PARAMETER :: Flag_ProfSW ( NumPass) = (/ Flag_WLProc_YES,     &
                                                        Flag_WLProc_YES /)
!
      INTEGER, PARAMETER :: Flag_ProfWN ( NumPass) = (/ Flag_WLProc_YES,     &
                                                        Flag_WLProc_YES /)
!
!
! **************
! Default Values
! **************
!
! *** Defined according to CERESlib values
!

      INTEGER (INT2), PARAMETER :: Default_INT2_FL = INT2_DFLT
      INTEGER,        PARAMETER :: Default_INT4_FL = INT4_DFLT
      REAL,           PARAMETER :: Default_REAL4_FL = REAL4_DFLT
!
!
!
! ******************************************************
! Miscellaneous variables defined once during processing
! ******************************************************
!
! *** Variables defined during processing
!
      INTEGER, SAVE :: Instr_Idx
!
!
! ********************************************************************
! Instrument and sample geometry variables required by Fu-Liou RT model
! ********************************************************************
!
! *** Variables defined during processing for each sample
!
      INTEGER, SAVE :: itp_scene
      REAL,    SAVE :: ssrna
      REAL,    SAVE :: ur
      REAL,    SAVE :: u0
!
!
! **********************************************
! Surface variables required by Fu-Liou RT model
! **********************************************
!
! *** Variables defined during processing for each sample for each pass
!
      REAL,    SAVE :: pts ( NumPass)
      REAL,    SAVE :: SfcEmiss ( NumPass, Num_BndSfcEm)            
      REAL,    SAVE :: SpSfcAlb ( NumPass, Num_BndSA, 0:MaxLev_Cld)
!
! *** Variables used in Synoptic processing
!
      REAL,    SAVE :: SpSfcAlb_Clear ( NumPass, Num_BndSA, 0:MaxLev_Cld)
      REAL,    SAVE :: SpSfcAlb_Pris  ( NumPass, Num_BndSA, 0:MaxLev_Cld)
!
!
! **************************************************************
! Atmospheric composition variables required by Fu-Liou RT model
! **************************************************************
!
      REAL, PARAMETER :: PristineAer = 0.0
      INTEGER, PARAMETER :: mxat= 7  
      INTEGER, PARAMETER :: mxac= 7    
      INTEGER, PARAMETER :: itranityp ( 5) = (/18, 10, 11, 1, 17/)
      REAL,    PARAMETER :: sh_aer ( 5) = (/ 5.0, 5.0, 2.0, 0.5, 1.0 /)
!
! *** Variables defined during processing for each sample for each pass
!
      REAL,    SAVE :: AerOptDepth ( NumPass)
      INTEGER, SAVE :: AerConstFlag ( mxac)
      REAL,    SAVE :: AerRatio ( NumPass)
      REAL,    SAVE :: aprof_match_vert (MaxLev_SARB, mxac)
      REAL,    SAVE :: a_taus ( mxat,mxac)
      REAL,    SAVE :: a_wlis ( mxat,mxac), a_taus_temp ( mxat,mxac)
      INTEGER, SAVE :: ModLev_Cnt ( 0:NumPass)
!      INTEGER*2, SAVE :: ModLev_Cnt ( 0:NumPass) !TOM-2025-0808
      REAL,    SAVE :: ModLev ( 0:NumPass, MaxLev_SARB)
      REAL,    SAVE :: ModLev_Temp ( 0:NumPass, MaxLev_SARB)
      REAL,    SAVE :: ModLev_SpHum ( 0:NumPass, MaxLev_SARB)
      REAL,    SAVE :: ModLev_ProO3 ( 0:NumPass, MaxLev_SARB)
      INTEGER, SAVE :: n_atau, nac, itps ( mxac)
!
!
! *****************************************************
! Cloud property variables required by Fu-Liou RT model
! *****************************************************
!
! *** Variables defined during processing for each sample for each pass
!
!
      INTEGER, SAVE :: CCLyr_Idx ( NumPass, MaxLev_Cld, 2) 
      REAL,    SAVE :: CF ( NumPass, 0:MaxLev_Cld)
      REAL,    SAVE :: IcePart_size ( NumPass, MaxLev_Cld) 
      REAL,    SAVE :: IW_content ( NumPass, MaxLev_Cld)
      REAL,    SAVE :: LiquidPart_size ( NumPass, MaxLev_Cld)
      REAL,    SAVE :: LW_content ( NumPass, MaxLev_Cld)
!
!
! *******************************************************
! Radiance and Flux variables defined by Fu-Liou RT model
! *******************************************************
!
! *** Variables defined during processing for each sample for each pass
!
!
      REAL,    SAVE :: Diff ( NumPass, NumSkyCond)
      REAL,    SAVE :: Dir ( NumPass, NumSkyCond)
      REAL,    SAVE :: DirDiff ( NumPass, NumSkyCond)
      REAL,    SAVE :: PAR ( NumPass, NumSkyCond)
      REAL,    SAVE :: TotLWrad ( NumPass, NumSkyCond)
      REAL,    SAVE :: WNflx_SatEm ( NumPass, NumSkyCond)
      REAL,    SAVE :: WNrad_RAW ( NumPass, NumSkyCond)
      REAL,    SAVE :: WNrad_SatEm ( NumPass, NumSkyCond)
      REAL,    SAVE :: WNurad_SatEm ( NumPass, NumSkyCond)
!
!
      REAL,    SAVE :: FL_TOA_LW ( NumPass, NumSkyCond)
      REAL,    SAVE :: FL_TOA_SW ( NumPass, NumSkyCond)
      REAL,    SAVE :: FL_TOA_WN ( NumPass, NumSkyCond)
!
!
      REAL,    SAVE :: LWProf ( NumPass, NumSkyCond, NumDir, MaxLev_SARB)
      REAL,    SAVE :: SWProf ( NumPass, NumSkyCond, NumDir, MaxLev_SARB)
      REAL,    SAVE :: WNProf ( NumPass, NumSkyCond, NumDir, MaxLev_SARB)
!
      REAL,    SAVE :: SW_TOAUP_UNCOR 
      REAL,    SAVE :: SW_SFCDN_UNCOR 
!
      REAL,    SAVE :: SW_TOAUP_CORR 
      REAL,    SAVE :: SW_SFCDN_CORR 
!
      REAL,    SAVE :: DnUVB_TOA ( NumPass, NumSkyCond) 
      REAL,    SAVE :: DnUVA_TOA ( NumPass, NumSkyCond) 
      REAL,    SAVE :: DnPAR_TOA ( NumPass, NumSkyCond) 
!
      REAL,    SAVE :: DnUVB_Sfc ( NumPass, NumSkyCond)       
      REAL,    SAVE :: DirDiff_UVB_Sfc ( NumPass, NumSkyCond)
      REAL,    SAVE :: DnUVA_Sfc ( NumPass, NumSkyCond)       
      REAL,    SAVE :: DirDiff_UVA_Sfc ( NumPass, NumSkyCond) 
      REAL,    SAVE :: PAR_Sfc ( NumPass, NumSkyCond)        
      REAL,    SAVE :: DirDiff_PAR_Sfc ( NumPass, NumSkyCond)
      REAL,    SAVE :: ActFlx_Sfc ( NumPass, NumSkyCond)
	  REAL,    SAVE :: Dir_SW_Sfc ( NumPass, NumSkyCond)
	  REAL,    SAVE :: Diff_SW_Sfc ( NumPass, NumSkyCond)
	  REAL,    SAVE :: Par_Purve_SW ( NumPass, NumSkyCond)
	  REAL,    SAVE :: DirDiff_Par_Purv ( NumPass, NumSkyCond)
	  REAL,    SAVE :: Par_ChlorA_SW ( NumPass, NumSkyCond)
	  REAL,    SAVE :: DirDiff_Par_ChlorA ( NumPass, NumSkyCond)     
!
      REAL,    SAVE :: IdxUV_Sfc ( NumPass, NumSkyCond) 
!
!
END MODULE FL_IO_Params




