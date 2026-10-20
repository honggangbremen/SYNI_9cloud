MODULE SynSARB_IO_Params
!
!
!******************************************************************************
! !F90
!
! !Name: 
!   SynSARB_IO_Params
!
! !Description
!
!  Routine ID:
!
!  Purpose: This module groups together all of the LogicID values for 
!           the files used in subsystem 7.2.
!
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!
! !Revision History:
!  Version 5.0   May 27, 2020 - SCCR #1657
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Added LIDs for Entropy, pristine, and tune flags
!  Version X.X   August 29, 2005 - SCCR #XXX
!                Scott Zentz, s.m.zentz@larc.nasa.gov
!                Added LID_Assalut, Sat_Name
!  Version 4.0   August 16, 2004 - SCCR #550  
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Added new LIDs for Chlor, Emiss, ICE, Match_Vert_Profile,
!                   OverHead Albedo, SCRH, SeaIce, Snow, ZJIN, Zone 
!                2. Added LID for Satellite 
!  Version 3.4   June 20, 2003 - SCCR #445  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Release 3 code
!
!
! !END
!
!******************************************************************************
!
!
  INTEGER, PARAMETER :: FourStream_Ver = 2
  INTEGER, PARAMETER :: LID_AerCollinsBackup = 3
  INTEGER, PARAMETER :: LID_Assalut = 6
  INTEGER, PARAMETER :: LID_Chlor = 801
  INTEGER, PARAMETER :: LID_Collins = 4
  INTEGER, PARAMETER :: LID_DrivTab = 1
  INTEGER, PARAMETER :: LID_EmissMap_855 = 1101
  INTEGER, PARAMETER :: LID_EmissMap_108 = 1102
  INTEGER, PARAMETER :: LID_EmissMap_119 = 1103
  INTEGER, PARAMETER :: LID_Entropy = 604
  INTEGER, PARAMETER :: LID_FLSA_0404 = 1200
  INTEGER, PARAMETER :: LID_GrMODISAer = 5
  INTEGER, PARAMETER :: LID_HCM_OcnAlb = 19
  INTEGER, PARAMETER :: LID_ICE_North = 701
  INTEGER, PARAMETER :: LID_ICE_South = 702
  INTEGER, PARAMETER :: LID_ICE_IDX = 703
  INTEGER, PARAMETER :: LID_ICE_AFWA = 705
  INTEGER, PARAMETER :: LID_IGBP = 13
  INTEGER, PARAMETER :: LID_Instrument = 152
  INTEGER, PARAMETER :: LID_Match_Vert_Profile = 401
  INTEGER, PARAMETER :: LID_MOA = 1000
  INTEGER, PARAMETER :: LID_Month = 145                 ! DataMonth
  INTEGER, PARAMETER :: LID_63_OHead_SALB = 101
  INTEGER, PARAMETER :: LID_16_OHead_SALB = 102
  INTEGER, PARAMETER :: LID_Pristine = 605
  INTEGER, PARAMETER :: LID_QC = 57                     ! SYNQC_yyyymm
  INTEGER, PARAMETER :: LID_QCMCF = 251
  INTEGER, PARAMETER :: LID_QCMETA = 11201
  INTEGER, PARAMETER :: LID_RuleFile = 22
  INTEGER, PARAMETER :: LID_SALB = 17
  INTEGER, PARAMETER :: LID_Satellite = 151
  INTEGER, PARAMETER :: LID_SCRH = 100
  INTEGER, PARAMETER :: LID_SeaIce = 104
  INTEGER, PARAMETER :: LID_SfcAlb_LUT = 18
  INTEGER, PARAMETER :: LID_SigTab = 2
  INTEGER, PARAMETER :: LID_Sites = 30
  INTEGER, PARAMETER :: LID_SnowMap = 103 
  INTEGER, PARAMETER :: LID_SolConst = 23 
  INTEGER, PARAMETER :: LID_SYN = 27                    ! SYNI_yyyymm
  INTEGER, PARAMETER :: LID_SYN_MCF = 250
  INTEGER, PARAMETER :: LID_SYN_META = 11200
  INTEGER, PARAMETER :: LID_SYNS_MCF = 252
  INTEGER, PARAMETER :: LID_SYNS_META = 11202
  INTEGER, PARAMETER :: LID_SYN_Subset = 28                    ! SYNI_yyyymm
  INTEGER, PARAMETER :: LID_TSI = 12 
  INTEGER, PARAMETER :: LID_Tune = 606
  INTEGER, PARAMETER :: LID_Year = 144
  INTEGER, PARAMETER :: LID_ZJIN = 21
  INTEGER, PARAMETER :: LID_ZJIN_Ice = 1002
  INTEGER, PARAMETER :: LID_ZJIN_Ocean = 1004
  INTEGER, PARAMETER :: LID_ZJIN_Snow = 1001
  INTEGER, PARAMETER :: LID_ZONE = 160    
  INTEGER, PARAMETER :: TwoStream_Ver = 1
!
!************************************
!***** MISCELLANIOUS PARAMETERS *****
!************************************
  INTEGER, PARAMETER :: Intvl_Span = 1       ! Hours between runs
  INTEGER, PARAMETER :: NumHeaderRec_SYNI = 1
  INTEGER, PARAMETER :: NumHeaderRec_TSI = 0
  INTEGER, PARAMETER :: Num_Intvl_Day = 24
!  INTEGER, PARAMETER :: NumHr_MOAInt = 6     ! Hours between MOA files
  INTEGER, PARAMETER :: NumHr_MOAInt = 3     ! Hours between MOA files
  INTEGER, PARAMETER :: NumReg_Zone = 360
!
!
  INTEGER, SAVE :: IUnit_Aer
  INTEGER, SAVE :: IUnit_IGBP
  INTEGER, SAVE :: IUnit_GMOD
  INTEGER, SAVE :: IUnit_SolConst
  INTEGER, SAVE :: IUnit_TSI
  INTEGER, SAVE :: OUnit_QC
  INTEGER, SAVE :: OUnit_SYN
  INTEGER, SAVE :: OUnit_SYN_Subset
  INTEGER, SAVE :: OUnit_SYNVB
  INTEGER, SAVE :: RecLen_SYN
  INTEGER, SAVE :: RecLen_Subset
  INTEGER, SAVE :: RecLen_TSI
  INTEGER, SAVE :: Sites_Unit
  INTEGER, SAVE :: StreamID
!
!
  INTEGER, PARAMETER :: CldLow_TSI = 2
  INTEGER, PARAMETER :: CldMidLow_TSI = 3
  INTEGER, PARAMETER :: CldUpMid_TSI = 4
  INTEGER, PARAMETER :: CldHi_TSI = 5
  INTEGER, PARAMETER :: CldHi_UpMid_TSI = 6
  INTEGER, PARAMETER :: CldHi_MidLow_TSI = 7
  INTEGER, PARAMETER :: CldHi_Low_TSI = 8
  INTEGER, PARAMETER :: CldUpMid_LowMid_TSI = 9
  INTEGER, PARAMETER :: CldUpMid_Low_TSI = 10
  INTEGER, PARAMETER :: CldMidLow_Low_TSI = 11
  INTEGER, PARAMETER :: CldHi_SARB = 1
  INTEGER, PARAMETER :: CldUM_SARB = 2
  INTEGER, PARAMETER :: CldLM_SARB = 3
  INTEGER, PARAMETER :: CldLow_SARB = 4
  INTEGER, PARAMETER :: Clr_SARB = 0
  INTEGER, PARAMETER :: Clr_TSI  = 1
! 
  CHARACTER (LEN=5) :: Sat_Name  
!
!
END MODULE SynSARB_IO_Params


