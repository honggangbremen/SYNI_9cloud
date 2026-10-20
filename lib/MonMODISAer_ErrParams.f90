MODULE MonMODISAer_ErrParams
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    MonMODISAer_ErrParams
!
! !Description:
!    Routine ID - 
!
!    Purpose - Contains error message indicies for the Instantaneous SARB
!              Subsystem Aerosol Pre-Processor 
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
!    IErr_DayHDFRead_AngEx1Ocean
!    IErr_DayHDFRead_AngEx2Ocean
!    IErr_DayHDFRead_AngExLandQA
!    IErr_DayHDFRead_CorrTauMix 
!    IErr_DayHDFRead_EffAvOcean
!    IErr_DayHDFRead_RefLandQA66
!
! !Revision History:
!    Version 3.6   January 16, 2004 - SCCR #498 
!                  Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   Removed parameters for land optical depth and added
!                   parameter for land angstrom exponent
!    Version 3.5   April 22, 2003 - SCCR #437
!                  Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                   Added parameters for new MODIS land aerosol format
!    Version 3.4   December 20, 2002 - SCCR #408
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
! *** Indicies for errors encountered reading the daily HDF input files
!
      INTEGER, PARAMETER :: IErr_DayHDFRead_AngEx1Ocean  =  1
      INTEGER, PARAMETER :: IErr_DayHDFRead_AngEx2Ocean  =  2
      INTEGER, PARAMETER :: IErr_DayHDFRead_CorrTauMix   =  3
      INTEGER, PARAMETER :: IErr_DayHDFRead_AngExLandQA  =  4
      INTEGER, PARAMETER :: IErr_DayHDFRead_EffAvOcean   =  8
      INTEGER, PARAMETER :: IErr_DayHDFRead_RefLandQA66  =  9
      INTEGER, PARAMETER :: IErr_DayHDFRead_NotHDFFile   =  10
      INTEGER, PARAMETER :: IErr_DayHDFRead_OpenHDFFile  =  11
      INTEGER, PARAMETER :: IErr_DayHDFRead_CloseHDFFile =  12
      INTEGER, PARAMETER :: IErr_DayHDFRead_CorrTauLand  =  13
!
!
      INTEGER, PARAMETER :: IErr_AerHistDailyRead = 20
!
!
END MODULE MonMODISAer_ErrParams
