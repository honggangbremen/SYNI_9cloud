!================================================================
MODULE CHLOR_CLIM
!
! !Revision History:
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Added IMPLICIT NONE statement to correct potential problems 
!                with uninitialized variables.  Made necessary local 
!                declarations of variables in subroutines.
!
USE SARB_IO_Utils
USE SynSARB_IO_Params
!
IMPLICIT NONE
!
INTEGER ios
INTEGER iunit_chlor
REAL, SAVE :: chlor1deg(360,180)
!
CONTAINS
!=============================================================
SUBROUTINE open_chlor_clim
!
USE SYN_IO_Utils
!
 CHARACTER (LEN = 15) :: Name_Sbr = 'open_chlor_clim'
 INTEGER :: VerID = 1
 INTEGER :: RECL = 360*180*4
!
CALL SARB_GenOpen (LID_Chlor, Flag_File_Read,      & ! I
           Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                              iunit_chlor,     & ! O
                                             VerID, RECL)    ! I (Opt)
!
READ(iunit_chlor , rec = 1, IOSTAT = ios) chlor1deg
IF ( ios /= 0 ) &
  CALL SARB_RdWrtError (LID_Chlor, ios, Name_Sbr, IRec = 1)
!
END SUBROUTINE open_chlor_clim
!=============================================================
!
END MODULE CHLOR_CLIM
!
!==============================================================
