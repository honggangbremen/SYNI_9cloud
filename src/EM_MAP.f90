MODULE EM_MAP
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   EM_MAP
!
! !Description:
!
!    Purpose - Read and process emissivity maps
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
!  Internal Routines:
!    open_em_maps - Opens emissivity maps
!    rd_em_maps   - Reads emissivity maps 
!    ga_emis      - Gets emissivity for specific location 
!
!  Modules Used:
!   CRHSARB            
!   SynSARB_IO_Params  
!   SARB_IO_Utils      
!   SYN_IO_Utils       
!
! !Revision History:
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Added IMPLICIT NONE statement to correct potential problems 
!                with uninitialized variables.  Made necessary local 
!                declarations of variables in subroutines.
!
! !END
!
!*****************************************************************************
!
!
!
 USE SYN_IO_Utils ,only:  Fred_OpenFile
 USE CRHSARB, only: find_10m_lon
 USE SARB_IO_Utils
 USE SynSARB_IO_Params
!
 IMPLICIT NONE
!
 INTEGER, PARAMETER :: nrzone = 2160*6
 INTEGER*2 ,ALLOCATABLE :: iemmap(:,:,:) 
 INTEGER iunit_em1,iunit_em2,iunit_em3
 CHARACTER (LEN=3) :: Emiss855
 REAL emis_gridavg(3)

 CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 SUBROUTINE open_em_maps
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   open_em_maps
!
! !Description:
!    Purpose - Opens emissivity maps
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
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
 CHARACTER*120 :: Name_Sbr = 'open_em_maps'
 INTEGER :: RECL = nrzone*2
 INTEGER :: VerID = 1

 IF (Sat_Name /= 'TRMM') THEN
!
  CALL SARB_GenOpen (LID_EmissMap_855, Flag_File_Read,               & ! I
                     Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
    		     iunit_em1, 	                             & ! O
    			                            VerID, RECL)       ! I (Opt)
  Emiss855 = 'YES'
 ELSE
  Emiss855 = 'NO'
 ENDIF
!
 CALL SARB_GenOpen (LID_EmissMap_108, Flag_File_Read,               & ! I
                    Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                           iunit_em2,               & ! O
                                         VerID, RECL)                ! I (Opt)

 CALL SARB_GenOpen (LID_EmissMap_119, Flag_File_Read,               & ! I
                    Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                           iunit_em3,               & ! O
                                          VerID, RECL)                ! I (Opt)
!
 END SUBROUTINE open_em_maps
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 SUBROUTINE rd_em_maps(ilat)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   rd_em_maps
!
! !Description:
!    Purpose - Reads emissivity maps
!
! 
! !Input Parameters:
!    ilat
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
 INTEGER, INTENT (IN) :: ilat

 CHARACTER*120 :: Name_Sbr = 'rd_em_maps'
 INTEGER :: ios

 IF ( .NOT. allocated ( iemmap ) ) ALLOCATE (iemmap(2160,6,3))

 IF (Emiss855 == 'YES') THEN
   READ ( iunit_em1 ,REC = ilat ,IOSTAT=ios) iemmap(:,:,1)
   IF ( ios /= 0 ) &
     CALL SARB_RdWrtError (LID_EmissMap_855, ios, Name_Sbr, IRec = ilat)
 ELSE
   iemmap(:,:,1) = 0.0
 ENDIF

 READ ( iunit_em2 ,REC = ilat ,IOSTAT=ios) iemmap(:,:,2)
 IF ( ios /= 0 ) &
   CALL SARB_RdWrtError (LID_EmissMap_108, ios, Name_Sbr, IRec = ilat)

 READ ( iunit_em3 ,REC = ilat ,IOSTAT=ios) iemmap(:,:,3)
 IF ( ios /= 0 ) &
   CALL SARB_RdWrtError (LID_EmissMap_119, ios, Name_Sbr, IRec = ilat)

 END SUBROUTINE rd_em_maps
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 SUBROUTINE ga_emis(ilon)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   ga_emis
!
! !Description:
!    Purpose - Gets emissivity for specific location
!
! 
! !Input Parameters:
!    ilon
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #nnn
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
 INTEGER ilon, ix1,ix2,kk
 REAL  em1(36)
!
 CALL find_10m_lon(ilon, ix1,ix2)
!
 DO kk = 1,3 ! 1=10.8,2=11.9
   em1(1:36) =  RESHAPE( iemmap(ix1:ix2,1:6,kk) ,(/36/) ) * 0.001 
   emis_gridavg(kk) = sum (em1)/36.
 END DO
!
 END SUBROUTINE ga_emis
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE EM_MAP
