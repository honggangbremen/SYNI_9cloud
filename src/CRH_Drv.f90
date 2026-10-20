MODULE PACKALB
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   PACKALB
!
! !Description:
!   Routine ID -
!
!   Purpose - 
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
!   unpka - 
!   pkalb -  
!        
!
!  Modules Used:
!                                            
!  External Files Used: 
!    
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
! *****************************************************************************

!
    CONTAINS
!
!***************************************************************************
!
  REAL FUNCTION unpka(bv)!! -128:127 --> 0-1.0
!
!***************************************************************************
!
! !F90
!
! !Name:
!   unpka 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - unpack
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!
    IMPLICIT NONE
    INTEGER ibvt 
    INTEGER*1 bv
!    
    IF ( bv < 0 ) THEN
      ibvt =256+bv
    ELSE
      ibvt=bv 
    ENDIF
! 
    unpka= 1.0 - LOG(1+ (255-ibvt)* 1.7182818/255.)
    IF( unpka < 0.001) unpka=0.0
! 
    RETURN

  END FUNCTION unpka
!
!***************************************************************************
!
  INTEGER*1 FUNCTION pkalb(alb)!! 0-1.0 --> 0:255
!
!***************************************************************************
!
! !F90
!
! !Name:
!   pkalb 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - pack
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!
    REAL alb
    
    pkalb=nint(255-(255./1.7182818)*(exp(1.-alb) -1.))
    
    RETURN
  
  END FUNCTION pkalb
!
!***************************************************************************
!
END MODULE PACKALB

!======================================================================

MODULE CREATECRHSARB
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   CREATECRHSARB
!
! !Description:
!   Routine ID -
!
!   Purpose - 
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
!   unpka - 
!   pkalb -  
!        
!
!  Modules Used:
!                                            
!  External Files Used: 
!    
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
! *****************************************************************************
    USE SYN_IO_Utils , only:  Fred_OpenFile, Fred_CloseFile
    USE PACKALB
    USE SARB_IO_Utils
    USE SynSARB_IO_Params
!    
    IMPLICIT NONE
!    
    INTEGER :: iunit
    INTEGER :: iunit_crh_in 
    INTEGER :: iunit_sno_ice_in
    INTEGER :: iunit_crh_out  
    INTEGER*2, ALLOCATABLE :: rawcrh(:) ! nregion
    INTEGER*1, ALLOCATABLE :: rawsnow(:) !nregion
    INTEGER, PARAMETER :: nregion = 2332800
!
!  PACKED SARB HISTORY ARRAY, TIME ORDERED...
    INTEGER*1 , ALLOCATABLE :: albhistory(:,:)  !!albhistory(31,nregion) 
    LOGICAL, SAVE :: nir_16_exists
!
    CONTAINS
!
!*******************************************************************************
!
  SUBROUTINE raw_sno_ice_file_input(iband,ida)
!
!***************************************************************************
!
! !F90
!
! !Name:
!   raw_sno_ice_file_input 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!
!
    IMPLICIT NONE
!
    INTEGER iband, ida, IRec
    INTEGER ios, LIDSI
    CHARACTER*50 :: Name_sbr = 'raw_sno_ice_file_input'
!
    INTEGER i255
    DATA i255 /255/      
    
    IF (iband == 3 ) LIDSI = LID_SnowMap
    IF (iband == 4 ) LIDSI = LID_SeaIce
!
    CALL Fred_OpenFile( iunit, ios, LogicID = LIDSI, VerID = ida, &
         ACCESS = 'direct', FORM = 'unformatted', ACTION = 'read',&
         RECL = nregion)
!   
    IF ( ios /= 0 ) THEN
      rawsnow(:) = INT( i255, kind=1)
    ELSE
      iunit_sno_ice_in = iunit
      IRec = 1
      READ  (UNIT = iunit_sno_ice_in, REC = IRec, IOSTAT = ios) rawsnow
!      
      IF ( ios /= 0 ) THEN
        CALL SARB_RdWrtError (LIDSI, ios, Name_sbr, IRec = IRec)
      ENDIF                      
      CALL Fred_CloseFile( iunit_sno_ice_in, ida )
    ENDIF
!
  END SUBROUTINE raw_sno_ice_file_input
!
!*******************************************************************************
!
  SUBROUTINE raw_crh_file_input(iband,ida)
!
!***************************************************************************
!
! !F90
!
! !Name:
!   raw_crh_file_input 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!    
    IMPLICIT NONE
!    
    INTEGER iband, ida, ios, LIDBAND, IRec
    CHARACTER*50 :: Name_sbr = 'raw_crh_file_input'
!  
    IF ((iband == 2 .OR. iband == 1) .AND. Sat_Name == 'TRMM') THEN
      rawcrh = 0.0
      RETURN
    ENDIF  
    
    IF ( iband == 2 ) LIDBAND = LID_16_OHead_SALB
    IF ( iband == 1 ) LIDBAND = LID_63_OHead_SALB
    
    CALL Fred_OpenFile( iunit, ios, LogicID = LIDBAND, VerID = ida, &
          ACCESS = 'direct', FORM = 'unformatted', ACTION = 'read', &
          RECL = nregion * 2)
!
    IF ( ios /= 0 ) THEN
      rawcrh = 0.0
    ELSE
      iunit_crh_in = iunit
      IRec = 1
      READ  (UNIT = iunit_crh_in, REC = IRec, IOSTAT = ios) rawcrh
!
      IF ( ios /= 0 ) THEN
        CALL SARB_RdWrtError (LIDBAND, ios, Name_sbr, IRec = IRec)
      ENDIF                      
      nir_16_exists = .TRUE.
      CALL Fred_CloseFile (iunit_crh_in, ida)
    ENDIF
!
  END SUBROUTINE raw_crh_file_input
!
!*******************************************************************************
!
  SUBROUTINE wopen_sarb_crh_history
!
!***************************************************************************
!
! !F90
!
! !Name:
!   wopen_sarb_crh_history 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!    
!    
    INTEGER :: RECL = 31 * nregion
    INTEGER :: VerID = 1
    CHARACTER (LEN = 22) :: Name_Sbr = 'wopen_sarb_crh_history'
!
    IF ( allocated(albhistory) )  DEALLOCATE ( albhistory )
    ALLOCATE ( albhistory(31,nregion) )
!
    CALL SARB_GenOpen (LID_SCRH, Flag_File_Write,                & ! I
                 Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                              iunit_crh_out,     & ! O
                                                   VerID, RECL)    ! I (Opt)
!    
  END SUBROUTINE wopen_sarb_crh_history
!
!*******************************************************************************
!
  SUBROUTINE create_sarb_crh_history(iband)
!
!***************************************************************************
!
! !F90
!
! !Name:
!   create_sarb_crh_history 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!        
    IMPLICIT NONE
!    
    INTEGER :: i, ios, ida, iband
    INTEGER :: LID_SCRH
    CHARACTER*50 :: Name_sbr = 'create_sarb_crh_history'
!
    IF ( allocated (rawcrh) ) DEALLOCATE (rawcrh)
    ALLOCATE ( rawcrh(nregion) )
    IF ( allocated (rawsnow) ) DEALLOCATE (rawsnow)
    ALLOCATE ( rawsnow(nregion) )

    DAYLOOP : DO ida = 1, 31  !!! 
      IF ( iband == 1 .OR. iband == 2 ) THEN
        CALL raw_crh_file_input(iband,ida)        
        DO i = 1, nregion 
          albhistory(ida, i) =   pkalb( rawcrh(i) / 1000. ) 
        END DO 
      ELSEIF ( iband == 3 .OR. iband == 4 ) THEN
        CALL raw_sno_ice_file_input(iband,ida)
        DO i = 1, nregion 
          albhistory(ida, i) =   rawsnow(i) 
	END DO 
      ENDIF
!
    END DO DAYLOOP
!
    DEALLOCATE (rawcrh)
    DEALLOCATE (rawsnow)
!    
    IF ( iband == 1 .OR. iband == 2 ) CALL time_interpolate_crh    !!! TIME INTERPOLATE ( 0.63&1.6)
!
    LID_SCRH = 100
    WRITE(UNIT = iunit_crh_out, REC = iband , IOSTAT = ios) albhistory
    IF ( ios /= 0 ) THEN
      CALL SARB_RdWrtError (LID_SCRH, ios, Name_sbr, IRec = iband)
    ENDIF
! 
  END SUBROUTINE create_sarb_crh_history
!
!*******************************************************************************
!
  SUBROUTINE time_interpolate_crh
!
!***************************************************************************
!
! !F90
!
! !Name:
!   time_interpolate_crh 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!       
    IMPLICIT NONE
!    
    INTEGER k, ida, ichange
    REAL omit
    REAL ts(31),tsx(31)
!    
    omit = -1.0
!
    REGIONS: DO k = 1, nregion
      tsx = -1.0
      ichange = 0
      DAY:  DO ida = 1, 31 !31
        ts(ida) =  unpka( albhistory(ida, k)  )
        IF ( ts(ida) == 0 ) ts(ida) = -1.0
!
        IF ( ida >= 2 ) THEN
          IF ( ts(ida) .NE. ts(ida - 1) )THEN
            ichange = ichange+1
            tsx(ida) = ts(ida)
          ENDIF  
        ENDIF
!
      END DO DAY
!
      CALL tintrp(tsx, 31, 1, omit)
!
      DO ida = 1, 31 ! Re-pack Interpolated Albedo Time Series.
        albhistory(ida, k) = pkalb (tsx(ida) )
      END DO
!
    END DO REGIONS
!
  END SUBROUTINE time_interpolate_crh
!
!*******************************************************************************
!
  SUBROUTINE tintrp(ara,nstep,idex,omit)
!
!***************************************************************************
!
! !F90
!
! !Name:
!   tintrp 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!
    IMPLICIT NONE
!
    INTEGER nstep, idex, igrd, is, ii, kk, j
    REAL omit, ync
    REAL ara(nstep,idex) 
!
    DO  kk = 1, idex
      igrd = 0
!            
      IF(ara(1, kk) .EQ. omit) THEN
        is = 0
41      is = is + 1
        IF(is .gt. nstep) RETURN ! go to 14
        IF(ara(is, kk) .EQ. omit) GO TO 41
        ara(1, kk) = ara(is, kk)
      END IF
!  
      IF(ara(nstep, kk) .EQ. omit) THEN
        is = nstep + 1
42      is = is - 1
        IF(is .le. 0) RETURN
        IF(ara(is, kk) .EQ. omit) GOTO 42
        ara(nstep, kk) = ara(is, kk)
      END IF
!
      DO ii = 1, nstep
        IF(ara(ii,kk).EQ.omit) THEN    !!  check for holes
          igrd = igrd + 1                    !!# of adjacent holes.
        ELSE
          IF(igrd .NE. 0) THEN
            ync=( ara(ii - (igrd + 1), kk) - ara(ii, kk) ) / float(igrd + 1)
!    
            DO  j = 1, igrd
              ara(ii - j, kk) = ara(ii,kk) + (float(j) * ync) !interpolate
            END DO
!     
            igrd = 0      ! reset hole counter
          END IF
        END IF
      END DO
    END DO
!
    RETURN     

  END SUBROUTINE tintrp
!
!*******************************************************************************
!
END MODULE CREATECRHSARB

!================================================================

SUBROUTINE Driver_Create
!
!***************************************************************************
!
! !F90
!
! !Name:
!   Driver_Create 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
!
    USE CREATECRHSARB
    USE SYN_IO_Utils 
!    
    IMPLICIT NONE
!    
    CALL Fred_Get_Files_From_PCF_List
!
    CALL wopen_sarb_crh_history

!! 1=0.63umOHSAlb 2=1.6umOHSAlb 3=SnowMap(y/n) 4=SeaIceMap(%)

    CALL create_sarb_crh_history(1) 
    nir_16_exists = .FALSE.
    CALL create_sarb_crh_history(2)
    CALL create_sarb_crh_history(3)
    CALL create_sarb_crh_history(4)
!
    DEALLOCATE( albhistory )
!
    CALL Fred_CloseFile (iunit_crh_out)
!
  END SUBROUTINE Driver_Create
