MODULE SYN_IO_Utils
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SYN_IO_Utils
!
! !Description:
!
!    Purpose - Contains routines for opening and closing files
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
!    Fred_OpenFile                - Opens files using SARB open interface
!    Fred_CloseFile               - Closes files using SARB close interface 
!    Fred_Get_Files_From_PCF_List - Gets filenames from PCF 
!
!  Modules Used:
!   SARB_Var           - (SARBlib)  Used by Sbr. Fred_OpenFile, Fred_CloseFile
!   PCF                - (CERESlib) Used by Sbr. Fred_Get_Files_From_PCF_List
!   SynSARB_IO_Params  - (SARBlib)  Used by Sbr. Fred_OpenFile, Fred_CloseFile
!   SARB_IO_Utils      - (SARBlib)  Used by Sbr. Fred_OpenFile, Fred_CloseFile
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
    USE SARB_IO_Utils
    USE pcf
    USE SARB_Var
    USE SynSARB_IO_Params
!
    IMPLICIT NONE
! 
! 
    INTEGER, SAVE :: nfiles
!
    TYPE MPCF
      SEQUENCE
      INTEGER :: lid
      INTEGER :: uid
      CHARACTER*200 :: filename
      INTEGER :: vid
    END TYPE MPCF
!
    TYPE (MPCF), SAVE :: fpcf(1000)
!
!
    CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE Fred_Get_Files_From_PCF_List
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Fred_Get_Files_From_PCF_List
!
! !Description:
!    Purpose - Gets file names from PCF
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
!    PCF      - (CERESlib) Sbr. GetFileName
!                                 
!  Input Files:
!    NONE
!
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Fix assignment of Logical file Id's (ilid = LogIDS(k)) in 
!                creation of CRH inputs from Snow/Ice/(0.63/1.6umOHS Albedo)
!                Corrects missing sea ice over poles
!
! !END
!
!*****************************************************************************
!
!
    INTEGER :: ilid, NumDays_Mon, IOS
    INTEGER :: Day, i, k
    INTEGER :: IncPath_Yes = 1

    INTEGER, DIMENSION(4) :: LogIDS
    LOGICAL :: File_EX = .FALSE.
    CHARACTER*200 :: FName
!  
    NumDays_Mon = DaysPerMonth(DataMonth, DataYear)
    nfiles = 0
    i = 1
    k = 1
    Day = 1
! 
    LogIDS(1:4) = (/ LID_63_OHead_SALB, LID_16_OHead_SALB, &
                  LID_SnowMap, LID_SeaIce /)
!      
    DO k = 1, 4  
     ilid = LogIDS(k)
!  
     DO Day = 1, NumDays_Mon
      CALL GetFileName (ilid,                                & ! I
                        FName, IOS,                          & ! O
                        Version = Day, incPath = IncPath_Yes ) ! I
!
      IF (IOS == 0) THEN
!          *** Successful file name retrieval. Determine if file exists.
!
       INQUIRE (FILE = FName, EXIST = File_EX, IOSTAT = IOS)
!
       IF (File_EX .EQV. .TRUE.) THEN 
         fpcf(i) % uid      = i + 200
         fpcf(i) % filename = FName
         fpcf(i) % vid      = Day
         fpcf(i) % lid = ilid
         nfiles = nfiles + 1
         i = i + 1
       ENDIF    
      ENDIF 
     
     END DO
    
    END DO
! 
END SUBROUTINE Fred_Get_Files_From_PCF_List
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE Fred_OpenFile(iunit, ios, LogicID, VerID, &
                         recl, action, access, form)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Fred_OpenFile
!
! !Description:
!    Purpose - Opens files using SARB interface
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
!
! !END
!
!*****************************************************************************
!    
    INTEGER k, i
    INTEGER, INTENT (OUT) :: iunit, ios
    INTEGER :: LogicID, VerID, Unit_Num
    INTEGER, OPTIONAL :: recl  
    CHARACTER*(*) :: action, access, form
    CHARACTER :: Name_Sbr
!     
    Name_Sbr = 'Fred_OpenFile ()'
    i = 0
    DO k = 1, nfiles
     IF ( LogicID == fpcf(k) % lid .AND. VerID == fpcf(k) % vid) THEN
      i=k
      EXIT
     ENDIF
    END DO
!
    IF ( i == 0 ) THEN
      ios = 1
      RETURN 
    ENDIF 
!
    CALL SARB_GenOpen (LogicID, Flag_File_Read, Flag_File_Direct, &
    	               Flag_File_Unformat, Name_Sbr,	          & ! I
    		       Unit_Num,                                  & ! O
    		       VerID, recl)                                 ! I 
!    
    iunit = Unit_Num
    ios = 0           
!  
END SUBROUTINE Fred_OpenFile
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE Fred_CloseFile (Unit_Num,         & ! I
                              VerID)           ! I (Opt)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Fred_CloseFile
!
! !Description:
!    Purpose - Closes files using SARB interface
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
!
! !END
!
!*****************************************************************************
!
!
    CHARACTER :: Name_Sbr
    INTEGER, OPTIONAL :: VerID
    INTEGER :: Unit_Num
!
    Name_Sbr = "Fred_CloseFile"
!    
    IF (PRESENT(VerID)) THEN
      CALL SARB_GenClose (Name_Sbr, Unit_Num, VerID)
      RETURN      
    ELSE 
      CALL SARB_GenClose (Name_Sbr, Unit_Num)
    END IF
!    
!    
END SUBROUTINE Fred_CloseFile
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SYN_IO_Utils
