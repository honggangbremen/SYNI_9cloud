MODULE SARB_IO_Utils
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SARB_IO_Utils 
!
! !Description:
!   Routine ID -
!
!   Purpose - Provides CERES-specific routines for opening and closing files
!             and retrieving run-time parameters from the PCF
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
!  Key Local Parameters:
!    ProcessContinue    - Flag indicating to continue processing after 
!                         generating error message
!    ProcessTerminate   - Flag indicating to terminate processing after 
!                         generating error message
!    Flag_File_Direct   - Flag indicating a file is direct access
!    Flag_File_Format   - Flag indicating a file is formatted
!    Flag_File_Read     - Flag indicating a file is opened in read mode
!    Flag_File_Seq      - Flag indicating a file is sequential access
!    Flag_File_Unformat - Flag indicating a file is unformatted
!    Flag_File_Write    - Flag indicating a file is opened in write mode
!
!  Internal Routines
!    DaysPerMonth            - Returns the number of days in a specified month
!    DetFileExist            - Determines whether or not a specified file 
!                              exists
!    RunTimeVar_DateRetrieve - Retrieves date-related run-time parameters 
!                              from the PCF
!    RunTimeVar_SatRetrieve  - Retrieves instrument-releated run-time 
!                              parameters from the PCF
!    SARB_GenClose           - Closes files using the CERESlib wrappers for
!                              the ECS Toolkit
!    SARB_GenOpen            - Opens files using the CERESlib wrappers for  
!                              the ECS Toolkit
!    SARB_RdWrtError         - Issues read/write failure error messages 
!                              using the CERESlib wrappers for the ECS 
!                              Toolkit routines
!
!  Modules Used:
!   ceres_status - (CERESlib) Used by Module Main body
!   io           - (CERESlib) Used by Sbr. SARB_GenClose, SARB_GenOpen
!   msg          - (CERESlib) Used by Sbr. DetFileExist, 
!                                          RunTimeVar_DateRetrieve,
!                                          RunTimeVar_SatRetrieve,
!                                          SARB_GenClose, SARB_GenOpen,
!                                          SARB_RdWrtError
!   pcf          - (CERESlib) Used by Sbr. DetFileExist, 
!                                          RunTimeVar_DateRetrieve,
!                                          RunTimeVar_SatRetrieve
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Used by Sbr. DetFileExist, 
!                                          RunTimeVar_DateRetrieve,
!                                          RunTimeVar_SatRetrieve,
!                                          SARB_GenClose, SARB_GenOpen,
!                                          SARB_RdWrtError
!
! !Revision History
!  Version 3.7  January 12, 2004 - SCCR #498 
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Added Sbr. SARB_RdWrtError  
!                2. Added parameters ProcessContinue, ProcessTerminate
!  Version 3.5  August 1, 2003 - SCCR #458 
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Added Sbr. RunTimeVar_SatRetrieve
!  Version 3.4  December 20, 2002 - SCCR #408
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Initial version of code
!
! !END
!
!******************************************************************************
!
!
      USE ceres_status, ONLY: DIRECT_FILE, FMT_FILE, READ_FILE,   &
                              SEQ_FILE, UNFMT_FILE, WRITE_FILE
      USE io, ONLY: CloseFile, OpenFile
      USE msg, ONLY: WriteReport
      USE pcf, ONLY: GetFileName, GetParam
!
!
      IMPLICIT NONE
!
      INCLUDE 'PGS_SARBIOUTIL_25750.f'
!
      CHARACTER (LEN=1), PARAMETER :: Flag_File_Direct = DIRECT_FILE
      CHARACTER (LEN=1), PARAMETER :: Flag_File_Format = FMT_FILE
      CHARACTER (LEN=1), PARAMETER :: Flag_File_Read = READ_FILE
      CHARACTER (LEN=1), PARAMETER :: Flag_File_Seq = SEQ_FILE
      CHARACTER (LEN=1), PARAMETER :: Flag_File_Unformat = UNFMT_FILE
      CHARACTER (LEN=1), PARAMETER :: Flag_File_Write = WRITE_FILE
!
!
      INTEGER, PARAMETER :: ProcessContinue = 0
      INTEGER, PARAMETER :: ProcessTerminate = 1
!
!
      CONTAINS
!   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   
      SUBROUTINE SARB_GenOpen (LID_File, ModeCode, AccessCode,     & ! I
                               FormatCode, Name_Sbr,               & ! I
                                                     Unit_Num,     & ! O
                                            Vers, RLength)           ! I (Opt)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SARB_GenOpen 
!
! !Description:
!   Routine ID -
!
!   Purpose - Opens files using the CERESlib wrappers for the ECS Toolkit
!             routines
!
!
! !Input Parameters:
!   AccessCode - Character flag indicating whether to open the file with direct
!                or sequential access
!   FormatCode - Character flag indicating whether to open the file as formatted
!                or unformatted
!   LID_File   - Integer logical identifier into PCF for file 
!   ModeCode   - Character flag indicating whether to open the file in read
!                or write mode 
!   Name_Sbr   - Character name of calling subroutine
!   RLength    - Record length required for the opening of direct access files
!   Vers       - Optional integer file version number used by the ECS 
!                Toolkit open routine
!
! !Output Parameters:
!   Unit_Num - Integer unit number assigned by the Toolkit
!
! !Team-Unique Header:
!
!  Modules Used:
!   io  - (CERESlib) Sbr: OpenFile
!   msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Params: SARBIOUTILS_E_FILE_OPEN_ERROR
!
! !Revision History
!  Version 3.4  December 20, 2002 - SCCR #408
!            Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!            Initial version of code
!
! !END
!
!******************************************************************************
!
!
      CHARACTER (LEN=*), INTENT (IN)  :: AccessCode
      CHARACTER (LEN=*), INTENT (IN)  :: FormatCode
      INTEGER,           INTENT (IN)  :: LID_File
      CHARACTER (LEN=*), INTENT (IN)  :: ModeCode
      CHARACTER (LEN=*), INTENT (IN)  :: Name_Sbr
      INTEGER, OPTIONAL, INTENT (IN)  :: RLength
      INTEGER,           INTENT (OUT) :: Unit_Num
      INTEGER, OPTIONAL, INTENT (IN)  :: Vers
!
      CHARACTER (LEN=41) :: CString
      INTEGER            :: IOS
      INTEGER            :: RLen
      INTEGER            :: VersNum
!
!
      IF ( PRESENT (Vers)) THEN
!     *** Use version number supplied by calling routine
!
          VersNum = Vers
!
      ELSE 
!     *** Assume only one version
!
          VersNum = 1
!
      END IF
!
!
      IF (Present (RLength)) THEN
!     *** Use record length provided by calling routine
!
          RLen = RLength
!
      ELSE
!     *** Set record length to zero
!
          RLen = 0
!
          IF (AccessCode == Flag_File_Direct) THEN
!         *** Cannot open direct access file without record length.
!             Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_NO_DA_REC_LENGTH,      & ! I 
                                FuncName = Name_Sbr)                  ! I
!
          END IF
!
      END IF
!
!
      CALL OpenFile (LogicID = LID_File, status = IOS,               & ! I 
                     handle = Unit_Num,                              & ! O
                     mode = ModeCode, access = AccessCode,           & ! I
                     format = FormatCode, reclen = RLen,             & ! I
                     version = VersNum)                                ! I
!
      IF (IOS /= 0) THEN
!     *** Error opening file.  Terminate processing.
!
          CString = 'IOS = xxxx   LID = xxxx   VERSION = xxxx '
          WRITE (CString(7:10), '(i4)') IOS
          WRITE (CString(20:23), '(i4)') LID_File
          WRITE (CString(37:40), '(i4)') VersNum
!
          CALL WriteReport (SARBIOUTIL_E_FILE_OPEN_ERROR,            & ! I
                            FuncName =  Name_Sbr,                    & ! I
                            StrVar = CString)                          ! I
!
      END IF
!
!
      END SUBROUTINE SARB_GenOpen
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 
      SUBROUTINE SARB_GenClose (Name_Sbr, Unit_Num,               & ! I
                                                      Vers)           ! I (Opt)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SARB_GenOpen 
!
! !Description:
!   Routine ID -
!
!   Purpose - Closes files using the CERESlib wrappers for the ECS Toolkit
!             routines
!
!
! !Input Parameters:
!   Name_Sbr - Character name of calling subroutine
!   Unit_Num - Integer unit number assigned by the Toolkit
!   Vers     - Optional integer PCF LID version number
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!   io  - (CERESlib) Sbr: CloseFile
!   msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Params: SARBIOUTIL_E_FILE_CLOSE_ERROR
!
! !Revision History
!  Version 3.4 - December 20, 2002 - SCCR #408
!            Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!            Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER,           INTENT (IN), OPTIONAL :: Vers
      CHARACTER (LEN=*), INTENT (IN)           :: Name_Sbr
      INTEGER,           INTENT (IN)           :: Unit_Num
!
      CHARACTER (LEN=41) :: CString
      INTEGER            :: IOS
      INTEGER            :: VersNum
!
!
      CALL CloseFile (Unit_Num,                                      & ! I
                                IOS)                                   ! O 
!
      IF (IOS /= 0) THEN
!     *** Error closing file
!
          IF ( PRESENT (Vers)) THEN
!         *** Use version number supplied by calling routine
!
              VersNum = Vers
!
          ELSE 
!         *** Assume only one version
!
              VersNum = 1
!
          END IF
!
          CString = "IOS = xxxx   Unit = xxxx   LID Ver = xxxx"
          WRITE (CString(7:10),'(i4)') IOS
          WRITE (CString(21:24),'(i4)') Unit_Num
          WRITE (CString(38:41),'(i4)') VersNum
!
          CALL WriteReport (SARBIOUTIL_E_FILE_CLOSE_ERROR,            & ! I
                            FuncName =  Name_Sbr,                     & ! I
                            StrVar = CString)                           ! I
!
      END IF 
! 
!
      END SUBROUTINE SARB_GenClose
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE DetFileExist (LID_File, Name_Sbr,                  & ! I
                                                   FileExist,       & ! O
                                             Vers,                  & ! I (Opt)
                                                   FileName)          ! O (Opt)
!
!******************************************************************************
!
! !F90
!
! !Name:
!    DetFileExist
!
! !Description:
!   Routine ID -
!
!   Purpose - Determines whether or not a specified file exists 
!
!
! !Input Parameters:
!   Vers     - Optional integer PCF LID version number
!   LID_File - PCF LID version
!   Name_Sbr - Character name of the calling routine
!
! !Output Parameters:
!   FileExist - Logical flag indicating whether or not the specified 
!               file exists
!   FileName  - Optional character variable for the filename
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr. WriteReport
!    pcf - (CERESlib) Sbr. GetFileName
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Parameters: SARBIOUTIL_E_DET_FNAME_EXIST,
!                                         SARBIOUTIL_E_RETRIEVE_FNAME
!
! !Revision History
!  Version 3.4 December 20, 2002 - SCCR #408
!            Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!            Initial version of code
!
! !END
!
!******************************************************************************
!
!
      LOGICAL,           INTENT (OUT)           :: FileExist
      CHARACTER (LEN=*), INTENT (OUT), OPTIONAL :: FileName
      INTEGER,           INTENT (IN)            :: LID_File
      CHARACTER (LEN=*), INTENT (IN)            :: Name_Sbr
      INTEGER,           INTENT (IN),  OPTIONAL :: Vers
!
      CHARACTER (LEN=200)           :: FName
      CHARACTER (LEN=41)            :: MsgStr
      INTEGER,            PARAMETER :: IncPath_Yes = 1
      INTEGER                       :: IOS
      INTEGER                       :: IVer
!
!
      IF (PRESENT (Vers)) THEN
!     *** Use version number supplied by the calling routine
!
          IVer = Vers
!
      ELSE
!     *** Assume only one version
!
          IVer = 1
!
      END IF
!
!
! *** Assign initial value for error message string
!
      MsgStr = 'IOS = xxxx   LID = xxxx   Version = xxxx '
      WRITE (MsgStr (20:23), '(i4)') LID_File
      WRITE (MsgStr (37:40), '(i4)') IVer
!
!
! *** Determine whether hourly file exists
!
      CALL GetFileName (LID_File,                               & ! I
                        FName, IOS,                             & ! O
                        Version = IVer, incPath = IncPath_Yes)    ! I
!
      IF (IOS == 0) THEN
!     *** Successful file name retrieval
!
          INQUIRE (FILE=FName, EXIST=FileExist, IOSTAT=IOS)
!
          IF (IOS /= 0) THEN
!         *** Error encountered determining whether file exists.
!             Terminate processing.
!
              WRITE (MsgStr (7:10), '(i4)') IOS
!
              CALL WriteReport (SARBIOUTIL_E_DET_FNAME_EXIST,   & ! I
                                FuncName = Name_Sbr,            & ! I
                                StrVar = MsgStr)                  ! I
!    
          END IF
!
          IF (PRESENT (FileName)) FileName = FName
!
      ELSE
!     *** File name read error.  Terminate processing.
!
          WRITE (MsgStr (7:10), '(i4)') IOS
!
          CALL WriteReport (SARBIOUTIL_E_RETRIEVE_FNAME,       & ! I
                            FuncName = Name_Sbr,               & ! I
                            StrVar = MsgStr)                     ! I
!
      END IF
!
!
      END SUBROUTINE DetFileExist
!   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE RunTimeVar_DateRetrieve (LID_Hr, LID_Dy,            & ! I (Opt)
                                          LID_Mn, LID_Yr,            & ! I (Opt)
                                          IHr, IDy, IMn, IYr,        & ! O (Opt)
                                          CHr, CDy, CMn, CYr)          ! O (Opt)
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    RunTimeVar_DateRetrieve
!
! !Description:
!    Routine ID - 
!
!    Purpose - Retrieves date-related and instrument-releated 
!              run-time parameters from the PCF
!
! 
! !Input Parameters:
!   LID_Dy   - PCF LID for Day run-time parameter
!   LID_Hr   - PCF LID for Hour run-time parameter
!   LID_Mon  - PCF LID for Month run-time parameter
!   LID_Yr   - PCF LID for Year run-time parameter
!
! !Output Parameters:
!   CDy   - Character format of Day run-time variable retrieved from PCF
!   CHr   - Character format of Hour run-time variable retrieved from PCF
!   CMon  - Character format of Month run-time variable retrieved from PCF
!   CYr   - Character format of Year run-time variable retrieved from PCF
!   IDy   - Integer format of Day run-time variable retrieved from PCF
!   IHr   - Integer format of Hour run-time variable retrieved from PCF
!   IMon  - Integer format of Month run-time variable retrieved from PCF
!   IYr   - Integer format of Year run-time variable retrieved from PCF
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr. WriteReport
!    pcf - (CERESlib) Sbr. GetParam
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Params: SARBIOUTIL_E_MISSING_LID_DY,
!                                     SARBIOUTIL_E_MISSING_LID_HR,
!                                     SARBIOUTIL_E_MISSING_LID_MN,
!                                     SARBIOUTIL_E_MISSING_LID_YR,
!                                     SARBIOUTIL_E_PCF_RETRIEVE_DY,
!                                     SARBIOUTIL_E_PCF_RETRIEVE_HR,
!                                     SARBIOUTIL_E_PCF_RETRIEVE_MN,
!                                     SARBIOUTIL_E_PCF_RETRIEVE_YR
!
! !Revision History:
!  Version 3.4  December 20, 2002 - SCCR #408
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER,           INTENT (IN),  OPTIONAL :: LID_Dy
      INTEGER,           INTENT (IN),  OPTIONAL :: LID_Hr
      INTEGER,           INTENT (IN),  OPTIONAL :: LID_Mn
      INTEGER,           INTENT (IN),  OPTIONAL :: LID_Yr
      CHARACTER (LEN=2), INTENT (OUT), OPTIONAL :: CDy
      CHARACTER (LEN=2), INTENT (OUT), OPTIONAL :: CHr
      CHARACTER (LEN=2), INTENT (OUT), OPTIONAL :: CMn
      CHARACTER (LEN=4), INTENT (OUT), OPTIONAL :: CYr
      INTEGER,           INTENT (OUT), OPTIONAL :: IDy
      INTEGER,           INTENT (OUT), OPTIONAL :: IHr
      INTEGER,           INTENT (OUT), OPTIONAL :: IMn
      INTEGER,           INTENT (OUT), OPTIONAL :: IYr
!
      CHARACTER (LEN=4)             :: CTemp4
      CHARACTER (LEN=6)             :: CTemp6
      INTEGER                       :: IStat
      CHARACTER (LEN=26), PARAMETER :: Name_Sbr = 'RunTimeVar_DateRetrieve ()'
! 
!
      IF (PRESENT (IHr)  .OR.  PRESENT (CHr)) THEN
!     *** Hour output is requested
!
          IF (PRESENT (LID_Hr)) THEN
!         *** All necessary information provided by calling routine
!
              CALL GetParam (LID_Hr, CTemp4, IStat)
!
              IF (IStat == 0) THEN
!
                  IF (PRESENT (IHr)) READ (CTemp4(2:3),'(i2)') IHr
                  IF (PRESENT (CHr)) READ (CTemp4(2:3),'(a2)') CHr
!
              ELSE
!             *** Run-time parameter retrieval error.  Terminate processing.
!
                  CALL WriteReport (SARBIOUTIL_E_PCF_RETRIEVE_HR,        & ! I
                                    FuncName = Name_Sbr,                 & ! I
                                    StrVar = " IStat = (%i4) ",          & ! I
                                    IVar1=IStat)                           ! I
!
              END IF
!
          ELSE
!         *** Necessary information missing.  Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_MISSING_LID_HR,             & ! I
                                FuncName = Name_Sbr)                       ! I
!
          END IF
!
      END IF
!
!
      IF (PRESENT (IDy)  .OR.  PRESENT (CDy)) THEN
!     *** Day output is requested
!
          IF (PRESENT (LID_Dy)) THEN
!         *** All necessary information provided by calling routine
!
              CALL GetParam (LID_Dy, CTemp4, IStat)
!
              IF (IStat == 0) THEN
!
                  IF (PRESENT (IDy)) READ (CTemp4(2:3),'(i2)') IDy
                  IF (PRESENT (CDy)) READ (CTemp4(2:3),'(a2)') CDy
!
              ELSE
!             *** Run-time parameter retrieval error.  Terminate processing.
!
                  CALL WriteReport (SARBIOUTIL_E_PCF_RETRIEVE_DY,        & ! I
                                    FuncName = Name_Sbr,                 & ! I
                                    StrVar = " IStat = (%i4) ",          & ! I
                                    IVar1=IStat)                           ! I
!
              END IF
!
          ELSE
!         *** Necessary information missing.  Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_MISSING_LID_DY,             & ! I
                                FuncName = Name_Sbr)                       ! I
!
          END IF
!
      END IF
!
!
      IF (PRESENT (IMn)  .OR.  PRESENT (CMn)) THEN
!     *** Day output is requested
!
          IF (PRESENT (LID_Mn)) THEN
!         *** All necessary information provided by calling routine
!
              CALL GetParam (LID_Mn, CTemp4, IStat)
!
              IF (IStat == 0) THEN
!
                  IF (PRESENT (IMn)) READ (CTemp4(2:3),'(i2)') IMn
                  IF (PRESENT (CMn)) READ (CTemp4(2:3),'(a2)') CMn
!
              ELSE
!             *** Run-time parameter retrieval error.  Terminate processing.
!
                  CALL WriteReport (SARBIOUTIL_E_PCF_RETRIEVE_MN,        & ! I
                                    FuncName = Name_Sbr,                 & ! I
                                    StrVar = " IStat = (%i4) ",          & ! I
                                    IVar1=IStat)                           ! I
!
              END IF
!
          ELSE
!         *** Necessary information missing.  Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_MISSING_LID_MN,             & ! I
                                FuncName = Name_Sbr)                       ! I
!
          END IF
!
      END IF
!
!
      IF (PRESENT (IYr)  .OR.  PRESENT (CYr)) THEN
!     *** Day output is requested
!
          IF (PRESENT (LID_Yr)) THEN
!         *** All necessary information provided by calling routine
!
              CALL GetParam (LID_Yr, CTemp6, IStat)
!
              IF (IStat == 0) THEN
!
                  IF (PRESENT (IYr)) READ (CTemp6(2:5),'(i4)') IYr
                  IF (PRESENT (CYr)) READ (CTemp6(2:5),'(a4)') CYr
!
              ELSE
!             *** Run-time parameter retrieval error.  Terminate processing.
!
                  CALL WriteReport (SARBIOUTIL_E_PCF_RETRIEVE_YR,        & ! I
                                    FuncName = Name_Sbr,                 & ! I
                                    StrVar = " IStat = (%i4) ",          & ! I
                                    IVar1=IStat)                           ! I
!
              END IF
!
          ELSE
!         *** Necessary information missing.  Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_MISSING_LID_YR,             & ! I
                                FuncName = Name_Sbr)                       ! I
!
          END IF
!
      END IF
!
!
      END SUBROUTINE RunTimeVar_DateRetrieve
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE RunTimeVar_SatRetrieve (LID_Inst, LID_Sat,         & ! I (Opt)
                                               CInst, CSat)           ! O (Opt)
!
!*******************************************************************************
!
! !F90
!
! !Name:
!    RunTimeVar_DateRetrieve
!
! !Description:
!    Routine ID - 
!
!    Purpose - Retrieves instrument-releated run-time parameters from the PCF
!
! 
! !Input Parameters:
!   LID_Inst - PCF LID for Instrument run-time parameter
!   LID_Sat  - PCF LID for Satellite run-time parameter
!
! !Output Parameters:
!   CInst - Character value of the instrument name
!   CSat  - Character value of the satellite name
!
! !Team-Unique Header:
!
!  Modules Used:
!    msg - (CERESlib) Sbr. WriteReport
!    pcf - (CERESlib) Sbr. GetParam
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Params: SARBIOUTIL_E_MISSING_LID_INST,
!                                     SARBIOUTIL_E_MISSING_LID_SAT,
!                                     SARBIOUTIL_E_PCF_RETRIEVE_INST,
!                                     SARBIOUTIL_E_PCF_RETRIEVE_SAT
!
! !Revision History:
!  Version 3.5   August 1, 2003 - SCCR #458 
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      CHARACTER (LEN=3), INTENT (OUT), OPTIONAL :: CInst
      CHARACTER (LEN=5), INTENT (OUT), OPTIONAL :: CSat
      INTEGER,           INTENT (IN),  OPTIONAL :: LID_Inst
      INTEGER,           INTENT (IN),  OPTIONAL :: LID_Sat
!
      CHARACTER (LEN=5)             :: CTemp5
      CHARACTER (LEN=7)             :: CTemp7
      INTEGER                       :: IStat
      INTEGER                       :: IVal
      CHARACTER (LEN=25), PARAMETER :: Name_Sbr = 'RunTimeVar_SatRetrieve ()'
!
!
      IF (PRESENT (CInst)) THEN
!     *** Instrument name output is requested
!
          IF (PRESENT (LID_Inst)) THEN
!         *** All necessary information provided by calling routine
!
              CALL GetParam (LID_Inst, CTemp5, IStat)
!
              IF (IStat == 0) THEN
!
                  READ (CTemp5(2:4),'(a3)') CInst
!
              ELSE
!             *** Run-time parameter retrieval error.  Terminate processing.
!
                  CALL WriteReport (SARBIOUTIL_E_PCF_RETRIEVE_INST,      & ! I
                                    FuncName = Name_Sbr,                 & ! I
                                    StrVar = " IStat = (%i4) ",          & ! I
                                    IVar1=IStat)                           ! I
!
              END IF
!
          ELSE
!         *** Necessary information missing.  Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_MISSING_LID_INST,           & ! I
                                FuncName = Name_Sbr)                       ! I
!
          END IF
!
      END IF
!
      IF (PRESENT (CSat)) THEN
!     *** Satellite name output is requested
!
          IF (PRESENT (LID_Sat)) THEN
!         *** All necessary information provided by calling routine
!
              CALL GetParam (LID_Sat, CTemp7, IStat)
!
              IF (IStat == 0) THEN
!
                  READ (CTemp7(2:6),'(a5)') CSat
!
                  IVal = INDEX (CSat,'"')
!
                  IF (IVal /= 0) THEN
!                 *** Replace quotes at end of name with blanks
!
                      WRITE (CSat (IVal:5), '(a)') " "
!
                  END IF
!
              ELSE
!             *** Run-time parameter retrieval error.  Terminate processing.
!
                  CALL WriteReport (SARBIOUTIL_E_PCF_RETRIEVE_SAT,       & ! I
                                    FuncName = Name_Sbr,                 & ! I
                                    StrVar = " IStat = (%i4) ",          & ! I
                                    IVar1=IStat)                           ! I
!
              END IF
!
          ELSE
!         *** Necessary information missing.  Terminate processing.
!
              CALL WriteReport (SARBIOUTIL_E_MISSING_LID_SAT,            & ! I
                                FuncName = Name_Sbr)                       ! I
!
          END IF
!
      END IF
!
!
      END SUBROUTINE RunTimeVar_SatRetrieve
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      FUNCTION DaysPerMonth (IMonth, IYear) RESULT (NDay)
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   DaysPerMonth
!
! !Description:
!   Routine ID - General SARB
!
!   Purpose - Returns the number of days in a specified month
!
!
! !Input Parameters:
!   IMonth - Two-digit month of year
!   IYear  - Four-digit year
!
! !Output Parameters:
!   NDay - Number of days in the month
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 3.4 December 20, 2002 - SCCR #408
!                  Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   Initial version of code
!
! !END
!
!*******************************************************************************
!
!
      INTEGER  :: IMonth
      INTEGER  :: IYear
      INTEGER  :: NDay
!
      INTEGER, PARAMETER :: Days_Per_Month ( 12) =              &
                          (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
!
!
      NDay = Days_Per_Month ( IMonth)
!
      IF ( MOD (IYear, 4) == 0) THEN
!     *** Leap year
!
          IF (IMonth == 2) NDay = 29
!
      END IF
!
!
      END FUNCTION DaysPerMonth
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   
      SUBROUTINE SARB_RdWrtError (LID_File, IErr, Name_Sbr,        & ! I
                                  TermFlag, Vers, IRec)              ! I (Opt)
!
!******************************************************************************
!
! !F90
!
! !Name:
!   SARB_RdWrtError 
!
! !Description:
!   Routine ID -
!
!   Purpose - Issues read/write failure error messages using the CERESlib
!             wrappers for the ECS Toolkit routines
!
!
! !Input Parameters:
!   IErr       - Error status code returned from Fortran READ or WRITE 
!                function
!   IRec       - (OPTIONAL) File record number on which error occured;
!                Default value = 1
!   LID_File   - Integer logical identifier into PCF for file 
!   Name_Sbr   - Character name of calling subroutine
!   TermFlag   - (OPTIONAL) Flag indicating whether or not to terminate 
!                processing; Default value = ProcessTerminate
!   Vers       - (OPTIONAL) Integer file version number used by the ECS 
!                Toolkit open routine;  Default value = 1
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!   msg - (CERESlib) Sbr: WriteReport
!
!  External Files Used:
!    PGS_SARBIOUTIL_25750.f - Params: SARBIOUTILS_E_FILE_RDWRT_ERROR,
!                                     SARBIOUTILS_W_FILE_RDWRT_ERROR
!
! !Revision History
!  Version 3.7  January 12, 2004 - SCCR #498 
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER,           INTENT (IN) :: IErr
      INTEGER, OPTIONAL, INTENT (IN) :: IRec
      INTEGER,           INTENT (IN) :: LID_File
      CHARACTER (LEN=*), INTENT (IN) :: Name_Sbr
      INTEGER, OPTIONAL, INTENT (IN) :: TermFlag
      INTEGER, OPTIONAL, INTENT (IN) :: Vers
!
      CHARACTER (LEN=57) :: CString
      INTEGER            :: RNum
      INTEGER            :: Flag_TermCont
      INTEGER            :: VersNum           
!
!
      IF ( PRESENT (Vers)) THEN
!     *** Use version number supplied by calling routine
!
          VersNum = Vers
!
      ELSE 
!     *** Assume only one version
!
          VersNum = 1
!
      END IF
!
!
      IF (Present (IRec)) THEN
!     *** Use record number provided by calling routine
!
          RNum = IRec
!
      ELSE
!     *** Set record number to one
!
          RNum = 1
!
      END IF
!
!
      IF ( PRESENT (TermFlag)) THEN
!     *** Use version number supplied by calling routine
!
          Flag_TermCont = TermFlag
!
      ELSE 
!     *** Assume only one version
!
          Flag_TermCont = ProcessTerminate
!
      END IF
!
!
      CString = 'IOS = xxxx   LID = xxxx   VERSION = xxxx   REC = xxxxxxx '
      WRITE (CString(7:10), '(i4)') IErr
      WRITE (CString(20:23), '(i4)') LID_File
      WRITE (CString(37:40), '(i4)') VersNum
      WRITE (CString(50:56), '(i7)') RNum
!
!
      IF (Flag_TermCont == ProcessTerminate) THEN
!     *** Terminate processing
!
          CALL WriteReport (SARBIOUTIL_E_FILE_RDWRT_ERROR,           & ! I
                            FuncName =  Name_Sbr,                    & ! I
                            StrVar = CString)                          ! I
!
      ELSE IF (Flag_TermCont == ProcessContinue) THEN
!     *** Issue diagnostic error message and continue processing
!
          CALL WriteReport (SARBIOUTIL_W_FILE_RDWRT_ERROR,           & ! I
                            FuncName =  Name_Sbr,                    & ! I
                            StrVar = CString)                          ! I

!
      END IF
!
!
      END SUBROUTINE SARB_RdWrtError
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE SARB_IO_Utils
