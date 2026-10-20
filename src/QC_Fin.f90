MODULE QC_Fin
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    QC_Fin
!
!
! !Description:
!    Routine ID - M5.3.1
!
!    Purpose - To provide the routines necessary to generate a formatted
!              QC Report file
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
!  Output Files:
!    QC_Unit - Formatted QC Report file
!
!  Internal Routines:
!    QC_Gen    - Outputs error statistics and counters to a formatted 
!                QC report file
!    Stat_Calc - Computes final QC report statistics and writes them 
!                to a formatted file
!
!  Modules Used:
!    ceres_valregions - (CERESlib) Used by Sbr. QC_Gen 
!    f90_kind         - (CERESlib) Used by Sbr. Stat_Calc
!    IO               - (CERESlib) Used by Sbr. QC_Gen
!    MSG              - (CERESlib) Used by Sbr. QC_Gen
!    Profile_Params   - (SARBlib)  Used by Sbr. QC_Gen
!    QCheader         - (CERESlib) Used by Sbr. QC_Gen
!    SARB_OutVar      - (SARBlib)  Used by Sbr. QC_Gen
!    SARB_QC_Params   - (SARBlib)  Used by Sbr. QC_Gen, Stat_Calc
!    SARB_Var         - (SARBlib)  Used by Sbr. QC_Gen, Stat_Calc
!
! !Revision History:
!  Version 4.0   October 4th, 2004
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                   1. Modified Sbr. QC_Gen
!  Version 3.3   R3 Date - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                   1. Modified Sbr. QC_Gen
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Modified Sbr. Stat_Calc
!                  2. Removed call to Sbr. DataDate_Retrive to Mod. Init_SS5
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Modified Sbr. StatCalc 
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                  1. Modified Sbr. QC_Gen
!                  2. Breakout of Sbr. StatCalc
!                  3. Replacement of Func. IDay_to_CDay, IMonth_to_CMonth, 
!                     and IYr_to_CYr with Sbr. DataDate_Retrieve
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                   Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
    USE ceres_valregions, ONLY: NumValReg_AWG
    USE f90_kind, ONLY:  REAL8
    USE IO, ONLY: OpenFile, CloseFile
    USE MSG, ONLY: WriteReport
    USE Profile_Params
    USE QCheader, ONLY: WriteHeader 
    USE SARB_OutVar
    USE SARB_QC_Params
    USE SARB_Var
!
!
    IMPLICIT NONE
!
!
 CONTAINS
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE QC_Gen (NumRecs, fqc, QC_Unit)                         ! I
!
!****************************************************************************
!
! !F90
!
! !Name:
!    QC_Gen
!
! !Description:
!    Routine ID - 5.3.1
!
!    Purpose - To output SARB QC report statistics to a formatted output
!              file
!
! 
! !Input Parameters:
!    fqc     - QC parameter structure
!    NumRecs - Number of records
!    QC_Unit - Output file unit number for QC report
!
! !Output Parameters:
!    NONE
! 
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_valregions - (CERESlib) Parameters: NumValReg_AWG
!    IO               - (CERESlib) Sbr. OpenFile, CloseFile
!    MSG              - (CERESlib) Sbr. WriteReport
!    Profile_Params   - (SARBlib)  Parameters: HRes_Mult_Left, HRes_Mult_Right, 
!                                              HRes_Sing_Left, HRes_Sing_Right, 
!                                              NumDir, NumPass, NumSkyCond, 
!                                              QCHead_SSID
!    QCheader         - (CERESlib) Sbr. WriteHeader
!    SARB_QC_Params   - (SARBlib)  Parameters: ICnt_DirDiff, ICnt_LW_Inv, 
!                                              ICnt_Night, ICnt_NoFlux, 
!                                              ICnt_NoPres,
!                                              ICnt_NumPass, ICnt_SW_Inv, 
!                                              ICnt_SfcRad_Init, 
!                                              ICnt_SunGlint, ICnt_Tridag, 
!                                              ICnt_TuneErr, ICnt_WN_Inv 
!                                  Structures: fqc_type
!    SARB_Var         - (SARBlib)  Parameters: CDay, CHour, CMonth, CYear, 
!                                              QC_Unit, ValSite_Pass, 
!                                              ValSite_PassCnt
!
!  Internal Routines Used:
!    Stat_Calc
!
!  Output Files:
!    QC_Unit - Formatted SARB QC Report
!
! !Revision History:
!  Version 4.0   October 4, 2004
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                1. Added/Altered WRITE 1002-1020 
!                2. Added/Altered FORMAT statements for 1002-1020
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removal of SS_ID parameter from calling argument list
!                     and moving of parmaeters for subsystem-dependent QC 
!                     report labels to Mod. Profile_Params
!  Version 3.0  March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Removed call to Sbr. DataDate_Retrive 
!                 2. Added use of Mod. SARB_Var variables
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. Breakout of Sbr. Stat_Calc
!                 2. Replaced calls to Func. IDay_to_CDay, IMonth_to_CMonth, 
!                    and IYr_to_CYr with single call to Sbr. DataDate_Retrieve 
!                 3. Added ICnt_SfcRad_Init to formatted output
!                 4. Added hour of data to output report
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. Added logic to compute and output additional parameters
!                    (fqc) to the QC Report
!                 2. Moved this routine from Module Foot_Drv
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Release 2 Code
!
! !END
!
!****************************************************************************
!
!
      TYPE (fqc_type),   INTENT (IN) :: fqc
      INTEGER,           INTENT (IN) :: NumRecs
      INTEGER,           INTENT (IN) :: QC_Unit
!
!
      REAL                           :: Bad_Percent
      CHARACTER (LEN=10)             :: CDate
      CHARACTER (LEN=10), PARAMETER  :: DayNt_QRCLab ( 2) =                    &
                                       (/ "DAYTIME  ", "NIGHTTIME" /)
      REAL                           :: Good_Percent
      INTEGER                        :: I, II, I1, I2
      INTEGER                        :: ICld, IDir, IPass, ISky
      INTEGER                        :: ILeftOvr
      INTEGER                        :: K
      CHARACTER (LEN=30), PARAMETER  :: Pass_QCRLab ( 2) =                     &
                                       (/ "INITIAL PASS ONLY             ",    &
                                          "INITIAL + n CONSTRAINED PASSES" /)
      CHARACTER (LEN=80)             :: TempHold
      CHARACTER (LEN=30)             :: TempPassQCRLab
      CHARACTER (LEN=18)             :: TempICldQCRLab
      INTEGER                        :: Total_Bad_FOVS
      REAL                           :: Total_Percent
!
!
! *** Assign initial values
!
      TempPassQCRLab = Pass_QCRLab ( 2)
      TempICldQCRLab = "  CLOUD LAYERS : "
!
!
! *** Build full data date and write report header
!
      CDate = CMonth//"/"//CDay//"/"//CYear
      CALL WriteHeader(Ounit = QC_Unit, Subsystem = QCHead_SSID, &
                       DataStart = CDate, DataStop = CDate)
!
! *** Sum Total Counters
!
      Total_Bad_FOVS = InvalidInput_TotQC + InvalidModel_TotQC
      Bad_Percent = REAL(Total_Bad_FOVS) / REAL(ICnt_FOV) * 100.0
      Good_Percent = REAL(ValidFOV_TotQC) / REAL(ICnt_FOV) * 100.0
      Total_Percent = REAL(ICnt_FOV) / REAL(NumRecs) * 100.0
!
! *** Write product level statistics
!
      WRITE (QC_Unit,1001) CHour
      WRITE (QC_Unit,1002) HRes_Mult_Left, NumRecs
      WRITE (QC_Unit,1003) HRes_Mult_Left, ValidFOV_TotQC, Good_Percent
      WRITE (QC_Unit,1004) HRes_Mult_Left, Total_Bad_FOVS, Bad_Percent
      WRITE (QC_Unit,1005) HRes_Mult_Left, ICnt_FOV, Total_Percent 
      WRITE (QC_Unit,1010) HRes_Mult_Left, ICnt_Night
      WRITE (QC_Unit,1015) HRes_Mult_Left, NumRecs - ICnt_Night 
      WRITE (QC_Unit,1018) InvalidInput_TotQC 
      WRITE (QC_Unit,1019) InvalidModel_TotQC
      WRITE (QC_Unit,1020) HRes_Mult_Right, Input_File, &
                       InvalidTSI_SSF_TotQC, CloudInconsist_TotQC,  &
                       ICnt_NoFlux ( 1), ICnt_NoFlux ( 2), &
                       ICnt_SunGlint ( 1), ICnt_SunGlint ( 2), &
                           ICnt_NoPres ( 1), ICnt_NoPres ( 2), &
                           ICnt_SfcRad_Init
                           
!
      DO IPass = 1, NumPass
!
          WRITE (QC_Unit, 1030) LabPass ( IPass), ICnt_DirDiff ( IPass),   &
                                LabPass ( IPass), ICnt_PAR ( IPass)
!
          DO ISky = 1, NumSkyCond
              DO IDir = 1, NumDir
                  WRITE (QC_Unit, 1040) LabDir ( IDir), LabSky ( ISky),    &
                      LabPass ( IPass), ICnt_LW_Inv ( IPass, ISky, IDir)
              END DO
          END DO
!
          DO ISky = 1, NumSkyCond
              DO IDir = 1, NumDir
                  WRITE (QC_Unit, 1050) LabDir ( IDir), LabSky (ISky),    &
                      LabPass ( IPass), ICnt_SW_Inv ( IPass, ISky, IDir)
              END DO
          END DO
!
          DO ISky = 1, NumSkyCond
              DO IDir = 1, NumDir
                  WRITE (QC_Unit, 1060) LabDir ( IDir), LabSky (ISky),    &
                      LabPass ( IPass), ICnt_WN_Inv ( IPass, ISky, IDir)
              END DO
          END DO
!
          DO ISky = 1, NumSkyCond
              WRITE (QC_Unit, 1070)  LabSky (ISky), LabPass ( IPass),  &
                                     ICnt_TotLWrad_Inv ( IPass, ISky), &
                                     LabSky (ISky), LabPass ( IPass),  &
                                     ICnt_WNflx_Inv ( IPass, ISky),    &
                                     LabSky (ISky), LabPass ( IPass),  &
                                     ICnt_WNrad_Inv ( IPass, ISky)  
          END DO
!
      END DO
!
!
      DO IPass = 1, NumPass
!
          WRITE (QC_Unit,1090) LabPass ( IPass), ICnt_Tridag ( 1, IPass),  &
                               LabPass ( IPass), ICnt_Tridag ( 2, IPass)
!
      END DO
!
!
! *** Report tuning errors
!
      WRITE (QC_Unit,1100) ICnt_TuneErr ( 1:8)
!
!
! *** Report summary of passes through the model
!
      WRITE (QC_Unit,1200) 
      DO I = 1,2
          WRITE (QC_Unit,'(5x,a10,a11)') DayNt_QRCLab ( I), HRes_Mult_Left
          DO IPass = 1, NumPass
              IF (IPass  == InitPass) THEN
                  WRITE (QC_Unit,'(10x,a28)') Pass_QCRLab ( InitPass)
              ELSE
                  WRITE (TempPassQCRLab (11:11),'(i1)') IPass-1
                  WRITE (QC_Unit,'(10x,a28)') TempPassQCRLab
              END IF
              DO ICld = 0, MaxLev_Cld
                   WRITE (TempICldQCRLab(1:1), '(i1)') ICld
                   WRITE (QC_Unit,'(14x,a18,i6)') TempICldQCRLab,             &
                                                  ICnt_NumPass ( I, IPass, ICld)
              END DO
          END DO
      END DO
! 
!
! *** Validation Region Pass summary
!
      WRITE (QC_Unit, 1300)
      DO I = 1, NumValReg_AWG, 12
!
         ILeftOvr = NumValReg_AWG - I + 1
!
         IF (ILeftOvr >= 12) THEN
!        *** Enough to fill a row
!
             WRITE (QC_Unit,1310) ValSite_Pass ( I:I+11),                &
                                  ValSite_PassCnt ( I:I+11)
!
         ELSE
!        *** Only a partial row
!
             WRITE (TempHold,'(80(a1))') ' '
             WRITE (TempHold (1:8),'(a8)') ' Reg Num'
             I1 = 9
             I2 = I1 + 5
             DO II = I, NumValReg_AWG
                 WRITE (TempHold (I1:I2),'(i6)') ValSite_Pass ( II)
                 I1 = I1 + 6
                 I2 = I1 + 5
             END DO
             WRITE (QC_Unit,'(a80)') TempHold
!
             WRITE (TempHold,'(80(a1))') ' '
             WRITE (TempHold (1:8),'(a8)') ' Passes '
             I1 = 9
             I2 = I1 + 5
             DO II = I, NumValReg_AWG
                 WRITE (TempHold (I1:I2),'(i6)') ValSite_PassCnt ( II)
                 I1 = I1 + 6
                 I2 = I1 + 5
             END DO
             WRITE (QC_Unit,'(a80)') TempHold
!
         END IF
!
      END DO
!
!
      WRITE (QC_Unit, 1000) 
!
!
      CALL Stat_Calc (fqc, QC_Unit)                                       ! I
!
!
! *** FORMAT STATEMENTS
 1000 FORMAT (//, ' PRODUCT LEVEL STATISTICS',/,' ************************',/)
 1001 FORMAT ( /,' HOUR: ',a2,/)
 1002 FORMAT (' TOTAL NUMBER OF INPUT   ', a11,' :', i7)
 1003 FORMAT (' TOTAL NUMBER OF VALID   ', a11,' :', i7, f7.2, '%')
 1004 FORMAT (' TOTAL NUMBER OF INVALID ', a11,' :', i7, f7.2, '%')
 1005 FORMAT (' TOTAL NUMBER OF OUTPUT  ', a11,' :', i7, f7.2, '%')
 1010 FORMAT (' NUMBER OF NIGHTTIME     ', a11,' :', i7)
 1015 FORMAT (' NUMBER OF DAYTIME       ', a11,' :', i7) 
 1018 FORMAT (' NUMBER OF FAILURES DUE TO INPUT     : ', i6)
 1019 FORMAT (' NUMBER OF FAILURES IN MODEL         : ', i6) 
 1020 FORMAT (' NUMBER OF ', a11,' NOT PROCESSED DUE TO -- ',                &
 /,t6,' INVALID ', a3, ' VALUES                                :',i6, &
 /,t6,' INVALID CLOUD INCONSISTANCIES                     :',i6, &
 /,t6,' INVALID SW LW WN CERES TOA FLUX VALUES - DAYTIME  :',i6, &
             /,t6,' INVALID SW LW WN CERES TOA FLUX VALUES - NIGHTTIME:',i6, &
             /,t6,' SUNGLINT OCCURRENCE - DAYTIME                     :',i6, &
             /,t6,' SUNGLINT OCCURRENCE - NIGHTTIME                   :',i6, &
             /,t6,' UNAVAILABLE CLOUD HEIGHTS - DAYTIME               :',i6, &
             /,t6,' UNAVAILABLE CLOUD HEIGHTS - NIGHTTIME             :',i6, &
             /,t6,' INVALID SURFACE ALBEDO SCENE TYPE                 :',i6)
 1030 FORMAT (t6,' INVALID DIRECT/DIFFUSE RATIO - ',a12,t57,':',i6,          &
             /t6,' INVALID PAR VALUE -',a12,t57,':',i6)
 1040 FORMAT (t6,' INVALID LW ',a9,a9,'SKY - ',a12,t57,':',i6)
 1050 FORMAT (t6,' INVALID SW ',a9,a9,'SKY - ',a12,t57,':',i6)
 1060 FORMAT (t6,' INVALID WN ',a9,a9,'SKY - ',a12,t57,':',i6)
 1070 FORMAT (t6,' INVALID TotLW Rad ',a9,'SKY - ',a12,t57,':',i6,          &
             /t6,' INVALID WN Flux SatEm ',a9,'SKY - ',a12,t57,':',i6,      &
             /t6,' INVALID WN Radiance ',a9,'SKY - ',a12,t57,':',i6)
 1090 FORMAT ( t6,' FIRST TRIDAG PAUSE - ',a12,t57,':',i6,                   & 
             /,t6,' SECOND TRIDAG PAUSE - ',a12,t57,':',i6  )
 1100 FORMAT ( //,'TUNING ERROR STATISTICS --',                              &
             /,'  NUMBER OF SIGMA TABLE VALUES EXCEEDING MAXTUNE    :',i6,  &
              /,'  NUMBER OF SIGMA TABLE VALUES EXCEEDING MCLD       :',i6,  &
              /,'  NUMBER OF SIGMA TABLE VALUES EXCEEDING NSID       :',i6,  &
              /,'  NUMBER OF OCCURRENCES OF SBR. TUNE_XXX IPASS > 10 :',i6,  &
              /,'  NUMBER OF CLOUD FRACTION ADJUSTMENT ERRORS        :',i6,  &
              /,'  NUMBER OF SINGULAR MATRIX ERRORS                  :',i6,  &
              /,'  NUMBER OF BAD TUNED CLOUD FRACTIONS               :',i6  )
 1200 FORMAT ( //,'SUMMARY OF PASSES THROUGH RADIATIVE TRANSFER MODEL --',//)  
 1300 FORMAT ( //, 'CERES VALIDATION REGION PASS SUMMARY', /)
 1310 FORMAT ( ' Reg Num',12(1x,i5),/,' Passes ',12(1x,i5),/)
!
!
      END SUBROUTINE QC_Gen
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Stat_Calc (fqc, QC_Unit)                             ! I
!
!****************************************************************************
!
! !F90
!
! !Name:
!    QC_Gen
!
! !Description:
!    Routine ID - 5.3.1.2
!
!    Purpose - To compute final SARB QC report statistics
!
! 
! !Input Parameters:
!    fqc     - QC parameter structure
!    QC_Unit - Output file unit number for QC report
!
! !Output Parameters:
!    NONE
! 
! !Team-Unique Header:
!
!  Modules Used:
!    f90_kind       - Parameters: REAL8
!    SARB_QC_Params - Parameters: avg, curt, icnt, it, lmax, lmin, ncase,  
!                                 ngt, np, rmax, rmin, sa, skew, std 
!                     Structures: fqc_type
!    SARB_Var       - Parameters: QC_Unit
!
!  Output Files:
!    QC_Unit - Formatted SARB QC Report
!
! !Revision History:
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Changed type of variable var from REAL4 to REAL8
!                 2. Replaced 1.0E-10 in variance check with new parameter, 
!                    var_min, and also redefined minimum value to be 1.0E-4
!                 3. Modified calculation of standard deviation to take the
!                    square root of the absolute value of the variable var
!                    instead of just the square root.
!                 4. Removed locations (record number) of minimum and maximum 
!                    values, locmin and locmax, from the formatted output
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Changed test against variance value, var, to
!                    check against the value of 1.0E-16 instead of 0.0
!                    before calculating the higher order statistics.
!  Version 2.3   June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. Breakout from Sbr. QC_Gen into own routine
!                 2. Moved calculation for variable std to after the
!                    check that the value of the variable var is positive
!                 3. Expanded format for Kurt value
!                 4. Added check for sample count of 1 before calculating
!                    values for std, skew, and curt
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                 1. Added logic to compute and output additional parameters
!                    (fqc) to the QC Report
!                 2. Moved this routine from Module Foot_Drv
!  Version 2.0   November 21, 1997 - SCCR #30
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Release 2 Code
!
! !END
!
!****************************************************************************
!
!
      TYPE (fqc_type),   INTENT (IN) :: fqc
      INTEGER,           INTENT (IN) :: QC_Unit
!
      INTEGER                 :: icase, igt, ip, lm_idx
      REAL (REAL8)            :: var
      REAL (REAL8), PARAMETER :: var_min = 0.0001
!
!
      DO ip = 1, np
!
      IF (icnt ( ip) > 0) THEN
        avg ( ip) = sa ( ip, 1) / REAL (icnt ( ip))
! +++ LHC addition
        IF (icnt ( ip) == 1) THEN
          std ( ip) = 0.
          skew ( ip) = 0.
          curt ( ip) = 0.
          rmin ( ip) = 0.
          rmax ( ip) = 0.
        END IF
      ELSE
        avg ( ip) = 0.
        std ( ip) = 0.
        skew ( ip) = 0.
        curt ( ip) = 0.
        rmin ( ip) = 0.
        rmax ( ip) = 0.
! +++++ END LHC addition
      END IF
!
      IF (icnt ( ip) > 1) THEN

       var =   (sa ( ip, 2) - (REAL (icnt ( ip)) * avg ( ip)**2 ))  & 
                 / REAL  (icnt ( ip) - 1)
!
!
       IF (var > var_min) THEN
            std ( ip) = SQRT (ABS (var))
        skew ( ip) =  (  sa ( ip, 3)                                &
                             - 3.0 * avg ( ip) * sa ( ip, 2)        &
                     + 2.0 * REAL (icnt ( ip)) * avg ( ip)**3 )     &
                   / REAL (icnt ( ip))
        skew ( ip) = skew ( ip) / std ( ip)**3
!
        curt ( ip) =   ( sa ( ip, 4)                                &
                       - 4.0 * avg ( ip) * sa ( ip, 3)              &
                               + 6.0 * avg ( ip)**2 * sa ( ip, 2)   &
                       - 3.0 * REAL (icnt ( ip)) * avg ( ip)**4 )   &
                   / REAL (icnt ( ip) )
        curt ( ip) = curt ( ip) / (var**2) - 3.0
! +++ LHC addition
       ELSE
           std ( ip) = 0.
           skew ( ip) = 0.
           curt ( ip) = 0.
! +++++ END LHC addition
       END IF ! var > 0 
      END IF

      END DO
!
      WRITE (QC_Unit,'(a4, a32, 1x, 3a10,a10,a15,2a10)')              &
        'id#','DESCRIPTION         ',                                 &
        'AVG','STDDEV','Good#','Skew','Kurt','Min','MAX'
!
      it = 0
      DO icase = 1, ncase
         DO igt = 1, ngt
          lm_idx = ( (icase-1) * ngt) + igt
          it = it + 1
          	  
	  WRITE (QC_Unit,'(i4,1x,a32,2f10.3,I10,f10.3,f15.3,2f10.3)')     &
                                       it, fqc % la ( igt, icase),        &
                                       avg ( lm_idx),                     &
                                       std ( lm_idx),                     &
                                       icnt ( lm_idx),                    &
                                       skew ( lm_idx),                    &
                                       curt ( lm_idx),                    &
                                       rmin ( lm_idx),                    &
                                       rmax ( lm_idx) 
         END DO 
      END DO
!
!
      END SUBROUTINE Stat_Calc
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE QC_Fin
