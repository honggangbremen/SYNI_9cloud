REAL FUNCTION ufp(i2flux)

INTEGER*2 i2flux
ufp = ((i2flux+32767)/40.)

IF (  i2flux == INT2_DFLT ) ufp = REAL4_DFLT
END FUNCTION ufp 


PROGRAM SYNI_Compare
!
!
      USE ED4_SYNI_STRUCTURE
!      USE SARB_PARAMS
      USE grid_nest_routines
      USE intsyn_io
!
      IMPLICIT NONE
!
!
      INTEGER, PARAMETER :: Diag_Unit = 7
      INTEGER, PARAMETER :: Input_Unit = 9
      INTEGER, PARAMETER :: InputName_Unit = 12
      INTEGER, PARAMETER :: SYN1_Unit = 1
      INTEGER, PARAMETER :: SYN2_Unit = 2
      INTEGER, PARAMETER :: UnitNum = 11
      
      REAL, PARAMETER :: MaxDeltaPAR = 0.01     
      REAL, PARAMETER :: MaxDeltaDir = 0.01     
      REAL, PARAMETER :: MaxDeltaDiff = 0.01     
      REAL, PARAMETER :: MaxDeltaFlux = 0.01     
      REAL, PARAMETER :: MaxDeltaH2O = 0.01     
      REAL, PARAMETER :: MaxDeltaSfcAlb = 0.01     
      REAL, PARAMETER :: MaxDeltaOptDep = 0.01     
      REAL, PARAMETER :: MaxDeltaSkinTemp = 0.01     
      REAL, PARAMETER :: MaxDeltaAreaFrac = 0.01     
      REAL, PARAMETER :: MaxDeltaCldTemp = 0.01     
      REAL, PARAMETER :: MaxDeltaUV = 0.01     
      REAL, PARAMETER :: MaxDeltaEnt = 0.01     
      REAL, PARAMETER :: MaxDeltaIns = 0.01     
      REAL, PARAMETER :: MaxDeltaUth = 0.01     

      REAL, PARAMETER :: MaxDeltaPercent = 0.005  

      CHARACTER (LEN=200) :: Fname_SYNI1 
      CHARACTER (LEN=200) :: Fname_SYNI2 
      CHARACTER (LEN=8),  PARAMETER :: Fname_Diag = 'Diag_Out'
      CHARACTER (LEN=5),  PARAMETER :: Fname_Input = 'Input' 
      CHARACTER (LEN=9),  PARAMETER :: Fname_Names = 'filenames' 

      INTEGER :: I, IFOV1, IFOV2
      INTEGER :: ICnt_MisMatch
      INTEGER :: IOS
      INTEGER :: IRec
      INTEGER :: LenSARB 
      INTEGER :: LenSARB1
      INTEGER :: LenSARB2
      INTEGER :: LenSARB3
      INTEGER :: LenSARB4
      INTEGER :: MisRec
      INTEGER :: Num_Hours
      INTEGER :: Syn_Zone
!
      REAL :: Delta

      TYPE (syni4_TYPE)    :: ExSYN_SARB

      TYPE (syni4_TYPE)    :: SYN_SARB
!
! *** Initialize counter
      ICnt_MisMatch = 0

!
! *** Open and Read Name File

      OPEN (InputName_Unit, FILE = Fname_Names, FORM = 'formatted', IOSTAT=Ios)
!
      IF (Ios /= 0) STOP ' ERROR OPENING Name File'
!
      READ (InputName_Unit, FMT = '(a200)', IOSTAT = IOS) Fname_SYNI1
      READ (InputName_Unit, FMT = '(a200)', IOSTAT = IOS) Fname_SYNI2
!
      
      IF (IOS /= 0) STOP ' ERROR READING Name File' 	  	  		   


! *** Open and Read Input File

      OPEN (Input_Unit, FILE = Fname_Input, FORM = 'formatted', IOSTAT=Ios)
!
      IF (Ios /= 0) STOP ' ERROR OPENING Input File'
!
      READ (Input_Unit, FMT = '(i3,/,i3)', IOSTAT = IOS) Syn_Zone, Num_Hours
!
      IF (IOS /= 0) STOP ' ERROR READING Input File' 	  	  		   

!
! *** Initialize data structures to prevent compiler errors and retrieve
!     record length.
!
      INQUIRE (IOLENGTH=LenSARB1) SYN_SARB
!
      LenSARB = LenSARB1
!
      OPEN (SYN1_Unit, FILE=FName_SYNI1, ACCESS='direct', FORM='unformatted', &
              RECL=LenSARB, ACTION='READ', STATUS='OLD', IOSTAT=Ios)
!
      IF (Ios /= 0) STOP ' ERROR OPENING SYN1'
!
      OPEN (SYN2_Unit, FILE=FName_SYNI2, ACCESS='direct', FORM='unformatted', &
              RECL=LenSARB, ACTION='READ', STATUS='OLD', IOSTAT=Ios)
!
      IF (Ios /= 0) STOP ' ERROR OPENING SYN2'
!
!
! *** Open output file
!
      OPEN (Diag_Unit, file=Fname_Diag, status='replace', form='formatted', &
             IOSTAT=Ios)
      IF (Ios /= 0) STOP 'ERROR OPENING DIAGNOSTIC OUTPUT FILE'
      WRITE (Diag_Unit, 1000) Fname_SYNI1
      WRITE (Diag_Unit, 1005) Fname_SYNI2
      WRITE (Diag_Unit, 1010) 'SYNI_Expected','SYNI_Computed'
!
      IFOV1 = 2
      IFOV2 = number_nested_regions(Syn_Zone) * Num_Hours + 1
!
	  DO IRec = IFOV1, IFOV2 
!
          MisRec = 0
!
!     *** Read from each input file
!
          READ (SYN1_Unit, REC=IRec, IOSTAT=IOS) ExSYN_SARB
!
          IF (IOS /= 0) THEN
             PRINT *, 'IREC=',IREC, '  IOS=',IOS
             STOP ' BAD READ ON SYN1'
          END IF
!
          READ (SYN2_Unit, REC=IRec, IOSTAT=IOS) SYN_SARB
!
          IF (IOS /= 0) THEN
             PRINT *, 'IREC=',IREC, '  IOS=',IOS
             STOP ' BAD READ ON SYN2'
          END IF
!
!      *** Print record
!
!         CALL IntSYN_Print (SYN_SARB, SYNI_Loc, SYNI_CNA, SYNI_SpecInt, UnitNum)         

!     *** Compare each SARB value from the two input files
!
 
!     *** Compare records only if Error codes are same

!          IF (    SYN_SARB % misc % error(1)                      &
!               == ExSYN_SARB % misc % error(1)) THEN
 
 
     DO I = 1, 6
	 
	 
! *** Tuned clear shortwave flux

	  Delta = (ufp(ExSYN_SARB % tu % clr % sw % u(I)) - & 
	          ufp(SYN_SARB % tu % clr % sw % u(I)))/ufp(ExSYN_SARB % tu % clr % sw % u(I))
		  
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % clr % sw % u ',I, IRec, ufp(ExSYN_SARB % tu % clr % sw % u(I)),         &
                                            ufp(SYN_SARB % tu % clr % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % clr % sw % d(I)) - &                           
                   ufp(SYN_SARB % tu % clr % sw % d(I)))/ufp(ExSYN_SARB % tu % clr % sw % d(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % clr % sw % d ',I, IRec, ufp(ExSYN_SARB % tu % clr % sw % d(I)),         &
                                            ufp(SYN_SARB % tu % clr % sw % d(I))
           END IF
          ENDIF

! *** Tuned clear longwave flux

	  Delta = (ufp(ExSYN_SARB % tu % clr % lw % u(I)) -                           &
                  ufp(SYN_SARB % tu % clr % lw % u(I)))/ufp(ExSYN_SARB % tu % clr % lw % u(I))
	  
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % clr % lw % u ',I, IRec, ufp(ExSYN_SARB % tu % clr % lw % u(I)),         &
                                            ufp(SYN_SARB % tu % clr % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % clr % lw % d(I)) -                          &
                   ufp(SYN_SARB % tu % clr % lw % d(I)))/ufp(ExSYN_SARB % tu % clr % lw % d(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % clr % lw % d ',I, IRec, ufp(ExSYN_SARB % tu % clr % lw % d(I)),         &
                                            ufp(SYN_SARB % tu % clr % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,6
          
	  Delta = (ufp(ExSYN_SARB % tu % clr % sfc % direct) - &
	           ufp(SYN_SARB % tu % clr % sfc % direct)) / ufp(ExSYN_SARB % tu % clr % sfc % direct)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % clr % sfc % direct ',IRec, ufp(ExSYN_SARB % tu % clr % sfc % direct),      &
                                            ufp(SYN_SARB % tu % clr % sfc % direct)
	  END IF

	  Delta = (ufp(ExSYN_SARB % tu % clr % sfc % diffuse) - &
	           ufp(SYN_SARB % tu % clr % sfc % diffuse)) / ufp(ExSYN_SARB % tu % clr % sfc % diffuse)  
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % clr % sfc % diffuse ',IRec, ufp(ExSYN_SARB % tu % clr % sfc % diffuse),      &
                                            ufp(SYN_SARB % tu % clr % sfc % diffuse)
          END IF


     DO I = 1, 6
	 
	 
! *** Tuned total shortwave flux

          Delta = (ufp(ExSYN_SARB % tu % tot % sw % u(I)) -                           &
                  ufp(SYN_SARB % tu % tot % sw % u(I))) / ufp(ExSYN_SARB % tu % tot % sw % u(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % tot % sw % u ',I, IRec, ufp(ExSYN_SARB % tu % tot % sw % u(I)),         &
                                            ufp(SYN_SARB % tu % tot % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % tot % sw % d(I)) -                           &
                    ufp(SYN_SARB % tu % tot % sw % d(I))) / ufp(ExSYN_SARB % tu % tot % sw % d(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % tot % sw % d ',I, IRec, ufp(ExSYN_SARB % tu % tot % sw % d(I)),         &
                                            ufp(SYN_SARB % tu % tot % sw % d(I))
           END IF
          ENDIF

! *** Tuned total longwave flux

	  
	  Delta = (ufp(ExSYN_SARB % tu % tot % lw % u(I)) -                           &
                   ufp(SYN_SARB % tu % tot % lw % u(I))) / ufp(ExSYN_SARB % tu % tot % lw % u(I)) 
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % tot % lw % u ',I, IRec, ufp(ExSYN_SARB % tu % tot % lw % u(I)),         &
                                            ufp(SYN_SARB % tu % tot % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % tot % lw % d(I)) -                           &
                   ufp(SYN_SARB % tu % tot % lw % d(I))) / ufp(ExSYN_SARB % tu % tot % lw % d(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % tot % lw % d ',I, IRec, ufp(ExSYN_SARB % tu % tot % lw % d(I)),         &
                                            ufp(SYN_SARB % tu % tot % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,6
          
	  Delta = (ufp(ExSYN_SARB % tu % tot % sfc % direct) - & 
	          ufp(SYN_SARB % tu % tot % sfc % direct)) / ufp(ExSYN_SARB % tu % tot % sfc % direct)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % tot % sfc % direct ',IRec, ufp(ExSYN_SARB % tu % tot % sfc % direct),      &
                                            ufp(SYN_SARB % tu % tot % sfc % direct)
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % tot % sfc % diffuse) - & 
	          ufp(SYN_SARB % tu % tot % sfc % diffuse)) / ufp(ExSYN_SARB % tu % tot % sfc % diffuse) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % tot % sfc % diffuse ',IRec, ufp(ExSYN_SARB % tu % tot % sfc % diffuse),      &
                                            ufp(SYN_SARB % tu % tot % sfc % diffuse)
          END IF


     DO I = 1, 6
	 
	 
! *** Tuned prs shortwave flux

	  Delta = (ufp(ExSYN_SARB % tu % prs % sw % u(I)) -                           &
                   ufp(SYN_SARB % tu % prs % sw % u(I))) / ufp(ExSYN_SARB % tu % prs % sw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % prs % sw % u ',I, IRec, ufp(ExSYN_SARB % tu % prs % sw % u(I)),         &
                                            ufp(SYN_SARB % tu % prs % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % prs % sw % d(I)) -                          &
                       ufp(SYN_SARB % tu % prs % sw % d(I))) / ufp(ExSYN_SARB % tu % prs % sw % d(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % prs % sw % d ',I, IRec, ufp(ExSYN_SARB % tu % prs % sw % d(I)),         &
                                            ufp(SYN_SARB % tu % prs % sw % d(I))
           END IF
          ENDIF

! *** Tuned prs longwave flux

	  Delta = (ufp(ExSYN_SARB % tu % prs % lw % u(I)) -                           &
                  ufp(SYN_SARB % tu % prs % lw % u(I))) / ufp(ExSYN_SARB % tu % prs % lw % u(I)) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % prs % lw % u ',I, IRec, ufp(ExSYN_SARB % tu % prs % lw % u(I)),         &
                                            ufp(SYN_SARB % tu % prs % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	  Delta = (ufp(ExSYN_SARB % tu % prs % lw % d(I)) -                           &
                  ufp(SYN_SARB % tu % prs % lw % d(I))) / ufp(ExSYN_SARB % tu % prs % lw % d(I)) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % prs % lw % d ',I, IRec, ufp(ExSYN_SARB % tu % prs % lw % d(I)),         &
                                            ufp(SYN_SARB % tu % prs % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,6
          
	  Delta = (ufp(ExSYN_SARB % tu % prs % sfc % direct) - & 
	          ufp(SYN_SARB % tu % prs % sfc % direct)) / ufp(ExSYN_SARB % tu % prs % sfc % direct)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % prs % sfc % direct ',IRec, ufp(ExSYN_SARB % tu % prs % sfc % direct),      &
                                            ufp(SYN_SARB % tu % prs % sfc % direct)
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % prs % sfc % diffuse) - & 
	          ufp(SYN_SARB % tu % prs % sfc % diffuse)) / ufp(ExSYN_SARB % tu % prs % sfc % diffuse)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % prs % sfc % diffuse ',IRec, ufp(ExSYN_SARB % tu % prs % sfc % diffuse),      &
                                            ufp(SYN_SARB % tu % prs % sfc % diffuse)
          END IF


     DO I = 1, 6
	 
	 
! *** Tuned tna shortwave flux

	  Delta = (ufp(ExSYN_SARB % tu % tna % sw % u(I)) -                           &
                  ufp(SYN_SARB % tu % tna % sw % u(I))) / ufp(ExSYN_SARB % tu % tna % sw % u(I)) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % tna % sw % u ',I, IRec, ufp(ExSYN_SARB % tu % tna % sw % u(I)),         &
                                            ufp(SYN_SARB % tu % tna % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % tna % sw % d(I)) -                           &
                   ufp(SYN_SARB % tu % tna % sw % d(I))) / ufp(ExSYN_SARB % tu % tna % sw % d(I)) 
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % tna % sw % d ',I, IRec, ufp(ExSYN_SARB % tu % tna % sw % d(I)),         &
                                            ufp(SYN_SARB % tu % tna % sw % d(I))
           END IF
          ENDIF

! *** Tuned tna longwave flux

	 Delta = (ufp(ExSYN_SARB % tu % tna % lw % u(I)) -                           &
                 ufp(SYN_SARB % tu % tna % lw % u(I))) / ufp(ExSYN_SARB % tu % tna % lw % u(I)) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % tna % lw % u ',I, IRec, ufp(ExSYN_SARB % tu % tna % lw % u(I)),         &
                                            ufp(SYN_SARB % tu % tna % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % tu % prs % lw % d(I)) -                           &
                   ufp(SYN_SARB % tu % prs % lw % d(I))) / ufp(ExSYN_SARB % tu % prs % lw % d(I))
	   IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'tu % prs % lw % d ',I, IRec, ufp(ExSYN_SARB % tu % tna % lw % d(I)),         &
                                            ufp(SYN_SARB % tu % tna % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,6
          
	  Delta = (ufp(ExSYN_SARB % tu % tna % sfc % direct) - & 
	           ufp(SYN_SARB % tu % tna % sfc % direct)) / ufp(ExSYN_SARB % tu % tna % sfc % direct)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % tna % sfc % direct ',IRec, ufp(ExSYN_SARB % tu % tna % sfc % direct),      &
                                            ufp(SYN_SARB % tu % tna % sfc % direct)
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % tna % sfc % diffuse) - & 
	          ufp(SYN_SARB % tu % tna % sfc % diffuse)) / ufp(ExSYN_SARB % tu % tna % sfc % diffuse)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % tna % sfc % diffuse ',IRec, ufp(ExSYN_SARB % tu % tna % sfc % diffuse),      &
                                            ufp(SYN_SARB % tu % tna % sfc % diffuse)
          END IF


! *** Tuning variables

	  Delta = (ExSYN_SARB % tu % var % pw -                           &
                      SYN_SARB % tu % var % pw) / ExSYN_SARB % tu % var % pw
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % var % pw ',IRec, ExSYN_SARB % tu % var % pw,         &
                                            SYN_SARB % tu % var % pw
          END IF

	  Delta = (ExSYN_SARB % tu % var % uth -                           &
                      SYN_SARB % tu % var % uth) / ExSYN_SARB % tu % var % uth
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % var % uth ',IRec, ExSYN_SARB % tu % var % uth,         &
                                            SYN_SARB % tu % var % uth
          END IF
	 
	  Delta = (ExSYN_SARB % tu % var % sfcalb -                           &
                      SYN_SARB % tu % var % sfcalb) / ExSYN_SARB % tu % var % sfcalb 
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % var % sfcalb ',IRec, ExSYN_SARB % tu % var % sfcalb,         &
                                            SYN_SARB % tu % var % sfcalb
          END IF

	  Delta = (ExSYN_SARB % tu % var % aot -                           &
                      SYN_SARB % tu % var % aot) / ExSYN_SARB % tu % var % aot
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % var % aot ',IRec, ExSYN_SARB % tu % var % aot,         &
                                            SYN_SARB % tu % var % aot
          END IF

	  Delta = (ExSYN_SARB % tu % var % skint -                           &
                      SYN_SARB % tu % var % skint) / ExSYN_SARB % tu % var % skint
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % var % skint ',IRec, ExSYN_SARB % tu % var % skint,         &
                                            SYN_SARB % tu % var % skint
          END IF

         DO I = 1, 4

	  Delta = (ExSYN_SARB % tu % var % cldfrac(I) -                           &
                      SYN_SARB % tu % var % cldfrac(I)) / ExSYN_SARB % tu % var % cldfrac(I) 
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % var % cldfrac ',I, IRec, ExSYN_SARB % tu % var % cldfrac(I),         &
                                            SYN_SARB % tu % var % cldfrac(I)
          END IF

	  Delta = (ExSYN_SARB % tu % var % cldtemp(I) -                           &
                      SYN_SARB % tu % var % cldtemp(I)) / ExSYN_SARB % tu % var % cldtemp(I)
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % var % cldtemp ',I, IRec, ExSYN_SARB % tu % var % cldtemp(I),         &
                                            SYN_SARB % tu % var % cldtemp(I)
          END IF

	  Delta = (ExSYN_SARB % tu % var % cldtau(I) -                           &
                      SYN_SARB % tu % var % cldtau(I)) / ExSYN_SARB % tu % var % cldtau(I)
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'tu % var % cldtau ',I, IRec, ExSYN_SARB % tu % var % cldtau(I),         &
                                            SYN_SARB % tu % var % cldtau(I)
          END IF
         
	 ENDDO  ! End cld comp


	  Delta = (ufp(ExSYN_SARB % tu % as % toa_uva) - & 
	          ufp(SYN_SARB % tu % as % toa_uva)) / ufp(ExSYN_SARB % tu % as % toa_uva) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % toa_uva ',IRec, ufp(ExSYN_SARB % tu % as % toa_uva),      &
                                            ufp(SYN_SARB % tu % as % toa_uva) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % toa_uvb) - & 
	          ufp(SYN_SARB % tu % as % toa_uvb)) / ufp(ExSYN_SARB % tu % as % toa_uvb)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % toa_uvb ',IRec, ufp(ExSYN_SARB % tu % as % toa_uvb),      &
                                            ufp(SYN_SARB % tu % as % toa_uvb) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % toa_par) - & 
	          ufp(SYN_SARB % tu % as % toa_par)) / ufp(ExSYN_SARB % tu % as % toa_par)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % toa_par ',IRec, ufp(ExSYN_SARB % tu % as % toa_par),      &
                                            ufp(SYN_SARB % tu % as % toa_par) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_tot_uva) - &
	          ufp(SYN_SARB % tu % as % sfc_tot_uva)) / ufp(ExSYN_SARB % tu % as % sfc_tot_uva) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_tot_uva ',IRec, ufp(ExSYN_SARB % tu % as % sfc_tot_uva),      &
                                            ufp(SYN_SARB % tu % as % sfc_tot_uva) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_tot_uvb) - & 
	          ufp(SYN_SARB % tu % as % sfc_tot_uvb)) / ufp(ExSYN_SARB % tu % as % sfc_tot_uvb)
 	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_tot_uvb ',IRec, ufp(ExSYN_SARB % tu % as % sfc_tot_uvb),      &
                                            ufp(SYN_SARB % tu % as % sfc_tot_uvb) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_tot_uvi) - &
	          ufp(SYN_SARB % tu % as % sfc_tot_uvi)) / ufp(ExSYN_SARB % tu % as % sfc_tot_uvi)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_tot_uvi ',IRec, ufp(ExSYN_SARB % tu % as % sfc_tot_uvi),      &
                                            ufp(SYN_SARB % tu % as % sfc_tot_uvi) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_tot_par_dir) - & 
	          ufp(SYN_SARB % tu % as % sfc_tot_par_dir)) / ufp(ExSYN_SARB % tu % as % sfc_tot_par_dir)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_tot_par_dir ',IRec, ufp(ExSYN_SARB % tu % as % sfc_tot_par_dir),      &
                                            ufp(SYN_SARB % tu % as % sfc_tot_par_dir) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_tot_par_dif) - &
	           ufp(SYN_SARB % tu % as % sfc_tot_par_dif)) / ufp(ExSYN_SARB % tu % as % sfc_tot_par_dif)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_tot_par_dif ',IRec, ufp(ExSYN_SARB % tu % as % sfc_tot_par_dif),      &
                                            ufp(SYN_SARB % tu % as % sfc_tot_par_dif) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_clr_par_dir) - & 
	          ufp(SYN_SARB % tu % as % sfc_clr_par_dir)) / ufp(ExSYN_SARB % tu % as % sfc_clr_par_dir) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_clr_par_dir ',IRec, ufp(ExSYN_SARB % tu % as % sfc_clr_par_dir),      &
                                            ufp(SYN_SARB % tu % as % sfc_clr_par_dir) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_clr_par_dif) - &
	          ufp(SYN_SARB % tu % as % sfc_clr_par_dif)) / ufp(ExSYN_SARB % tu % as % sfc_clr_par_dif)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_clr_par_dif ',IRec, ufp(ExSYN_SARB % tu % as % sfc_clr_par_dif),      &
                                            ufp(SYN_SARB % tu % as % sfc_clr_par_dif) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_prs_par_dir) - &
	          ufp(SYN_SARB % tu % as % sfc_prs_par_dir)) / ufp(ExSYN_SARB % tu % as % sfc_prs_par_dir)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_prs_par_dir ',IRec, ufp(ExSYN_SARB % tu % as % sfc_prs_par_dir),      &
                                            ufp(SYN_SARB % tu % as % sfc_prs_par_dir) 
          END IF

	  Delta = (ufp(ExSYN_SARB % tu % as % sfc_prs_par_dif) - &
	          ufp(SYN_SARB % tu % as % sfc_prs_par_dif)) / ufp(ExSYN_SARB % tu % as % sfc_prs_par_dif) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % as % sfc_prs_par_dif ',IRec, ufp(ExSYN_SARB % tu % as % sfc_prs_par_dif),      &
                                            ufp(SYN_SARB % tu % as % sfc_prs_par_dif) 
          END IF


	  Delta = (ExSYN_SARB % tu % ent % toa_swe_inc - & 
	          SYN_SARB % tu % ent % toa_swe_inc) / ExSYN_SARB % tu % ent % toa_swe_inc
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % toa_swe_inc ',IRec, ExSYN_SARB % tu % ent % toa_swe_inc,      &
                                           SYN_SARB % tu % ent % toa_swe_inc
          END IF

	  Delta = (ExSYN_SARB % tu % ent % toa_lwe_out - & 
	          SYN_SARB % tu % ent % toa_lwe_out) / ExSYN_SARB % tu % ent % toa_lwe_out
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % toa_lwe_out ',IRec, ExSYN_SARB % tu % ent % toa_lwe_out,      &
                                            SYN_SARB % tu % ent % toa_lwe_out
          END IF

	  Delta = (ExSYN_SARB % tu % ent % sfc_swe_inc - &
	          SYN_SARB % tu % ent % sfc_swe_inc) / ExSYN_SARB % tu % ent % sfc_swe_inc
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % sfc_swe_inc ',IRec, ExSYN_SARB % tu % ent % sfc_swe_inc,      &
                                            SYN_SARB % tu % ent % sfc_swe_inc
          END IF

	  Delta = (ExSYN_SARB % tu % ent % sfc_swe_gen - &
	          SYN_SARB % tu % ent % sfc_swe_gen) / ExSYN_SARB % tu % ent % sfc_swe_gen
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % sfc_swe_gen ',IRec, ExSYN_SARB % tu % ent % sfc_swe_gen,      &
                                            SYN_SARB % tu % ent % sfc_swe_gen
          END IF

	  Delta = (ExSYN_SARB % tu % ent % sfc_lwe_out - & 
	          SYN_SARB % tu % ent % sfc_lwe_out) / ExSYN_SARB % tu % ent % sfc_lwe_out
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % sfc_lwe_out ',IRec, ExSYN_SARB % tu % ent % sfc_lwe_out,      &
                                            SYN_SARB % tu % ent % sfc_lwe_out
          END IF

	  Delta = (ExSYN_SARB % tu % ent % sfc_lwe_up - & 
	          SYN_SARB % tu % ent % sfc_lwe_up) / ExSYN_SARB % tu % ent % sfc_lwe_up
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % sfc_lwe_up ',IRec, ExSYN_SARB % tu % ent % sfc_lwe_up,      &
                                            SYN_SARB % tu % ent % sfc_lwe_up
          END IF

	  Delta = (ExSYN_SARB % tu % ent % sfc_lwe_dn - &
	          SYN_SARB % tu % ent % sfc_lwe_dn) / ExSYN_SARB % tu % ent % sfc_lwe_dn
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % sfc_lwe_dn ',IRec, ExSYN_SARB % tu % ent % sfc_lwe_dn,      &
                                            SYN_SARB % tu % ent % sfc_lwe_dn
          END IF

	  Delta = (ExSYN_SARB % tu % ent % sfc_lwe_gen - & 
	          SYN_SARB % tu % ent % sfc_lwe_gen) / ExSYN_SARB % tu % ent % sfc_lwe_gen
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % sfc_lwe_gen ',IRec, ExSYN_SARB % tu % ent % sfc_lwe_gen,      &
                                            SYN_SARB % tu % ent % sfc_lwe_gen
          END IF



	  Delta = (ExSYN_SARB % tu % ent % atm_swe_inc - & 
	          SYN_SARB % tu % ent % atm_swe_inc) / ExSYN_SARB % tu % ent % atm_swe_inc
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % atm_swe_inc ',IRec, ExSYN_SARB % tu % ent % atm_swe_inc,      &
                                            SYN_SARB % tu % ent % atm_swe_inc
          END IF

	  Delta = (ExSYN_SARB % tu % ent % atm_swe_gen - & 
	          SYN_SARB % tu % ent % atm_swe_gen) / ExSYN_SARB % tu % ent % atm_swe_gen
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % atm_swe_gen ',IRec, ExSYN_SARB % tu % ent % atm_swe_gen,      &
                                            SYN_SARB % tu % ent % atm_swe_gen
          END IF

	  Delta = (ExSYN_SARB % tu % ent % atm_lwe_out - &
	          SYN_SARB % tu % ent % atm_lwe_out) / ExSYN_SARB % tu % ent % atm_lwe_out
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % atm_lwe_out ',IRec, ExSYN_SARB % tu % ent % atm_lwe_out,      &
                                            SYN_SARB % tu % ent % atm_lwe_out
          END IF

	  Delta = (ExSYN_SARB % tu % ent % atm_lwe_gen - & 
	          SYN_SARB % tu % ent % atm_lwe_gen) / ExSYN_SARB % tu % ent % atm_lwe_gen
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'tu % ent % atm_lwe_gen ',IRec, ExSYN_SARB % tu % ent % atm_lwe_gen,      &
                                            SYN_SARB % tu % ent % atm_lwe_gen
          END IF

!
! *** UNTUNED variables
!

     DO I = 1, 2
	 
	 
! *** Untuned clear shortwave flux

	  Delta = (ufp(ExSYN_SARB % ut % clr % sw % u(I)) -                           &
                      ufp(SYN_SARB % ut % clr % sw % u(I))) / ufp(ExSYN_SARB % ut % clr % sw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % clr % sw % u ',I, IRec, ufp(ExSYN_SARB % ut % clr % sw % u(I)),         &
                                            ufp(SYN_SARB % ut % clr % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % clr % sw % d(I)) -                           &
                       ufp(SYN_SARB % ut % clr % sw % d(I))) / ufp(ExSYN_SARB % ut % clr % sw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % clr % sw % d ',I, IRec, ufp(ExSYN_SARB % ut % clr % sw % d(I)),         &
                                            ufp(SYN_SARB % ut % clr % sw % d(I))
           END IF
          ENDIF

! *** Untuned clear longwave flux

	  Delta = (ufp(ExSYN_SARB % ut % clr % lw % u(I)) -                           &
                      ufp(SYN_SARB % ut % clr % lw % u(I))) / ufp(ExSYN_SARB % ut % clr % lw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % clr % lw % u ',I, IRec, ufp(ExSYN_SARB % ut % clr % lw % u(I)),         &
                                            ufp(SYN_SARB % ut % clr % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % clr % lw % d(I)) -                           &
                       ufp(SYN_SARB % ut % clr % lw % d(I))) / ufp(ExSYN_SARB % ut % clr % lw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % clr % lw % d ',I, IRec, ufp(ExSYN_SARB % ut % clr % lw % d(I)),         &
                                            ufp(SYN_SARB % ut % clr % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,2
          


     DO I = 1, 2
	 
	 
! *** Untuned total shortwave flux

	  Delta = (ufp(ExSYN_SARB % ut % tot % sw % u(I)) -                           &
                      ufp(SYN_SARB % ut % tot % sw % u(I))) / ufp(ExSYN_SARB % ut % tot % sw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % tot % sw % u ',I, IRec, ufp(ExSYN_SARB % ut % tot % sw % u(I)),         &
                                           ufp( SYN_SARB % ut % tot % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % tot % sw % d(I)) -                           &
                       ufp(SYN_SARB % ut % tot % sw % d(I))) / ufp(ExSYN_SARB % ut % tot % sw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % tot % sw % d ',I, IRec, ufp(ExSYN_SARB % ut % tot % sw % d(I)),         &
                                            ufp(SYN_SARB % ut % tot % sw % d(I))
           END IF
          ENDIF

! *** Untuned total longwave flux

	  Delta = (ufp(ExSYN_SARB % ut % tot % lw % u(I)) -                           &
                      ufp(SYN_SARB % ut % tot % lw % u(I))) / ufp(ExSYN_SARB % ut % tot % lw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % tot % lw % u ',I, IRec, ufp(ExSYN_SARB % ut % tot % lw % u(I)),         &
                                            ufp(SYN_SARB % ut % tot % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % tot % lw % d(I)) -                           &
                       ufp(SYN_SARB % ut % tot % lw % d(I))) / ufp(ExSYN_SARB % ut % tot % lw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % tot % lw % d ',I, IRec, ufp(ExSYN_SARB % ut % tot % lw % d(I)),         &
                                            ufp(SYN_SARB % ut % tot % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,2
          


     DO I = 1, 2
	 
	 
! *** Tuned prs shortwave flux

	  Delta = (ufp(ExSYN_SARB % ut % prs % sw % u(I)) -                           &
                      ufp(SYN_SARB % ut % prs % sw % u(I))) / ufp(ExSYN_SARB % ut % prs % sw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % prs % sw % u ',I, IRec, ufp(ExSYN_SARB % ut % prs % sw % u(I)),         &
                                            ufp(SYN_SARB % ut % prs % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % prs % sw % d(I)) -                           &
                       ufp(SYN_SARB % ut % prs % sw % d(I))) / ufp(ExSYN_SARB % ut % prs % sw % d(I)) 
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % prs % sw % d ',I, IRec, ufp(ExSYN_SARB % ut % prs % sw % d(I)),         &
                                            ufp(SYN_SARB % ut % prs % sw % d(I))
           END IF
          ENDIF

! *** Tuned prs longwave flux

	  Delta = (ufp(ExSYN_SARB % ut % prs % lw % u(I)) -                           &
                      ufp(SYN_SARB % ut % prs % lw % u(I))) / ufp(ExSYN_SARB % ut % prs % lw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % prs % lw % u ',I, IRec, ufp(ExSYN_SARB % ut % prs % lw % u(I)),         &
                                            ufp(SYN_SARB % ut % prs % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % prs % lw % d(I)) -                           &
                       ufp(SYN_SARB % ut % prs % lw % d(I))) / ufp(ExSYN_SARB % ut % prs % lw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % prs % lw % d ',I, IRec, ufp(ExSYN_SARB % ut % prs % lw % d(I)),         &
                                            ufp(SYN_SARB % ut % prs % lw % d(I))
           END IF
          ENDIF

!
!
        
	 ENDDO  ! End I = 1,2
          


     DO I = 1, 2
	 
	 
! *** Tuned tna shortwave flux

	  Delta = (ufp(ExSYN_SARB % ut % tna % sw % u(I)) -                           &
                      ufp(SYN_SARB % ut % tna % sw % u(I))) / ufp(ExSYN_SARB % ut % tna % sw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % tna % sw % u ',I, IRec, ufp(ExSYN_SARB % ut % tna % sw % u(I)),         &
                                            ufp(SYN_SARB % ut % tna % sw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % tna % sw % d(I)) -                           &
                       ufp(SYN_SARB % ut % tna % sw % d(I))) / ufp(ExSYN_SARB % ut % tna % sw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % tna % sw % d ',I, IRec, ufp(ExSYN_SARB % ut % tna % sw % d(I)),         &
                                            ufp(SYN_SARB % ut % tna % sw % d(I))
           END IF
          ENDIF

! *** Tuned tna longwave flux

	  Delta = (ufp(ExSYN_SARB % ut % tna % lw % u(I)) -                          &
                      ufp(SYN_SARB % ut % tna % lw % u(I))) / ufp(ExSYN_SARB % ut % tna % lw % u(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % tna % lw % u ',I, IRec, ufp(ExSYN_SARB % ut % tna % lw % u(I)),         &
                                            ufp(SYN_SARB % ut % tna % lw % u(I))
          END IF

	  
	  IF (I > 1) THEN
	   Delta = (ufp(ExSYN_SARB % ut % tna % lw % d(I)) -                           &
                       ufp(SYN_SARB % ut % tna % lw % d(I))) / ufp(ExSYN_SARB % ut % tna % lw % d(I))
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
               IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
               MisRec = 1
               WRITE (Diag_Unit, 2110) 'ut % tna % lw % d ',I, IRec, ufp(ExSYN_SARB % ut % tna % lw % d(I)),         &
                                            ufp(SYN_SARB % ut % tna % lw % d(I))
           END IF
          ENDIF

!
!        
	 ENDDO  ! End I = 1,2
          


! *** Tuning variables

	  Delta = (ExSYN_SARB % ut % var % pw -                           &
                      SYN_SARB % ut % var % pw) / ExSYN_SARB % ut % var % pw
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'ut % var % pw ',IRec, ExSYN_SARB % ut % var % pw,         &
                                            SYN_SARB % ut % var % pw
          END IF

	  Delta = (ExSYN_SARB % ut % var % uth -                           &
                      SYN_SARB % ut % var % uth) / ExSYN_SARB % ut % var % uth
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'ut % var % uth ',IRec, ExSYN_SARB % ut % var % uth,         &
                                            SYN_SARB % ut % var % uth
          END IF
	 
	  Delta = (ExSYN_SARB % ut % var % sfcalb -                           &
                      SYN_SARB % ut % var % sfcalb) / ExSYN_SARB % ut % var % sfcalb
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'ut % var % sfcalb ',IRec, ExSYN_SARB % ut % var % sfcalb,         &
                                            SYN_SARB % ut % var % sfcalb
          END IF

	  Delta = (ExSYN_SARB % ut % var % aot -                           &
                      SYN_SARB % ut % var % aot) / ExSYN_SARB % ut % var % aot
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'ut % var % aot ',IRec, ExSYN_SARB % ut % var % aot,         &
                                            SYN_SARB % ut % var % aot
          END IF

	  Delta = (ExSYN_SARB % ut % var % skint -                           &
                      SYN_SARB % ut % var % skint) / ExSYN_SARB % ut % var % skint 
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'ut % var % skint ',IRec, ExSYN_SARB % ut % var % skint,         &
                                            SYN_SARB % ut % var % skint
          END IF

         DO I = 1, 4

	  Delta = (ExSYN_SARB % ut % var % cldfrac(I) -                           &
                      SYN_SARB % ut % var % cldfrac(I)) / ExSYN_SARB % ut % var % cldfrac(I)
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % var % cldfrac ',I, IRec, ExSYN_SARB % ut % var % cldfrac(I),         &
                                            SYN_SARB % ut % var % cldfrac(I)
          END IF

	  Delta = (ExSYN_SARB % ut % var % cldtemp(I) -                           &
                      SYN_SARB % ut % var % cldtemp(I)) / ExSYN_SARB % ut % var % cldtemp(I)
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % var % cldtemp ',I, IRec, ExSYN_SARB % ut % var % cldtemp(I),         &
                                            SYN_SARB % ut % var % cldtemp(I)
          END IF

	  Delta = (ExSYN_SARB % ut % var % cldtau(I) -                           &
                      SYN_SARB % ut % var % cldtau(I)) / ExSYN_SARB % ut % var % cldtau(I) 
          IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2110) 'ut % var % cldtau ',I, IRec, ExSYN_SARB % ut % var % cldtau(I),         &
                                            SYN_SARB % ut % var % cldtau(I)
          END IF
         
	 ENDDO  ! End cld comp



	  IF (ExSYN_SARB % misc % hr_month /= SYN_SARB % misc % hr_month) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % hr_month ',IRec, ExSYN_SARB % misc % hr_month,      &
                                            SYN_SARB % misc % hr_month
          END IF

	  IF (ExSYN_SARB % misc % regnum /= SYN_SARB % misc % regnum) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % regnum ',IRec, ExSYN_SARB % misc % regnum,      &
                                            SYN_SARB % misc % regnum
          END IF



	  Delta = (ufp(ExSYN_SARB % misc % toaswinsolation) - & 
	           ufp(SYN_SARB % misc % toaswinsolation)) / ufp(ExSYN_SARB % misc % toaswinsolation)
	  IF (ABS(Delta) > MaxDeltaPercent) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2100) 'misc % toaswinsolation ',IRec, ufp(ExSYN_SARB % misc % toaswinsolation),      &
                                            ufp(SYN_SARB % misc % toaswinsolation)
          END IF

	  IF (ExSYN_SARB % misc % sfcalbedo_source /= SYN_SARB % misc % sfcalbedo_source) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % sfcalbedo_source ',IRec, ExSYN_SARB % misc % sfcalbedo_source,      &
                                            SYN_SARB % misc % sfcalbedo_source
          END IF

	  DO I = 1,2
	  
	  IF (ExSYN_SARB % misc % error(I) /= SYN_SARB % misc % error(I)) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % error ',IRec, ExSYN_SARB % misc % error(I),      &
                                            SYN_SARB % misc % error(I)
          END IF

	  ENDDO
	  
	  IF (ExSYN_SARB % misc % sfc_pres /= SYN_SARB % misc % sfc_pres) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % sfc_pres ',IRec, ExSYN_SARB % misc % sfc_pres,      &
                                            SYN_SARB % misc % sfc_pres
          END IF

	  IF (ExSYN_SARB % misc % sfc_hgt /= SYN_SARB % misc % sfc_hgt) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % sfc_hgt ',IRec, ExSYN_SARB % misc % sfc_hgt,      &
                                            SYN_SARB % misc % sfc_hgt
          END IF

	  IF (ExSYN_SARB % misc % sfc_lev /= SYN_SARB % misc % sfc_lev) THEN
              IF (MisRec == 0) ICnt_MisMatch = ICnt_MisMatch + 1
              MisRec = 1
              WRITE (Diag_Unit, 2000) 'misc % sfc_lev ',IRec, ExSYN_SARB % misc % sfc_lev,      &
                                            SYN_SARB % misc % sfc_lev
          END IF


      ENDDO  ! End IRec loop
!
!
!
! *** Print total number of mismatches
      
      WRITE (Diag_Unit,8000) Icnt_MisMatch
      WRITE (Diag_Unit,8001) IFOV2
      IF (Icnt_MisMatch == 0) print*,'SUCCESSFUL' 
      
      IF (Icnt_MisMatch /= 0) THEN
       print*,'ERROR: Check comparison file'
      ENDIF 
!
!
! *** FORMAT STATMENTS
 1000 FORMAT ( 1x,'SYNI Expected = ',a200,//)
 1005 FORMAT ( 1x,'SYNI Computed = ',a200,//)
 1010 FORMAT ( 1x,58x,a13,7x,a13,//)

 2000 FORMAT ( 1x,a25,15x,'Rec # ',i6,3x,' SYN1: ',i9,5x,' SYN2: ',i9)
 2010 FORMAT ( 1x,a25,3x,'Level ',i2,3x,' Rec # ',i6,3x,' SYN1: ',i9,5x,' SYN2: ',i9)

 2100 FORMAT ( 1x,a25,15x,'Rec # ',i6,3x,' SYN1: ',f11.6,3x,' SYN2: ',f11.6)
 2110 FORMAT ( 1x,a25,3x,'Level ',i2,3x,' Rec # ',i6,3x,' SYN1: ',f11.6,3x,' SYN2: ',f11.6)

 8000 FORMAT ( 1x,35x,'TOTAL RECORD MISMATCHES: ',i6)
 8001 FORMAT ( 1x,35x,'TOTAL Number of RECORDS: ',i6)         
!
!
END PROGRAM SYNI_Compare
















