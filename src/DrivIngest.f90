MODULE DrivIngest
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   DrivIngest
!
! !Description:
!    Routine ID - M5.1.2
!
!    Purpose - To provide a module of routines to open and ingest SARB
!              derivative tables.
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
!  Internal Routines:
!    Deriv_Init - Drives the ingestion of SARB's derivative tables
!    dt_load    - Loads SARB's derivative tables
!
!  Modules Used:
!    DrivTab_Var        - (SARBlib)  Used by Sbr. dt_load
!    iois called                  - (CERESlib) Used by Sbr. Deriv_Init
!    SARB_Error_Process - (SARBlib)  Used by Sbr. dt_load
!    SARB_Var           - (SARBlib)  Used by Sbr. Deriv_Init
!
! !Revision History:
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Modified Sbr. Deriv_Init
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Modified Sbr. dt_load
!                 Updated prolog for Sbr. Deriv_Init
!  Version 2.0   November 21, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
      USE io
      USE pcf
      USE SARB_Error_Process
      USE SARB_Var
!
      IMPLICIT NONE
!
!
!
CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Deriv_Init (LID_DrivTab)                                ! I
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    Deriv_Init
!
! !Description:
!   Routine ID - 5.1.2
!
!   Purpose - To drive the opening and reading of the derivative table file
!             used by the SARB tuning process
!
! !Input Parameters:
!    LID_DrivTab - Logical ID of derivative tables in the PCF
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Modules Used:
!    IO       - Sbr. CloseFile, OpenFile
!    SARB_Var - Parameters: DrivTab_Unit
!
!  Internal Routines Used:
!    dt_load
!
! !Revision History:
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Removed PCF LID and unit number declarations statements
!                     to Mod. SARB_Var
!                  2. Renamed PCF LID parameter from DrivTab_ID to LID_DrivTab
!  Version 2.0   November 21, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN) :: LID_DrivTab
!
      INTEGER ::  IOS
!
!
      CALL OpenFile (LogicID = LID_DrivTab, status = IOS, &
                     handle = DrivTab_Unit, &
                     mode = "R", access ="S", &
                     format = "U")
!
      CALL dt_load (DrivTab_Unit)
!
      CALL CloseFile (file_handle = DrivTab_Unit, status = IOS)
!
      END SUBROUTINE Deriv_Init
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE dt_load ( iunit)                                 ! I
!
!**************************************************************************
!
! !F90
!
! !Name:
!    dt_load
!
! !Description:
!    Routine ID - 5.1.2.1
!
!    Purpose -
!
!
! !Input Parameters:
!    iunit - Derivative table unit number obtained via toolkit
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header
!
!  Modules Used:
!    DrivTab_Var         - (SARBlib)  Parameters: 
!    SARB_Error_Process  - (SARBlib)  Sbr. DerivLoad_Error_Process
!                                          Parameters: IVar1_Temp, IVar2_Temp
!
! !Revision History:
!  Version 2.4   March 15, 1999 - SCCR #132
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. Initialized IVARn varaibles to zero.
!  Version 2.0   November 21, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!**************************************************************************
!
!
      USE DrivTab_Var
!
!
      INTEGER, INTENT (IN) :: iunit
!
      CHARACTER (LEN=80)         :: cx
      INTEGER                    :: i
      INTEGER, SAVE, ALLOCATABLE :: ib ( :)
      INTEGER                    :: ic ( 20)
      INTEGER, SAVE, ALLOCATABLE :: ie ( :)
      INTEGER                    :: IErr
      CHARACTER (LEN=80), SAVE   :: ilab (2, maxstate, maxsubtab)
      INTEGER                    :: ioff
      INTEGER                    :: ios
      INTEGER                    :: isv
      INTEGER                    :: itv
      INTEGER                    :: itbl
      INTEGER                    :: ix
      INTEGER                    :: iz
      INTEGER                    :: k
      INTEGER                    :: kk
      INTEGER                    :: nbyte
      INTEGER                    :: ntot_word 
      INTEGER, SAVE, ALLOCATABLE :: nword ( :)
      REAL,    SAVE              :: rnode ( maxnode, maxstate, maxsubtab)
      REAL                       :: rx
      INTEGER :: lenrec, IVar1_Temp, IVar2_Temp, IVar3_Temp, IVar4_Temp
!
!
      IErr = 0
      IVAR1_Temp = 0
      IVAR2_Temp = 0
      IVAR3_Temp = 0
      IVAR4_Temp = 0
!
      READ (iunit, IOSTAT=IOS) ntbl
      IF (IOS /= 0) THEN
          IErr = 1
          IVar1_Temp = IOS
          CALL DerivLoad_Error_Process (IErr, IVar1_Temp, IVar2_Temp, &
                                        IVar3_Temp, IVar4_Temp)
      END IF
      ALLOCATE (ib ( ntbl+1)) 
      ALLOCATE (ie ( ntbl))

      ALLOCATE (nword ( ntbl))
      READ (iunit, IOSTAT=IOS) (nword ( k), k = 1, ntbl)
      IF (IOS /= 0) THEN
          IErr = 2
          IVar1_Temp = IOS
          CALL DerivLoad_Error_Process (IErr, IVar1_Temp, IVar2_Temp, &
                                        IVar3_Temp, IVar4_Temp)
      END IF
      ntot_word = SUM (nword)
      IF (ntbl > maxsubtab) THEN
          IVar1_Temp = ntbl
          IVar2_Temp = maxsubtab
          IErr = 3
          CALL DerivLoad_Error_Process (IErr, IVar1_Temp, IVar2_Temp, &
                                        IVar3_Temp, IVar4_Temp)
      END IF
      ALLOCATE (bt ( ntot_word ) )

      INQUIRE (iolength=lenrec) bt
      ib ( 1) = 1
      DO k = 1, ntbl
       ie ( k) = ib ( k) + nword ( k) - 1
       read (iunit, IOSTAT=IOS) bt ( ib ( k) : ie ( k))
          IF (IOS /= 0) THEN
              IVar1_Temp = k
              IVar2_Temp = ie ( k)
              IVar3_Temp = ib ( k)
              IVar4_Temp = IOS
              IErr = 4
              CALL DerivLoad_Error_Process (IErr, IVar1_Temp, IVar2_Temp, &
                                            IVar3_Temp, IVar4_Temp)
          END IF
       ib ( k+1) = ie ( k) + 1
      END DO

! *** Precompute table housekeeping 'dt_setup'

      DO itbl = 1, ntbl

       iz = ib ( itbl) - 1 !!! Begining of table 'in words' in bt

       ntv ( itbl) = bt ( iz+1)
       nsv ( itbl) = bt ( iz+2)

       DO i = 1, ntv ( itbl) 
        ix =  bt( iz+2+i) 
        dsig ( i, itbl) = TRANSFER (ix, rx) 
       END DO

       DO i = 1, nsv ( itbl) 
        nsizes ( i, itbl) = bt ( iz + 2 + ntv ( itbl) + i) 
       END DO

!     *** labels
       DO itv = 1, ntv ( itbl)
        DO k = 1, 20
           ic ( k) = bt (   iz + 2 + ntv ( itbl)                   &
                                 + nsv ( itbl) + (itv-1)*20 + k)
        END DO
        ilab ( 1, itv, itbl) = TRANSFER (ic ( 1:20), cx) !cx
       END DO

       DO isv = 1, nsv ( itbl)
        DO k = 1,20
           ic ( k) = bt (  iz + 2 + ntv ( itbl) + nsv ( itbl)      &
                                 + ntv ( itbl)*20 + (isv-1)*20 + k) 
        END DO
        ilab ( 2, isv, itbl) = TRANSFER (ic ( 1:20), cx) !cx
       END DO

!     *** Nodes
       ioff =   iz + 2 + ntv ( itbl) + nsv ( itbl)                      &
                   + ntv ( itbl)*20 + nsv ( itbl)*20 

       kk = 0
       DO isv = 1, nsv ( itbl)
        DO k = 1, nsizes ( isv, itbl) 
         kk = kk + 1
         ix = bt ( ioff+kk)
         rnode ( k, isv, itbl) = TRANSFER (ix, rx)
        END DO 
        bnds ( 1, isv, itbl) = rnode ( 1, isv, itbl)
        bnds ( 2, isv, itbl) = rnode ( nsizes ( isv, itbl), isv, itbl)
       END DO

       ioff = ioff + kk +1

       nbyte = bt ( ioff)

       ioffs ( itbl) = ioff

!     *** Offsets 
       itdim ( itbl) = 1
       os ( 1, itbl) = 1
       DO isv = 1, nsv ( itbl)
        os ( isv, itbl) = itdim ( itbl)
        itdim ( itbl) = itdim ( itbl) * nsizes ( isv, itbl)
        rng ( isv, itbl) = bnds ( 2, isv, itbl) - bnds ( 1, isv, itbl)
       END DO

      END DO ! itbl
!
!
      END SUBROUTINE dt_load
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
END MODULE DrivIngest
