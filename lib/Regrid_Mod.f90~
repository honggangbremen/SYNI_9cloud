MODULE Regrid_Mod

      USE ceres_defaults, ONLY: Real4_Dflt
      USE MSG, ONLY: WriteReport
      USE grid_values,  ONLY:     &
       LatOff_CERESNESTED,        &  
       LongOff_CERESNESTED,       &  
       NumBands_CERESNESTED,      & 
       NumReg_Global_CERESNESTED, &
       RegHgt_CERESNESTED,        &
       CLat_CERESNESTED,          &
       CLong_CERESNESTED,         &
       BegReg_CERESNESTED,        &              
       EndReg_CERESNESTED,        &  
       NumReg_CERESNESTED,        &
       zone
       
      USE MOA_IO_DEF

!
      IMPLICIT NONE
!
      INCLUDE 'PGS_GRID_26509.f'
!
!
! *** DEFAULT value
!
      REAL, PARAMETER :: Default_MOA_REAL   =  Real4_Dflt

!
! *** Max allowable default input area percentage before target is default
!
      REAL, PARAMETER :: DPerAllow_CERES05Deg = 0.2
!
!
! *** CERES 0.5 degree equal angle grid parameters  ( 0.5 x 0.5 deg equal angle grid)
!
      REAL,    PARAMETER :: LatOff_CERES05Deg   = 0.0
      REAL,    PARAMETER :: LongOff_CERES05Deg  = 0.0
!
      REAL    :: RegHgt_CERES05Deg ( NumBands_CERES05Deg) = 0.5
!
!      INTEGER :: NumBands_CERES05Deg = 360
      INTEGER :: NumReg_CERESHalfDeg ( NumBands_CERES05Deg) = 720
!
      INTEGER :: BegReg_CERES05Deg( NumBands_CERES05Deg) = 0
      INTEGER :: EndReg_CERES05Deg( NumBands_CERES05Deg) = 0
!
      NAMELIST / CERESNESTED_GRID / RegHgt_CERESNESTED, NumReg_CERESNESTED, &
                                      BegReg_CERESNESTED, EndReg_CERESNESTED
!
      NAMELIST / CERES05Deg_GRID /   RegHgt_CERES05Deg, NumReg_CERESHalfDeg, &
                                      BegReg_CERES05Deg, EndReg_CERES05Deg
				     

! *** Module-wide declarations  
      REAL,    PRIVATE :: Area_TARGET
      INTEGER, PRIVATE :: BCount, BFlag
      REAL,    PRIVATE :: DelLong_In, DelLong_TARGET
      REAL,    PRIVATE :: East_In,  East_TARGET
      INTEGER, PRIVATE :: HFlag, InColat, InLoop
      REAL,    PRIVATE :: Length
      REAL,    PRIVATE :: North_In, North_TARGET
      INTEGER, PRIVATE :: RegIdx_In, RegIdx_TARGET
      REAL,    PRIVATE :: South_In, South_TARGET
      INTEGER, PRIVATE :: TARGETColat
      REAL,    PRIVATE :: West_In,  West_TARGET, Width
! *** Grid Parameters
      REAL,    PRIVATE :: LatOff_In
      REAL,    PRIVATE :: LongOff_In
      INTEGER, PRIVATE :: NumBands_In
      INTEGER, PRIVATE :: NumReg_Global_In

      REAL,    PRIVATE :: LatOff_TARGET
      REAL,    PRIVATE :: LongOff_TARGET
      INTEGER, PRIVATE :: NumBands_TARGET
      INTEGER, PRIVATE :: NumReg_Global_TARGET



  CONTAINS

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Grid_Arrays ( Grid_Index_In,                           & ! I
                                              BegReg_Out,  EndReg_Out,  & ! O
                                              NumReg_Out, RegHgt_Out)     ! O
!******************************************************************************
! !F90
!
! !Name:
!   Grid_Arrays
!
! !Description
!    Routine ID - 12.10.7
!
!    Purpose - To get the Grid Information needed for the respected grid
!              index
!
! !Input Parameters:
!    Grid_Index_In - Index number reprecenting the grid
!
! !Output Parameters:
!    BegReg_Out - Array of Beginning Region numbers for each Latitude zone
!    EndReg_Out - Array of Ending Region numbers for each Latitude zone
!    NumReg_Out - Array of number of regions for each Latitude zone
!    RegHgt_Out - Array of region hieghts for each Latitude zone
!
! !Team-Unique Header:
!
!  Modules Used:
!    Grid_Params - Parmeters: BegReg_CERESNESTED, EndReg_CERESNESTED, 
!                             NumReg_CERESNESTED, RegHgt_CERESNESTED,
!                             BegReg_CERES05Deg,   EndReg_CERES05Deg,   
!                             NumReg_CERES05Deg,   RegHgt_CERES05Deg
!    MSG         - Sbr. WriteReport
!
!  External Files:
!    PGS_GRID_26509.f - Parameters: GRID_E_FAILED_GRIDIDX_INVALID
!
! !Revision History:
!  Version 3.0  April 11, 2023
!               Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!               Modification of module for CERESlib conversion of
!               equal angle to equal area grid
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!
!***************************************************************************
!
!
      INTEGER, INTENT (IN) :: Grid_Index_In
!
      INTEGER, INTENT (OUT):: BegReg_Out ( :), EndReg_Out ( :)
      INTEGER, INTENT (OUT):: NumReg_Out ( :)
      REAL,    INTENT (OUT):: RegHgt_Out ( :)
!
      SELECT CASE ( Grid_Index_In)
         CASE ( Index_CERES05Deg)
             BegReg_Out = BegReg_CERES05Deg
             EndReg_Out = EndReg_CERES05Deg
             NumReg_Out = NumReg_CERES05Deg
             RegHgt_Out = RegHgt_CERES05Deg
         CASE ( Index_CERESNESTED)
             BegReg_Out = BegReg_CERESNESTED
             EndReg_Out = EndReg_CERESNESTED
             NumReg_Out = NumReg_CERESNESTED
             RegHgt_Out = RegHgt_CERESNESTED
         CASE DEFAULT
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (GRID_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "Grid_Arrays()",                 & ! I
                            StrVar = " occured with Grid_Index :%i3",   & ! I
                            Ivar1 = Grid_Index_In)
!
      END SELECT
!
      END SUBROUTINE Grid_Arrays
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Grid_Scalars ( Grid_Index_In,                           & ! I
                                       NumBands_Out, NumReg_Global_Out,  & ! O
                                       LatOff_Out, LongOff_Out)            ! O
!
!******************************************************************************
! !F90
!
! !Name:
!   Grid_Scalars
!
! !Description
!    Routine ID - 12.10.8
!
!    Purpose - to get the Grid Information for the respected grid
!              index needed
!
! !Input Parameters:
!    Grid_Index_In
!
! !Output Parameters:
!    NumBands_Out
!    NumReg_Global_Out
!    LatOff_Out
!    LongOff_Out
!
! !Team-Unique Header:
!
!  Modules Used:
!    Grid_Params - Parameters : 
!                  NumBands_CERESNESTED, LatOff_CERESNESTED,
!                  LongOff_CERESNESTED, NumReg_Global_CERESNESTED,
!                  NumBands_CERES05Deg, LatOff_CERES05Deg, 
!                  LongOff_CERES05Deg, NumReg_Global_CERES05Deg
!    MSG         - Sbr. WriteReport
!
!  External Files:
!    PGS_GRID_26509.f - Parameters: GRID_E_FAILED_GRIDIDX_INVALID
!
! !Revision History:
!  Version 3.0  April 11, 2023
!               Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!               Modification of module for CERESlib conversion of
!               equal angle to equal area grid
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!****************************************************************************
!
!
      INTEGER, INTENT( IN) :: Grid_Index_In
      INTEGER, INTENT(OUT) :: NumBands_Out, NumReg_Global_Out
      REAL,    INTENT(OUT) :: LatOff_Out, LongOff_Out
!
      SELECT CASE ( Grid_Index_In)
         CASE ( Index_CERESNESTED)
             NumBands_Out = NumBands_CERESNESTED
             LatOff_Out   = LatOff_CERESNESTED
             LongOff_Out  = LongOff_CERESNESTED
             NumReg_Global_Out = NumReg_Global_CERESNESTED
         CASE ( Index_CERES05Deg)
             NumBands_Out = NumBands_CERES05Deg
             LatOff_Out   = LatOff_CERES05Deg
             LongOff_Out  = LongOff_CERES05Deg
             NumReg_Global_Out = NumReg_Global_CERES05Deg
         CASE DEFAULT
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (GRID_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "Grid_Scalars()",                & ! I
                            StrVar = " occured with Grid_Index :%i3",   & ! I
                            Ivar1 = Grid_Index_In)
!
      END SELECT
!
      END SUBROUTINE Grid_Scalars
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Calc_Grid_Regions ( NumBands, NumRegPerBand,       & ! I 
                                            BegReg_Out,  EndReg_Out)  ! I/O

!******************************************************************************
!
!
      INTEGER, INTENT (IN)   :: NumBands, NumRegPerBand 
      INTEGER, INTENT (INOUT):: BegReg_Out ( :), EndReg_Out ( :)
!

      INTEGER :: I
  
      BegReg_Out = 1
      EndReg_Out = 1
      
      DO I = 1, NumBands - 1
       EndReg_Out(I) = BegReg_Out(I) + NumRegPerBand  - 1
       BegReg_Out(I+1) = EndReg_Out(I) + 1
      ENDDO

      EndReg_Out(NumBands) = BegReg_Out(NumBands) + NumRegPerBand - 1

!
      END SUBROUTINE Calc_Grid_Regions

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Calc_Grid_Regions_Nested ( NumBands, NumRegPerBand,       & ! I 
                                            BegReg_Out,  EndReg_Out)  ! I/O

!******************************************************************************
!
!
      INTEGER, INTENT (IN)   :: NumBands, NumRegPerBand ( :)
      INTEGER, INTENT (INOUT):: BegReg_Out ( :), EndReg_Out ( :)
!

      INTEGER :: I
  
      BegReg_Out = 1
      EndReg_Out = 1
      
      DO I = 1, NumBands - 1
       EndReg_Out(I) = BegReg_Out(I) + NumRegPerBand (I) - 1
       BegReg_Out(I+1) = EndReg_Out(I) + 1
      ENDDO

      EndReg_Out(NumBands) = BegReg_Out(NumBands) + NumRegPerBand (NumBands) - 1

!
      END SUBROUTINE Calc_Grid_Regions_Nested

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      SUBROUTINE Regrid_DRV ( Grid_Index_In, Grid_Index_Out, Data_In, Dflt_In,       & ! I
                                                               DPer_Allow,           & ! I
                                                                      Data_TARGET)     ! O
!
!*************************************************************************
! !F90
!
! !Name:
!    Regrid_DRV
!
! !Description
!    Routine ID - 12.10.1
!
!    Purpose - To contain the necessary Call statements to drive the 
!              regridding process
!
! !Input Parameters:
!    Grid_Index_In  - Grid index of the input data
!    Data_In        - Input data
!    Dflt_In        - Default value used in the input data
!    DPer_Allow     - Integer from 0.0  - 1.0 representing the percentage of
!                     each Target grid region that can be default without
!                     labeling the entire region as default
!
! !Output Parameters:
!    Data_TARGET    - Data regridded to Target grid
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    None
!
!  Modules Used:
!    Grid_Setup - Subroutines: Grid_Arrays, Grid_Scalars
!
! !Revision History:
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!*************************************************************************
!
      INTEGER, INTENT ( IN) :: Grid_Index_In
      INTEGER, INTENT ( IN) :: Grid_Index_Out
      REAL,    INTENT ( IN) :: Data_In (:)
      REAL,    INTENT ( IN) :: Dflt_In
      REAL,    INTENT ( IN) :: DPer_Allow
      REAL,    INTENT (OUT) :: Data_TARGET (:)
!
      INTEGER, ALLOCATABLE :: BegReg_InG ( :)
      INTEGER, ALLOCATABLE :: EndReg_InG ( :)
      INTEGER, ALLOCATABLE :: NumReg_InG ( :)
      REAL,    ALLOCATABLE :: RegHgt_InG ( :)
!
      INTEGER, ALLOCATABLE :: BegReg_TARGET ( :)
      INTEGER, ALLOCATABLE :: EndReg_TARGET ( :)
      INTEGER, ALLOCATABLE :: NumReg_TARGET ( :)
      REAL,    ALLOCATABLE :: RegHgt_TARGET ( :)
!
! *** Setup input grid
!
      CALL Grid_Scalars ( Grid_Index_In,                                 & ! I
                                        NumBands_In, NumReg_Global_In,   & ! O
                                        LatOff_In, LongOff_In)             ! O
!
      ALLOCATE ( BegReg_InG (NumBands_In))
      ALLOCATE ( EndReg_InG (NumBands_In))
      ALLOCATE ( NumReg_InG (NumBands_In))
      ALLOCATE ( RegHgt_InG (NumBands_In))
!
      CALL Grid_Arrays ( Grid_Index_In,                                  & ! I
                         BegReg_InG, EndReg_InG, NumReg_InG, RegHgt_InG)   ! O
!
! *** Setup output grid ***
!
      CALL Grid_Scalars ( Grid_Index_Out,                                 & ! I
                                 NumBands_TARGET, NumReg_Global_TARGET,   & ! O
                                 LatOff_TARGET, LongOff_TARGET)             ! O
!
      ALLOCATE ( BegReg_TARGET (NumBands_TARGET))
      ALLOCATE ( EndReg_TARGET(NumBands_TARGET))
      ALLOCATE ( NumReg_TARGET (NumBands_TARGET))
      ALLOCATE ( RegHgt_TARGET (NumBands_TARGET))
!
      CALL Grid_Arrays ( Grid_Index_Out,                                             & ! I
                         BegReg_TARGET, EndReg_TARGET, NumReg_TARGET, RegHgt_TARGET)   ! O
!
! *** Regrid data from input grid to output grid
!
      CALL Horiz_Regrid ( Data_In, Dflt_In, DPer_Allow,                  & ! I
                          BegReg_InG, NumReg_InG, RegHgt_InG,            & ! I
                                       BegReg_TARGET, EndReg_TARGET,     & ! I
                                       NumReg_TARGET, RegHgt_TARGET,     & ! I
                                                     Data_TARGET)          ! O
!
      END SUBROUTINE Regrid_DRV
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Horiz_Regrid ( Val_In, Deflt, DPer_Allow,               & ! I 
                                       BegReg_In, NumReg_In, RegHgt_In,  & ! I
                                       BegReg_Out, EndReg_Out,     & ! I
                                       NumReg_Out, RegHgt_Out,     & ! I
                                                        Val_Out )          ! O
!
!******************************************************************************
! !F90
!
! !Name:
!   Horiz_Regrid
!
! !Description
!
!  Routine ID: 12.10.2
!
!  Purpose: To convert data on an Input grid to the TARGET grid
!
!
! !Input Parameters:
!    Deflt      - Default value for Input data
!    DPer_Allow - Integer from 0.0  - 1.0 representing the percentage of
!                 each Target grid region that can be default without
!                 labeling the entire region as default
!    BegReg_In  - Index of 1st Input region in each Input latitudinal band
!    NumReg_In  - Number of Input regions in each Input latitudinal band
!    RegHgt_In  - The hieght if each input latatude band in degree
!    Val_In     - Input region values
!    BegReg_Out - Index of 1st TARGET region in each TARGET latitudinal band
!    EndReg_Out - Index of last TARGET region in each TARGET latitudinal band
!    NumReg_Out - Number of TARGET regions in each TARGET latitudinal band
!    RegHgt_Out - The hieght if each output latitude band in degrees
!
! !Output Parameters:
!    Val_Out - Output values interpolated to the TARGET grid
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    Bcount        - Integer used to acount the number of regions needed to
!                    back track through a Latitude band due to a Longitude
!                    Offset
!    BFlag         - Integer flag used to decide if a target grid region needs
!                    to back track through a Latitude band before starting
!                    due to Longitude Offsets
!                    = 0; No back track was performed
!                    = 1; A back track procedure was performed
!    DFlag_weight  - Real number between 0-1 representing the percentage of
!                    a target grid region that contains a default value
!    HFlag         - Integer flag used to hold the gridding process from
!                    advancing because of different starting positions due
!                    the Longitude Offsets
!                    = 0; DONOT hold any latitudinal advances
!                    = 1; Hold the latitude advance when crossing from the
!                         End Region Number to the Begin Regin Number of the
!                         same latitude band
!    ISkip     - Number of TARGET latitudinal bands to skip (per pole) due
!                to lack of input data
!### LongWrap  - Integer flag indicating whether or not the last TARGET region
!###             for a given latitudinal zone needs input from the first input
!###             region for the same zone
!###              = 0; OK
!###              = 1; Account for the first input region in zone when
!###                   calculating the value of the last TARGET region in the
!###                   zone.
!
!  Modules Used:
!    NONE
!
!  Internal Subroutines:
!    LatAdvance
!    LenCalc
!    LongAdvance
!    WidthCalc
!
!  Discussion of Complex Algorithms:
!   The basic algorithm is actually quite simple--
!     1. Determine percentage of TARGET region that is in a given Input region:
!        Area of TARGET region inside Input region / Area of full TARGET region
!     2. Weight value of Input region by above percentage.
!     3. Sum all portions of the TARGET region that are distributed amoung
!        multiple input regions.
!
! !Revision History:
!  Version 2.0  March 19, 1997
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (IN)  :: BegReg_In ( NumBands_In)
      INTEGER, INTENT (IN)  :: BegReg_Out ( NumBands_TARGET)
      INTEGER, INTENT (IN)  :: EndReg_Out ( NumBands_TARGET)
      REAL,    INTENT (IN)  :: Deflt
      REAL,    INTENT (IN)  :: DPer_Allow
      INTEGER, INTENT (IN)  :: NumReg_Out ( NumBands_TARGET)
      INTEGER, INTENT (IN)  :: NumReg_In ( NumBands_In)
      REAL,    INTENT (IN)  :: RegHgt_In ( NumBands_In)
      REAL,    INTENT (IN)  :: RegHgt_Out ( NumBands_TARGET)
      REAL,    INTENT (IN)  :: Val_In ( NumReg_Global_In)
      REAL,    INTENT (OUT) :: Val_Out( NumReg_Global_TARGET)
!  
      REAL, PARAMETER :: Southpole = 180.
      INTEGER :: FirstLat_Target, tflag
      INTEGER :: ISkip, I, LongWrap
      INTEGER :: NPTemp
      REAL    :: NINTForce
      REAL    :: DFlag_weight ( NumReg_Global_TARGET)
      REAL    :: NTarget_Bound ( NumReg_Global_TARGET)
      REAL    :: STarget_Bound ( NumReg_Global_TARGET)
!
! *** Initialize values
!
      tflag=0
      DFlag_weight = 0.0
      Val_Out      = 0.0
      ISkip        = 0
      NINTForce    = .49
      InColat = 1                          ! Initializing for first run
!
!     *** This defines the Northern boundary and Southern boundary 
!         for each latitudinal zone
!
      DO i = 1,NumBands_TARGET    
         IF ( i /= 1) THEN
            STarget_Bound( i) = NTarget_Bound( i-1)
            NTarget_Bound( i) = NTarget_Bound( i-1) - RegHgt_Out( i)
         ELSE 
            STarget_Bound( i) = Southpole
            NTarget_Bound( i) = STarget_Bound( i) - RegHgt_Out( i)
         END IF
      END DO
!
      IF ( LatOff_In /= 0.) THEN
!     *** The first latitudinal band for the input data does not begin at the
!         South Pole, therefore some TARGET bands will not be fully covered
!         by the input data set and need to be set to the default value.
!
          South_In = Southpole - LatOff_In
!
          DO I = 1, NumBands_TARGET
             IF ( South_In >= STarget_Bound ( I)) THEN
                South_TARGET = STarget_Bound ( I)
                FirstLat_Target = I
                ISkip = FirstLat_Target - 1 
                DO WHILE (( South_In - RegHgt_In(InColat)) >=               &
                                              STarget_Bound ( I))
                    South_In = South_In - RegHgt_In(InColat)
                    InColat = InColat + 1
                END DO
                EXIT
             END IF
          END DO
!
!     *** Skip regions around both South and North Poles
!
          DO TARGETColat = 1, ISkip
              DFlag_weight( BegReg_Out(TARGETColat) :                    &
                            EndReg_Out(TARGETColat)) = 100.
              NPTemp = NumBands_TARGET - TARGETColat + 1
              DFlag_weight( BegReg_Out(NPTemp):EndReg_Out(NPTemp)) = 100.
          END DO
      ELSE
          South_In = Southpole
      END IF
!
      DO TARGETColat = 1+ISkip, NumBands_TARGET-ISkip
!
!     *** Assigns the N&S Boundary regions for Latitude region in
!         The Target Grid
!
          North_TARGET = NTarget_Bound( TARGETColat)
          South_TARGET = STarget_Bound( TARGETColat) 
!
!     *** Calculate area of TARGET regions in the current TARGET band
!
          DelLong_TARGET = 360. / NumReg_Out( TARGETColat)
          Area_TARGET = DelLong_TARGET * RegHgt_Out( TARGETColat)

!
!     *** Account for each Input zone that contributes to current TARGET zone
          InLoop = 0
!
          DO WHILE ( InLoop == 0)  
!
!         *** Initialize longitudinally varying variables
!
              North_In  = South_In - RegHgt_In ( InColat)
              RegIdx_In = BegReg_In ( InColat)
              DelLong_In = 360. / NumReg_In ( InColat)
              West_In   = LongOff_In
              East_In   = West_In + DelLong_In
!
              RegIdx_TARGET = BegReg_Out ( TARGETColat)
              West_TARGET   = LongOFF_TARGET
              East_TARGET   = West_TARGET + DelLong_TARGET
              BFlag  = 0
              BCount = 0
              HFlag  = 0
!
!         *** Calculate length of TARGET region contained within the Input zone
              CALL LenCalc ( RegHgt_In( InColat), RegHgt_Out( TARGETColat))
!
              DO WHILE (RegIdx_TARGET >= BegReg_Out ( TARGETColat) .AND. &
                               RegIdx_TARGET <= EndReg_Out ( TARGETColat))
!             *** Add portion of TARGET region contained within the Input zone
!                 to total value for the TARGET region
!
                  IF ( LongOff_TARGET < LongOff_In .AND.                    &
                      West_TARGET <= West_In .AND. BFlag == 0 ) THEN
!                 *** Allow for western part of first region
!
                       DO WHILE ( West_In > LongOff_TARGET)
!                      *** Make sure all regions are accounted for backtrack
                           BCount = BCount + 1
                           West_In = West_In - DelLong_In
                           East_In = West_In + DelLong_In
                           RegIdx_In =  BegReg_In( InColat)                 & 
                                      + NumReg_In( InColat) - BCount
                       END DO
                       BFlag = 1
                  END IF
!
                  IF ( BCount == 1) HFlag = 1
!
!             *** Calculate width of TARGET region in the Input zone
                  CALL WidthCalc ( DelLong_TARGET)
!

                  IF ( Val_In ( RegIdx_In)  /=  Deflt) THEN
!                 *** The value of the Input region is good. Use it.
                      Val_Out ( RegIdx_TARGET) = Val_Out ( RegIdx_TARGET) +  &
                                       ( ( (Length * Width) /Area_TARGET )   &
                                                 * Val_In ( RegIdx_In)  )
                  ELSE
!                 *** Add up the area that is default
                      DFlag_weight ( RegIdx_TARGET) =                        &
                                           DFlag_weight ( RegIdx_TARGET) +   &
                                           (Length * Width) / Area_TARGET 
!
                  END IF
!
!             *** Advance to next regions within zone as necessary
                  CALL LongAdvance ( BegReg_In (InColat), LongOff_In)      ! I
!
                  IF ( RegIdx_TARGET == EndReg_Out( TARGETColat)  .AND.  &
                      LongOFF_TARGET > 0.0  .AND. East_In <= 360.) THEN
                      RegIdx_TARGET = BegReg_Out ( TARGETColat)
                      East_TARGET = 360.
                      LongWrap = 1
                  ELSEIF ( RegIdx_TARGET > EndReg_Out( TARGETColat) .AND. &
                      LongOFF_TARGET < 0.0  .AND. East_In <= 360.)THEN
                      LongWrap = 0
                  ELSEIF ( RegIdx_TARGET == EndReg_Out( TARGETColat) .AND. &
                      LongOFF_TARGET <= 0.0  .AND.          &
                      RegIdx_In > (BegReg_In(InColat)+ NumReg_In(InColat)-1))THEN
                      RegIdx_In = RegIdx_In - NumReg_In(InColat)
                      LongWrap = 0
                  ELSE 
                      IF ( LongWrap == 1 .AND. East_In > 360.) EXIT
                  END IF
!
              END DO
!
!         *** Advance to the next latitudinal zones as necessary
              CALL LatAdvance ( TARGETColat, RegHgt_In,                & ! I
                                             RegHgt_Out,               & ! I
                                                      InColat, InLoop)   ! I/O
!         *** Reset wrap-around flag
              LongWrap = 0
!
          END DO
!
     END DO
!
     WHERE ( DFlag_weight <= DPer_Allow)
         Val_Out = Val_Out / ( 1. - DFlag_weight)
     ELSEWHERE
         Val_Out = Default_MOA_REAL
     END WHERE
!
!
     END SUBROUTINE Horiz_Regrid
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
     SUBROUTINE LenCalc ( RegHeight_In, RegHeight_TARGET)
!
!**************************************************************************
! !F90
!
! !Name:
!    LenCalc
!
! !Description
!    Routine ID - 12.10.3
!
!    Purpose - To calculate the length of the portion of the CERES region that
!              is contained within the Input region
!
!
! !Input Parameters:
!    RegSize_In - Latitudinal height of Input region
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    Length
!    North_TARGET
!    North_In
!    South_TARGET
!    South_In
!
!  Modules Used:
!
! !Revision History:
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!******************************************************************************
!
!
     REAL, INTENT (IN) :: RegHeight_In, RegHeight_TARGET
!
     IF ( South_TARGET < South_In) THEN
!    *** Account for contributions from the southern portion
! 
         IF ( North_In <= North_TARGET) THEN
!        *** The length of the TARGET region is fully contained
!            within the length of the Input region
!
             Length = RegHeight_TARGET
         ELSE
!        *** The TARGET region latitudinally straddles two Input regions
!
             Length = ABS( South_TARGET - North_In)
         END IF
     ELSE
!    *** Account for contributions from the northern portion
!
         IF ( North_TARGET <= North_In) THEN
!        *** The Input zone is fully contained within the TARGET zone
!
             Length = RegHeight_In
         ELSE
!        *** The TARGET region latitudinally straddles two Input regions
! 
             Length = South_In - North_TARGET
         END IF
     END IF
!
     END SUBROUTINE LenCalc
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
     SUBROUTINE WidthCalc ( DelLong_TARGET)                               ! I
!
!******************************************************************************
! !F90
!
! !Name:
!   WidthCalc
! 
! !Description
!    Routine ID - 12.10.4
!
!    Purpose - To calculate the width of the portion of the CERES region that
!              is contained within the Input region
!
!
! !Input Parameters:
!    DelLong_TARGET - Longitudinal width of the full CERES region
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    East_TARGET
!    East_In
!    West_TARGET
!    West_In
!    Width
!
! !Revision History:
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!******************************************************************************
!
!
     REAL :: DelLong_TARGET
!
!
     IF ( East_TARGET >= East_In) THEN
!    *** Account for contributions from the western portion of the
!        TARGET region.
!              
         IF ( (East_In - West_TARGET) >= DelLong_In) THEN
!        *** The Input region is fully contained, longitudinally,
!            within the TARGET region
!
            Width = DelLong_In
         ELSE
!        *** Only a portion of the Input region is contained within
!            the TARGET region
!
             Width = East_In - West_TARGET
         END IF
     ELSE
!    *** Account for contributions from the eastern portion of the
!        TARGET region.
!
         IF ( (East_TARGET - West_In) >= DelLong_TARGET) THEN
!        *** The TARGET region is fully contained, longitudinally,
!            within the Input region
!
             Width = DelLong_TARGET
         ELSE
!        *** Only a portion of the TARGET region is contained
!            within the Input region
!
             Width = East_TARGET - West_In
         END IF
    END IF
!
    END SUBROUTINE WidthCalc
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE LongAdvance ( Beg_In, Long_In)                        ! I
!
!******************************************************************************
! !F90
!
! !Name:
!   LongAdvance
! 
! !Description
!    Routine ID - 12.10.5
!
!    Purpose: To advance to the next CERES and/or Input regions within the
!             current bands as necessary
!
! !Input Parameters:
!    Beg_In  - First region number in a given input data zone
!    Long_In - Amount of degrees to back up past the Greenwich Meridian, if needed  
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    East_TARGET
!    East_In
!    RegIdx_TARGET
!    RegIdx_In
!    West_TARGET
!    West_In
!
!  Modules Used:
!
! !Revision History:
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!******************************************************************************
!

      INTEGER, INTENT (IN) :: Beg_In
      REAL,    INTENT (IN) :: Long_In
!
      IF ( East_TARGET > East_In  .and. HFlag /= 1) THEN
!     *** Advance to the next Input region in the current band.
!
          RegIdx_In = RegIdx_In + 1
          West_In = East_In
          East_In = West_In + DelLong_In
          BCount = BCount - 1
      ELSE IF ( East_TARGET > East_In .AND. HFlag == 1) THEN
!     *** Advance to the first IN region in the current band.
!
          RegIdx_In = Beg_In
          West_In = Long_In
          East_In = West_In + DelLong_In
          HFlag = 0
          BCount = BCount - 1
      ELSE IF ( East_TARGET < East_In .and. HFlag /= 1) THEN 
!     *** Advance to the next TARGET region in the current band.
!
          RegIdx_TARGET = RegIdx_TARGET + 1
          West_TARGET = East_TARGET
          East_TARGET = West_TARGET + DelLong_TARGET
      ELSE
!     *** Advance to both the next TARGET and Input regions
!
          RegIdx_In = RegIdx_In + 1
          West_In = East_In
          East_In = West_In + DelLong_In
          RegIdx_TARGET = RegIdx_TARGET + 1
          West_TARGET = East_TARGET
          East_TARGET = West_TARGET + DelLong_TARGET
          BCount = BCount - 1
      END IF
!
      IF (East_TARGET > 360.) THEN
!     *** Overflow caused by summation across the band
!
          East_TARGET = 360.
      END IF
!
      END SUBROUTINE LongAdvance
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE LatAdvance ( ITarget, Input_Hgt, Target_Hgt,          & ! I
                                                   InColat, InLoop)      ! I/O
!
!******************************************************************************
! !F90
!
! !Name:
!    LatAdvance
! 
! !Description
!    Routine ID - 12.10.6
!
!    Purpose - To advance to the next TARGET and/or Input latitudinal bands as
!              necessary
!
!
! !Input Parameters:
!    ITarget    - Target grids colatitudinal band index
!    Input_Hgt  - Hieght of input grid latitudinal band
!    Target_Hgt - Hieght of target grid latitudinal band
!
!  Input/Output:
!    InLoop  - Integer flag
!    InColat - Input colatitudinal band index
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    InColat
!    North_TARGET
!    North_In
!    South_TARGET
!    South_In
!
!  Modules Used:
!
! !Revision History:
!  Version 2.0  March 19, 1997
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0  February 29, 1996
!               Lisa H. Coleman, SAIC, l.h.coleman@larc.nasa.gov
!               Initial version of code
!
! !END
!
!******************************************************************************
!
!
      INTEGER, INTENT (INOUT) :: InColat, InLoop
      INTEGER, INTENT ( IN)   :: ITarget
      REAL,    INTENT ( IN)   :: Input_Hgt(NumBands_In)
      REAL,    INTENT ( IN)   :: Target_Hgt(NumBands_TARGET)
!
      IF ( North_TARGET > North_In) THEN
!     *** Go to the next TARGET colatitudinal zone
!
          IF ( ITarget < NumBands_TARGET) THEN
              South_TARGET = North_TARGET
              North_TARGET = South_TARGET - Target_Hgt(ITarget+1)
          END IF
          InLoop = 1 
      ELSE IF ( North_TARGET < North_In) THEN
!     *** Go to the next Input colatitudinal zone
!
          InColat = InColat + 1
          South_In = North_In
          North_In = South_In - Input_Hgt(InColat)
      ELSE
!     *** Go to the next TARGET zone and the next Input zone
!
          IF ( ITarget < NumBands_TARGET) THEN
!         *** Check if Target is in last latitude band before advancing
              South_TARGET = North_TARGET
              North_TARGET = South_TARGET - Target_Hgt(ITarget+1)
              InColat = InColat + 1
          END IF
          South_In = North_In
          North_In = South_In - Input_Hgt(InColat)
          InLoop = 1
      END IF
!
!
      END SUBROUTINE LatAdvance
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE Regrid_Mod
