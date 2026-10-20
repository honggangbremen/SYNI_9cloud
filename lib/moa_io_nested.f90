MODULE MOA_IO_NESTED
!
!
!******************************************************************************
!
! !F90
!
! !Name:
!  MOA_IO_NESTED 
!
! !Description:
!
!  Purpose - Interface routine library for nested netcdf MOA access
!
! !Input Parameters:
!   NONE
!
! !Output Parameters:
!   NONE
!
! !Team Unique Header:
!
!  Internal Subroutines:
!    Get_Dataset_Index          - retrieve a dataset index for reading single dataset from netcdf MOA
!    Get_NCDFHourly_Data        - retrieves hourly data from the available Hourly fields in netcdf MOA
!    LatLong_Reg                - converts real colat and colong values into region number for specific grids
!    MOA_Check                  - checks output values of MOA structure for acceptable ranges 
!    MOA_Data_Get_Reg           - assigns values to a MOA structure from the global arrays for specific
!                                 hour and coordinates
!    MOA_FileInterp_Check       - checks if a requested interpolation hour is between the two open files
!    MOA_Header_Get             - assigns header values from a specific open file
!    MOA_Header_print           - retrieves MOA header and prints its values 
!    MOA_Interpolate_Reg        - interpolates the MOA data fields between values from two open files 
!    MOA_NCDF_Allocate          - allocates memory structure of open MOA file
!    MOA_NCDF_Close             - closes an open MOA file
!    MOA_NCDF_Close_Wrapper     - closes up to two open MOA files
!    MOA_NCDF_Deallocate        - deallocates memory structure of open MOA file
!    MOA_NCDF_GetDataset        - read a single dataset from netcdf MOA file
!    MOA_NCDFOpen_Wrapper       - determine whether to open one or two MOA files, and open them  
!    MOA_NCDF_Open              - open a MOA file and read its contents into the global file arrays 
!    MOA_NCDFRead_Region_Wrap   - determine whether to read one or two MOA files, read them for a specific region,
!                                 and temporally interpolate that data if necessary       
!    MOA_NCDFRead_LatLon_Wrap   - determine whether to read one or two MOA files, read them for a set of coordinates,
!                                 and temporally interpolate that data if necessary                                     
!    MOA_print_Header           - prints MOA header values                        
!    MOA_NCDFRead_LatLon        - read a MOA structure for a specific set of coordinates
!    MOA_NCDFRead_LatLon_Interp - interpolate a MOA structure between two files for a specific set of coordinates
!    MOA_NCDFRead_Hourly_LatLon - read the hourly data for a specific set of coordinates 
!    MOA_NCDFRead_Hourly_Region - read the hourly data for a specific region number
!    MOA_NCDFRead_Region        - read a MOA structure for a specific region
!    MOA_NCDFRead_Reg_Interp    - interpolate a MOA structure between two files for a specific region
!    MOA_NCDF_FileInterp_Hour   - calculate the interpolation hour between two MOA files
!    MOA_Profile_Check          - check the MOA profiles for default values above the surface and interpolate if needed
!    MOA_Rval_Interp            - interpolate MOA values
!    Region_LatLon              - convert a region number to real lat long values
!
!  Modules Used:
!    moa_io_def   
!    moa_io_read    
!    NETCDF    
!    PCF  
!
!  External Files:
!    PGS_MOAIO_26500.f   
!
! !Revision History:
!  Version 1.0   March 8, 2023  -  SCCR# 2167
!                Tom Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!******************************************************************************
!

      USE moa_io_def
      USE PCF, ONLY : GetFileName
      USE netcdf
      USE Regrid_Mod
!
      IMPLICIT NONE
!      
      INTERFACE MOA_NCDFRead_Wrapper
         MODULE PROCEDURE                   &
              MOA_NCDFRead_Region_Wrap,      &
              MOA_NCDFRead_LatLon_Wrap
      END INTERFACE MOA_NCDFRead_Wrapper

      INTEGER, PARAMETER :: NumDS_NCDF = 29

! *** Dataset names used to index netcdf MOA datasets
      
      CHARACTER (LEN=50) :: moa_dataset_names(NumDS_NCDF)
      
      DATA moa_dataset_names  / &
      'Longitude', 'Latitude', &
      'Region_number', 'Surface_pressure', 'Surface_geopotential_height', &
      'Surface_skin_temperature', 'Air_mass_index', &
      'Temperature_profile', 'Humidity_profile', &
      'Surface_U_wind', 'Surface_V_wind', 'U_Wind_profile', 'V_Wind_profile', &
      'Troposphere_pressure', 'Ozone_profile', &
      'Column_integrated_ozone', &
      'Aerosol_optical_depth', &
      'Column_averaged_relative_humidity', 'Precipitable_water', &
      'Total_cloud_fraction', &
      'Upper_troposphere_relative_humidity', 'Estimated_inversion_strength', &
      'Potential_temperature', 'Hourly_Skin_temperature', 'Hourly_Two_meter_temperature', &
      'Hourly_Ten_meter_temperature', 'Hourly_Two_meter_humidity', &
      'Hourly_Ten_meter_humidity', 'Daily_average_two_meter_temperature' /
 
      TYPE MOA_NCDF_TYPE
         SEQUENCE
         CHARACTER (LEN=300)                :: file
         INTEGER	                    :: ncid
         INTEGER, DIMENSION(NumDS_NCDF)     :: dsid
         LOGICAL                            :: all_read
         CHARACTER (LEN=27)                 :: Header_DataDate
         CHARACTER (LEN=19)                 :: Header_ProcDate
         INTEGER (KIND=INT4)                :: Header_GridIdxIn
         INTEGER (KIND=INT4)                :: Header_GridIdxOut
         INTEGER (KIND=INT4)                :: Header_NumReg
         REAL (KIND=REAL4),   ALLOCATABLE   :: Header_PresLevel                    (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Header_WindLevel                    (:)
         INTEGER (KIND=INT4), ALLOCATABLE   :: Region_number                       (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Surface_pressure                    (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Surface_geopotential_height         (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Surface_skin_temperature            (:)
         INTEGER (KIND=INT4), ALLOCATABLE   :: Air_mass_index                      (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Temperature_profile                 (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Humidity_profile                    (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Surface_U_wind                      (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Surface_V_wind                      (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: U_Wind_profile                      (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: V_Wind_profile                      (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Troposphere_pressure                (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Ozone_profile                       (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Column_integrated_ozone             (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Aerosol_optical_depth               (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Column_averaged_relative_humidity   (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Precipitable_water                  (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Total_cloud_fraction                (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Upper_troposphere_relative_humidity (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Estimated_inversion_strength        (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Potential_temperature               (:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Hourly_Skin_temperature             (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Hourly_Two_meter_temperature        (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Hourly_Ten_meter_temperature        (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Hourly_Two_meter_humidity           (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Hourly_Ten_meter_humidity           (:,:)
         REAL (KIND=REAL4),   ALLOCATABLE   :: Daily_average_two_meter_temperature (:)
      END TYPE MOA_NCDF_TYPE
            
      LOGICAL, DIMENSION(MaxMOA_NCDF) :: MOA_NCDFinUse(1:MaxMOA_NCDF) = .FALSE.
      
      TYPE (MOA_NCDF_TYPE), DIMENSION(MaxMOA_NCDF)  :: MOA_NCDFDataRaw
     
           
      CONTAINS
 
 
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_Region_Wrap ( UnitNums, Hour, IReg,          &  ! I
                                                             MOA, Stat)       ! O
!
!**************************************************************************
! !F90
!
! !Name:
!   MOANCDFRead_Region_Wrap
!
! !Desription:
!
!    Purpose - To determine whether to read one or two MOA files, read them,
!              and temporally interpolate their data if necessary
!
!
! !Input Parameters:
!    Hour     - Hour of data to be processed
!    IReg     - CERES nested region number
!    UnitNums - Unit numbers assigned by Toolkit to MOA file(s)
!
! !Output Parameters:
!    MOA  - MOA data coinciding with requested region and hour
!    Stat - (Optional) I/O status returned from opening process and
!           returned to calling routine. If omitted and an I/O error
!           occurs, processing terminates through the Toolkit error
!            message utility
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MOA_IO       - (CERESlib) Structure: MOA_TYPE
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!   PGS_MOAIO_26500 - Parameters: MOAIO_E_FAILED_MOA_READ,
!                                 MOAIO_E_FAILED_UNIT_NUMS_DFLT,
!                                 MOAIO_W_FAILED_UNIT_NUMS_DFLT
!
! !Revision History:
!  Version 1.0   April 18, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER,          INTENT ( IN)           :: UnitNums ( 2)
      INTEGER,          INTENT ( IN)           :: Hour
      INTEGER,          INTENT ( IN)           :: IReg
      TYPE (MOA_TYPE),  INTENT (OUT)           :: MOA
      INTEGER,          INTENT (OUT), OPTIONAL :: Stat
!
      TYPE (MOA_HEADER_TYPE) :: MOA_Header
      INTEGER                :: Ios
!
!
! *** Initialize Status Flag
!
      IF (Present (Stat)) Stat = OK

      !
      IF ( UnitNums  (1) == -9 .AND. UnitNums ( 2) == -9) THEN
!     *** No valid unit numbers.
!
          IF (Present (Stat)) THEN
!         *** Return control to calling routine
!
              Stat = BAD
              CALL WriteReport (MOAIO_W_FAILED_UNIT_NUMS_DFLT,          & ! I
                                FuncName = "MOARead_Region_Wrap()",     & ! I
                                StrVar = "occured at region number: ",  & ! I
                                Ivar1 = Ireg)                             ! I
!
          ELSE
!         *** Terminate processing
!
              CALL WriteReport (MOAIO_E_FAILED_UNIT_NUMS_DFLT,          & ! I
                                FuncName = "MOARead_Region_Wrap()",     & ! I
                                StrVar = "occured at region number: ",  & ! I
                                Ivar1 = Ireg)                             ! I
          END IF
!
      ELSE
          IF ( UnitNums ( 1) /= -9 .AND. UnitNums ( 2) == -9) THEN
!         *** Read only from one MOA file. No temporal interpolation necessary.
!
	      
             CALL MOA_Header_Get ( UnitNums(1), MOA_Header) 
       

             CALL MOA_NCDFRead_Region ( UnitNums(1), MOA_Header % GridIdx_Output, & ! I
                                                                     MOA, Ios,    & ! O
                                                                             IReg)  ! O
!
          ELSE IF ( UnitNums ( 1) /= -9 .AND. UnitNums ( 2) /= -9) THEN
!         *** Temporal interpolation necessary between data from two MOA files.
!
             CALL MOA_Header_Get ( UnitNums(1), MOA_Header)    
             
             CALL MOA_NCDFRead_Reg_Interp (UnitNums ( 1), UnitNums ( 2),          & ! I
                                         MOA_Header % GridIdx_Output, Hour,       & ! I
                                                                      MOA, Ios,   & ! O
                                                                           IReg)    ! I
!
          ELSE IF ( UnitNums ( 1) == -9 .AND. UnitNums ( 2) /= -9) THEN
!         *** Read only from one MOA file. No temporal interpolation necessary.
!
              CALL MOA_Header_Get ( UnitNums(2), MOA_Header)    

              CALL MOA_NCDFRead_Region (UnitNums( 2), MOA_Header % GridIdx_Output, & ! I
                                                                    MOA, Ios,      & ! O
                                                                          IReg)      ! O
          END IF
!
          IF ( Ios /= OK) THEN
              IF (Present (Stat)) THEN
!             *** Return control to the calling routine
!
                  Stat = BAD
!
              ELSE
!             *** Terminate processing
!
                  CALL WriteReport (MOAIO_E_FAILED_MOA_READ,               & ! I
                                    FuncName = "MOARead_Region_Wrap()",    & ! I
                                    StrVar = "occured at region number: ", & ! I
                                    Ivar1 = Ireg)                            ! I
              END IF
!
          END IF
!
      END IF
!
!
      END SUBROUTINE MOA_NCDFRead_Region_Wrap
!
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_LatLon_Wrap ( UnitNums, Hour, Lat, Long,     &  ! I
                                                             MOA, Stat)       ! O

!
!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDFRead_LatLon_Wrap
!
! !Desription:
!
!    Purpose - To determine whether to read one or two MOA files, read them,
!              and temporally interpolate their data if necessary
!
!
! !Input Parameters:
!    Hour     - Hour of data to be processed
!    IReg     - CERES nested region number
!    UnitNums - Unit numbers assigned by Toolkit to MOA file(s)
!
! !Output Parameters:
!    MOA  - MOA data coinciding with requested region and hour
!    Stat - (Optional) I/O status returned from opening process and
!           returned to calling routine. If omitted and an I/O error
!           occurs, processing terminates through the Toolkit error
!            message utility
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MOA_IO       - (CERESlib) Structure: MOA_TYPE
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!   PGS_MOAIO_26500 - Parameters: MOAIO_E_FAILED_MOA_READ,
!                                 MOAIO_E_FAILED_UNIT_NUMS_DFLT,
!                                 MOAIO_W_FAILED_UNIT_NUMS_DFLT
!
! !Revision History:
!  Version 1.0   April 18, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER,          INTENT ( IN)           :: UnitNums ( 2)
      INTEGER,          INTENT ( IN)           :: Hour
      REAL,             INTENT ( IN)           :: Lat
      REAL,             INTENT ( IN)           :: Long
      TYPE (MOA_TYPE),  INTENT (OUT)           :: MOA
      INTEGER,          INTENT (OUT), OPTIONAL :: Stat
!
      TYPE (MOA_HEADER_TYPE) :: MOA_Header
      INTEGER                :: Ios
!
!
! *** Initialize Status Flag
!
      IF (Present (Stat)) Stat = OK
!
      IF ( UnitNums  (1) == -9 .AND. UnitNums ( 2) == -9) THEN
!     *** No valid unit numbers.
!
          IF (Present (Stat)) THEN
!         *** Return control to calling routine
!
              Stat = BAD
              CALL WriteReport (MOAIO_W_FAILED_UNIT_NUMS_DFLT,          & ! I
                                FuncName = "MOARead_LatLon_Wrap()")       ! I
!
          ELSE
!         *** Terminate processing
!
              CALL WriteReport (MOAIO_E_FAILED_UNIT_NUMS_DFLT,          & ! I
                                FuncName = "MOARead_LatLon_Wrap()")       ! I
          END IF
!
      ELSE
          IF ( UnitNums ( 1) /= -9 .AND. UnitNums ( 2) == -9) THEN
!         *** Read only from one MOA file. No temporal interpolation necessary.
!
             CALL MOA_Header_Get ( UnitNums(1), MOA_Header)    

             CALL MOA_NCDFRead_LatLon ( UnitNums(1), MOA_Header % GridIdx_Output,  & ! I
                                                                       MOA, Ios,   & ! O
                                                                        Lat,Long)    ! O
!
          ELSE IF ( UnitNums ( 1) /= -9 .AND. UnitNums ( 2) /= -9) THEN
!         *** Temporal interpolation necessary between data from two MOA files.
!
             CALL MOA_Header_Get ( UnitNums(1), MOA_Header)    

             CALL MOA_NCDFRead_LatLon_Interp (UnitNums ( 1), UnitNums ( 2),       & ! I
                                         MOA_Header % GridIdx_Output, Hour,       & ! I
                                                                      MOA, Ios,   & ! O
                                                                     Lat,Long)      ! I
!
          ELSE IF ( UnitNums ( 1) == -9 .AND. UnitNums ( 2) /= -9) THEN
!         *** Read only from one MOA file. No temporal interpolation necessary.
!
             CALL MOA_Header_Get ( UnitNums(2), MOA_Header)   

             CALL MOA_NCDFRead_LatLon (UnitNums( 2), MOA_Header % GridIdx_Output, & ! I
                                                                    MOA, Ios,     & ! O
                                                                   Lat,Long)        ! O
          END IF
!
          IF ( Ios /= OK) THEN
              IF (Present (Stat)) THEN
!             *** Return control to the calling routine
!
                  Stat = BAD
!
              ELSE
!             *** Terminate processing
!
                  CALL WriteReport (MOAIO_E_FAILED_MOA_READ,               & ! I
                                    FuncName = "MOARead_LatLon_Wrap()")      ! I
              END IF
!
          END IF
!
      END IF
!
!
      END SUBROUTINE MOA_NCDFRead_LatLon_Wrap
!
!
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFOpen_Wrapper ( MOA_ID, Hour,                       &  ! I
                                              UnitNum, MOA_Header, Stat,    &  ! O
                                                                         Vers) ! I
!
!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDFOpen_Wrapper
!
! !Desription:
!
!    Purpose - To determine whether to open one or two MOA files, open them,
!              and retrieve their header(s)
!
!
! !Input Parameters:
!    Hour   - Hour of data to be processed
!    MOA_ID - PCF logical identifier for MOA file(s)
!    Vers   - (Optional) Starting PCF version number of MOA files. If omitted,
!              value is set to 1.
!
! !Output Parameters:
!    MOA_Header - Header record from MOA file(s)
!    Stat       - (Optional) I/O status returned from opening process and
!                 returned to calling routine. If omitted and an I/O error
!                 occurs, processing terminates through the Toolkit error
!                 message utility
!    UnitNum    - Unit numbers assigned by Toolkit to MOA file(s)
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MOA_IO       - (CERESlib) Structure: MOA_HEADER_TYPE
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!   PGS_MOAIO_26500 - Parameters: MOAIO_E_FAILED_MOA_INTERPOLATE,
!                                 MOAIO_E_FAILED_MOA_OPEN
!
! !Revision History:
!  Version 1.0   April 18, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER,                INTENT (IN)              :: Hour
      INTEGER,                INTENT (IN)              :: MOA_ID
      TYPE (MOA_HEADER_TYPE), INTENT (OUT)             :: MOA_Header
      INTEGER,                INTENT (OUT),   OPTIONAL :: Stat
      INTEGER,                INTENT (OUT)             :: UnitNum ( 2)
      INTEGER,                INTENT (IN),    OPTIONAL :: Vers
!
      INTEGER :: Vers_Num, Ios
      INTEGER :: Hour1, Hour2
      TYPE (MOA_HEADER_TYPE) :: MOA2_Header
!
!
! *** Initialize open success flag to indicate successful open
!
      IF (Present (Stat)) Stat = OK
      UnitNum = -9
!
! *** Use Version Number supplied or use 1 as a default
!
      IF ( PRESENT ( Vers)) THEN
          Vers_Num = Vers
      ELSE
          Vers_Num = 1
      END IF
!
! *** Try to open first hour listed (non-interpolation case)
!
      CALL MOA_NCDF_Open ( MOA_ID,                           &
                                  UnitNum( 1), Ios, 	     &  ! O
                                       MOA_Header, Vers_Num)    ! I
      IF ( Ios == OK) THEN
	 RETURN
      END IF
!
!
! *** Try to open second hour listed (interpolation case)
!
      CALL MOA_NCDF_Open ( MOA_ID,                             &
                                  UnitNum( 1), Ios, 	       &  ! O
                                       MOA_Header, Vers_Num+1)    ! I
!
!
      IF ( MOA_Header % GridIdx_Output < Index_CERES05Deg .OR. Ios /= OK) THEN
!     *** Error opening MOA file
          UnitNum ( 1) = -9
!
          IF (PRESENT (Stat)) THEN
!         *** Return I/O status to calling routine
!
               Stat = BAD
               RETURN
!
          ELSE
!         *** Terminate processing
!
               CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN,               & ! I
                                 FuncName = "MOA_NCDFOpen_Wrapper()")     ! I
          END IF
      END IF
!
      READ ( MOA_Header % DataDate ( 12:13), '(i2)') Hour1
!
!
! *** Try to open third hour listed (interpolation case)
!
      CALL MOA_NCDF_Open ( MOA_ID,          	               &  ! I
                                  UnitNum( 2), Ios, 	       &  ! O
                                      MOA2_Header, Vers_Num+2)    ! I

!
      IF ( MOA2_Header % GridIdx_Output < Index_CERES05Deg .OR. Ios /= OK ) THEN
!     *** Error opening MOA file
          UnitNum ( 1) = -9
!
          IF (PRESENT (Stat)) THEN
!         *** Return I/O status to calling routine
!
               Stat = BAD
               RETURN
!
          ELSE
!         *** Terminate processing
!
               CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN,               & ! I
                                 FuncName = "MOA_NCDFOpen_Wrapper()")     ! I
          END IF
      END IF
!
      READ ( MOA2_Header % DataDate ( 12:13), '(i2)') Hour2
!
!
! *** Check file info and determine interpolation hour ( 1-3)
!
      CALL MOA_FileInterp_Check ( MOA_Header, MOA2_Header, Hour,    & ! I
                                                              Ios)
      IF ( Ios /= OK ) THEN
!
               CALL WriteReport (MOAIO_E_FAILED_MOA_INTERPOLATE,        & ! I
                                 FuncName = "MOA_Open_Wrapper()")         ! I
      END IF
!
!
      END SUBROUTINE MOA_NCDFOpen_Wrapper

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
      SUBROUTINE MOA_NCDF_Open ( MOA_ID,                            &  ! I
                                      UnitNum, Stat,                &  ! O
                                                  MOA_Header, Vers)    ! I
!
!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDF_Open
!
! !Desription:
!
!    Purpose - To open one MOA file, read its contents,
!              and store that data in memory using 
!              MOA_NCDFDataRaw structure
!
!
! !Input Parameters:
!    MOA_ID     - PCF logical identifier for MOA file(s)
!    MOA_Header - Header record from MOA file(s)
!    Vers       - (Optional) Starting PCF version number of MOA files. If omitted,
!                 value is set to 1.
!
! !Output Parameters:
!    Stat       - (Optional) I/O status returned from opening process and
!                 returned to calling routine. If omitted and an I/O error
!                 occurs, processing terminates through the Toolkit error
!                 message utility
!    UnitNum    - Unit numbers assigned by Toolkit to MOA file(s)
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MOA_IO       - (CERESlib) Structure: MOA_HEADER_TYPE
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!   PGS_MOAIO_26500 - Parameters: MOAIO_E_FAILED_MOA_OPEN
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER,                INTENT (IN)              :: MOA_ID
      INTEGER,                INTENT (OUT)             :: UnitNum

      INTEGER,                INTENT (OUT)             :: Stat
      TYPE (MOA_HEADER_TYPE), INTENT (INOUT), OPTIONAL :: MOA_Header
      INTEGER,                INTENT (IN),    OPTIONAL :: Vers
!
      INTEGER  :: Ios, i
      INTEGER  :: Vers_Num
!
      CHARACTER (LEN=300) :: MOA_NCDF_file
      LOGICAL :: file_exist
      integer :: ncid, dsid, nfok, errflag
      integer :: start(1), count(1)
      start(1) = 1
      
      Stat = OK
      nfok = NF90_NOERR
      errflag = 0
!
! *** Use Version Number supplied or use 1 as a default
!
      IF ( PRESENT ( Vers)) THEN
          Vers_Num = Vers
      ELSE
          Vers_Num = 1
      END IF
!
! *** Get netcdf filename
!
      CALL GetFileName (MOA_ID,			         & ! I
 	    		MOA_NCDF_file, Ios,	         & ! O
 	    		Version = Vers_Num, incPath = 1)   ! I
!

      INQUIRE(FILE=MOA_NCDF_file, EXIST=file_exist)

!
!     Open netcdf MOA file and read all data that will be needed from file
!
      IF (file_exist) THEN
         
	 UnitNum = -9
         DO i = 1,MaxMOA_NCDF
            if ( MOA_NCDFinUse(i) .and. MOA_NCDFDataRaw(i)%file .eq. MOA_NCDF_file ) then
               ! MOA file already open
               UnitNum = i
               IF ( PRESENT ( MOA_Header) ) THEN
                  CALL MOA_Header_Get ( UnitNum, MOA_Header )
               ENDIF
               RETURN
            ENDIF
         ENDDO
         
	 DO i = 1,MaxMOA_NCDF
            IF ( .NOT. MOA_NCDFinUse(i) ) THEN
               UnitNum = i
               EXIT
	    ENDIF
         ENDDO
         
	 IF ( UnitNum > 0 ) THEN
            Ios = nf90_open(MOA_NCDF_file, NF90_NOCLOBBER, ncid)
            IF (Ios == nfok) THEN
               MOA_NCDFinUse(UnitNum) = .TRUE.
               Ios = OK
            ELSE
               Ios = BAD
            ENDIF
         ELSE
            Ios = BAD
         ENDIF
 
      ELSE
         Ios = BAD
      ENDIF
!
      IF ( Ios /= OK) THEN
!     *** File OPEN error
!
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_MOA_OPEN,      & ! I
               FuncName = "MOA_NCDF_Open()", StrVar = MOA_NCDF_file) ! I
          RETURN
!
      END IF
!
! *** Allocate MOA memory structure
!
      CALL MOA_NCDF_Allocate ( UnitNum )
      
! *** Read Header
      
      Ios = nf90_inq_varid(ncid, "Header_DataDate", dsid)
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid,MOA_NCDFDataRaw(UnitNum)%Header_DataDate )
      IF ( Ios==nfok ) Ios = nf90_inq_varid(ncid, "Header_ProcDate", dsid) 
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid, MOA_NCDFDataRaw(UnitNum)%Header_ProcDate)
      IF ( Ios==nfok ) Ios = nf90_inq_varid(ncid, "Header_GridIdxIn", dsid) 
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid, MOA_NCDFDataRaw(UnitNum)%Header_GridIdxIn )
      IF ( Ios==nfok ) Ios = nf90_inq_varid(ncid, "Header_GridIdxOut", dsid) 
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid, MOA_NCDFDataRaw(UnitNum)%Header_GridIdxOut )
      IF ( Ios==nfok ) Ios = nf90_inq_varid(ncid, "Header_NumReg", dsid) 
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid,MOA_NCDFDataRaw(UnitNum)%Header_NumReg )
      
      IF ( Ios==nfok ) Ios = nf90_inq_varid(ncid, "Header_PresLevel", dsid)       
      count(1) = NL
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid, MOA_NCDFDataRaw(UnitNum)%Header_PresLevel, &
           start, count)
      
      IF ( Ios==nfok ) Ios = nf90_inq_varid(ncid, "Header_WindLevel", dsid)
      count(1) = NW
      IF ( Ios==nfok ) Ios = nf90_get_var(ncid, dsid, MOA_NCDFDataRaw(UnitNum)%Header_WindLevel, &
           start, count)
      
      IF ( Ios==nfok ) THEN
         MOA_NCDFDataRaw(UnitNum)%file     = MOA_NCDF_file
         MOA_NCDFDataRaw(UnitNum)%ncid     = ncid
         MOA_NCDFDataRaw(UnitNum)%all_read = .FALSE.
         DO i = 1,NumDS_NCDF
            MOA_NCDFDataRaw(UnitNum)%dsid(i) = -1
         ENDDO
         IF (PRESENT ( MOA_Header) ) THEN
            CALL MOA_Header_Get ( UnitNum, MOA_Header )
         ENDIF
      ELSE
         CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN,              &
              FuncName = "MOA_NCDF_Open()",                            &
              StrVar = " Error during MOA header read" )
      ENDIF
      
      END SUBROUTINE MOA_NCDF_Open

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE MOA_NCDF_Allocate ( UnitNum )

!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDF_Allocate
!
! !Desription:
!
!    Purpose - Allocates MOA structure of open MOA file
!
!
! !Input Parameters:
!    UnitNum    - Unit numbers of open MOA file
!
! !Output Parameters:
!
! !Team-Unique Header:
!
!  Modules Used:
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   June 12, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
      
      INTEGER, INTENT ( IN) :: UnitNum
      
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Header_PresLevel ( NL))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Header_WindLevel ( NW))
!      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Longitude(NumReg_CERES05Deg))
!      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Latitude(NumBands_CERES05Deg))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Region_number (NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_pressure(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_geopotential_height(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_skin_temperature(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Air_mass_index(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Temperature_profile(NumReg_Global_CERESNested,NL))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Humidity_profile(NumReg_Global_CERESNested,NL))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_U_wind(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_V_wind(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% U_Wind_profile(NumReg_Global_CERESNested,NW))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% V_Wind_profile(NumReg_Global_CERESNested,NW))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Troposphere_pressure(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Ozone_profile(NumReg_Global_CERESNested,NL))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Column_integrated_ozone(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Aerosol_optical_depth(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Column_averaged_relative_humidity(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Precipitable_water(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Total_cloud_fraction(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Upper_troposphere_relative_humidity(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Estimated_inversion_strength(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Potential_temperature(NumReg_Global_CERESNested))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Skin_temperature(NumReg_Global_CERESNested,NH))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Two_meter_temperature(NumReg_Global_CERESNested,NH))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Ten_meter_temperature(NumReg_Global_CERESNested,NH))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Two_meter_humidity(NumReg_Global_CERESNested,NH))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Ten_meter_humidity(NumReg_Global_CERESNested,NH))
      ALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Daily_average_two_meter_temperature(NumReg_Global_CERESNested))
    
    END SUBROUTINE MOA_NCDF_Allocate

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -

      FUNCTION Get_Dataset_Index ( dsname ) RESULT ( idx )
!
!**************************************************************************
! !F90
!
! !Name:
!   Get_Dataset_Index
!
! !Desription:
!
!    Purpose - To retrieve an index for reading a single netcdf dataset
!
!
! !Input Parameters:
!    dsname     - name of dataset to be indexed
!
! !Output Parameters:
!    idx  - dataset index
!
! !Team-Unique Header:
!
!  Modules Used:
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 27, 2023 - SCCR #2167
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      CHARACTER(LEN=*) :: dsname
      INTEGER :: idx
      INTEGER :: i
      
      idx = -1
      DO i = 1,NumDS_NCDF
         IF ( dsname .EQ. moa_dataset_names(i) ) idx = i
      ENDDO

      END FUNCTION Get_Dataset_Index
    
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -

      SUBROUTINE MOA_NCDF_GetDataset ( DSname, UnitNum, Stat )
!
!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDF_GetDataset
!
! !Desription:
!
!    Purpose - To access data for one dataset
!              and store that data in memory using 
!              MOA_NCDFDataRaw structure
!
!
! !Input Parameters:
!    DSname     - Name of dataset to be stored
!    UnitNum    - Index of which MOA file to use
!
! !Output Parameters:
!    Stat       - (Optional) I/O status returned from opening process and
!                 returned to calling routine. If omitted and an I/O error
!                 occurs, processing terminates through the Toolkit error
!                 message utility
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!   PGS_MOAIO_26500 - Parameters: MOAIO_E_FAILED_MOA_OPEN
!
! !Revision History:
!  Version 1.0   April 27, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      CHARACTER(LEN=*), INTENT (IN) :: DSname
      INTEGER,     INTENT (IN)      :: UnitNum
      INTEGER,     INTENT (OUT)     :: Stat
      
      INTEGER :: idx, ncid, dsid, Ios, nfok
      INTEGER :: start1(1),start2(2),start3(3)
      INTEGER :: count1(1),count2(2),count3(3)

      INTEGER :: IReg

      REAL :: buffer_eqag(NumReg_CERES05Deg,NumBands_CERES05Deg)
      REAL :: buffer_eqagMP(NumReg_CERES05Deg,NumBands_CERES05Deg,NL)
      REAL :: buffer_eqagWD(NumReg_CERES05Deg,NumBands_CERES05Deg,NW)
      REAL :: buffer_eqagHR(NumReg_CERES05Deg,NumBands_CERES05Deg,NH)
     
      INTEGER :: buffer_eqag_int(NumReg_CERES05Deg,NumBands_CERES05Deg)
      
      nfok = NF90_NOERR

      ncid = MOA_NCDFDataRaw(UnitNum)%ncid
      IF (UnitNum .LT. 0 .OR. ncid .LT. 0 ) THEN
         CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN, & 
              FuncName = "MOA_NCDF_GetDataset()", &
              StrVar = " UnitNum %i not opened", Ivar1=UnitNum )
      ENDIF
         
      stat = OK
      idx = Get_Dataset_Index ( DSname )
      IF (idx <= 0) THEN
       	  CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN,  &
               FuncName = "MOA_NCDF_GetDataset()", &
               StrVar = " Unknown Dataset : "//DSname ) 
      ENDIF
       
      IF (MOA_NCDFDataRaw(UnitNum)%dsid(idx) > 0) RETURN
      
      Ios = nf90_inq_varid(ncid, DSname, dsid) 
      IF (Ios /= nfok .OR. idx < 0) THEN    
         CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN,  &
              FuncName = "MOA_NCDF_GetDataset()", &
              StrVar = " NetCDF dataset not found : "//DSname ) 
      ENDIF

! *** Calculate parameters needed for grid conversion

! *** CERES 0.5 degree equal angle grid

      CALL Calc_Grid_Regions ( NumBands_CERES05Deg, NumReg_CERES05Deg,   & ! I 
                               BegReg_CERES05Deg,  EndReg_CERES05Deg)      ! I/O

! *** CERES nested grid

      CALL Calc_Grid_Regions_Nested ( NumBands_CERESNESTED, NumReg_CERESNESTED, & ! I 
                               BegReg_CERESNESTED,  EndReg_CERESNESTED)    ! I/O


      start1(1) = 1
      start2 = (/1,1/)
      start3 = (/1,1,1/)
      count2 = (/720,360/)
      count3 = (/720,360,0/)
      
      IF ( DSname .EQ. "Longitude" ) THEN  ! Just read long and lat into buffer to verify
         count1(1) = 720
         Ios = nf90_get_var(ncid, dsid,  buffer_eqag, start1, count1)
      ELSE IF ( DSname .EQ. "Latitude" ) THEN
         count1(1) = 360
         Ios = nf90_get_var(ncid, dsid,  buffer_eqag, start1, count1)
      ELSE IF ( DSname .EQ. "Region_number" ) THEN

! *** Discard equal angle region numbers and assign nested region numbers

       DO IReg = 1, NumReg_Global_CERESNested
        MOA_NCDFDataRaw(UnitNum)%Region_number(IReg) = IReg
       ENDDO

! *** Convert the data set from equal angle to nested grid

      ELSE IF ( DSname .EQ. "Surface_pressure" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Surface_pressure )
      ELSE IF ( DSname .EQ. "Surface_geopotential_height" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Surface_geopotential_height )
      ELSE IF ( DSname .EQ. "Surface_skin_temperature" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Surface_skin_temperature )
      ELSE IF ( DSname .EQ. "Air_mass_index" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag_int, start2, count2)
         CALL Convert_EQAG_Nested_Int ( buffer_eqag_int, MOA_NCDFDataRaw(UnitNum)%Air_mass_index )
      ELSE IF ( DSname .EQ. "Surface_U_wind" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Surface_U_wind )
      ELSE IF ( DSname .EQ. "Surface_V_wind" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Surface_V_wind )
      ELSE IF ( DSname .EQ. "Troposphere_pressure" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Troposphere_pressure )
      ELSE IF ( DSname .EQ. "Column_integrated_ozone" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Column_integrated_ozone )
      ELSE IF ( DSname .EQ. "Aerosol_optical_depth" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Aerosol_optical_depth )
      ELSE IF ( DSname .EQ. "Column_averaged_relative_humidity" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Column_averaged_relative_humidity )
      ELSE IF ( DSname .EQ. "Precipitable_water" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Precipitable_water )
      ELSE IF ( DSname .EQ. "Total_cloud_fraction" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Total_cloud_fraction )
      ELSE IF ( DSname .EQ. "Upper_troposphere_relative_humidity" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Upper_troposphere_relative_humidity )
      ELSE IF ( DSname .EQ. "Estimated_inversion_strength" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Estimated_inversion_strength )
      ELSE IF ( DSname .EQ. "Potential_temperature" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Potential_temperature )
      ELSE IF ( DSname .EQ. "Daily_average_two_meter_temperature" ) THEN
         Ios = nf90_get_var(ncid, dsid, buffer_eqag, start2, count2)
         CALL Convert_EQAG_Nested_Real ( buffer_eqag, MOA_NCDFDataRaw(UnitNum)%Daily_average_two_meter_temperature )
      ELSE IF ( DSname .EQ. "Temperature_profile" ) THEN
         count3(3) = NL
         Ios = nf90_get_var(ncid, dsid, buffer_eqagMP, start3, count3)
         CALL Convert_EQAG_Nested_RealMP ( buffer_eqagMP, MOA_NCDFDataRaw(UnitNum)%Temperature_profile)
      ELSE IF ( DSname .EQ. "Humidity_profile" ) THEN
         count3(3) = NL
         Ios = nf90_get_var(ncid, dsid, buffer_eqagMP, start3, count3)
         CALL Convert_EQAG_Nested_RealMP ( buffer_eqagMP, MOA_NCDFDataRaw(UnitNum)%Humidity_profile)
      ELSE IF ( DSname .EQ. "Ozone_profile" ) THEN
         count3(3) = NL
         Ios = nf90_get_var(ncid, dsid, buffer_eqagMP, start3, count3)
         CALL Convert_EQAG_Nested_RealMP ( buffer_eqagMP, MOA_NCDFDataRaw(UnitNum)%Ozone_profile)
      ELSE IF ( DSname .EQ. "U_Wind_profile" ) THEN
         count3(3) = NW
         Ios = nf90_get_var(ncid, dsid, buffer_eqagWD, start3, count3)
         CALL Convert_EQAG_Nested_RealWD ( buffer_eqagWD, MOA_NCDFDataRaw(UnitNum)%U_Wind_profile)
      ELSE IF ( DSname .EQ. "V_Wind_profile" ) THEN
         count3(3) = NW
         Ios = nf90_get_var(ncid, dsid, buffer_eqagWD, start3, count3)
         CALL Convert_EQAG_Nested_RealWD ( buffer_eqagWD, MOA_NCDFDataRaw(UnitNum)%V_Wind_profile)
      ELSE IF ( DSname .EQ. "Hourly_Skin_temperature" ) THEN
         count3(3) = 3
         Ios = nf90_get_var(ncid, dsid, buffer_eqagHR, start3, count3)
         CALL Convert_EQAG_Nested_RealHourly ( buffer_eqagHR, MOA_NCDFDataRaw(UnitNum)%Hourly_Skin_temperature)
      ELSE IF ( DSname .EQ. "Hourly_Two_meter_temperature" ) THEN
         count3(3) = 3
         Ios = nf90_get_var(ncid, dsid, buffer_eqagHR, start3, count3)
         CALL Convert_EQAG_Nested_RealHourly ( buffer_eqagHR, MOA_NCDFDataRaw(UnitNum)%Hourly_Two_meter_temperature)
      ELSE IF ( DSname .EQ. "Hourly_Ten_meter_temperature" ) THEN
         count3(3) = 3
         Ios = nf90_get_var(ncid, dsid, buffer_eqagHR, start3, count3)
         CALL Convert_EQAG_Nested_RealHourly ( buffer_eqagHR, MOA_NCDFDataRaw(UnitNum)%Hourly_Ten_meter_temperature)
      ELSE IF ( DSname .EQ. "Hourly_Two_meter_humidity" ) THEN
         count3(3) = 3
         Ios = nf90_get_var(ncid, dsid, buffer_eqagHR, start3, count3)
         CALL Convert_EQAG_Nested_RealHourly ( buffer_eqagHR, MOA_NCDFDataRaw(UnitNum)%Hourly_Two_meter_humidity)
      ELSE IF ( DSname .EQ. "Hourly_Ten_meter_humidity" ) THEN
         count3(3) = 3
         Ios = nf90_get_var(ncid, dsid, buffer_eqagHR, start3, count3)
         CALL Convert_EQAG_Nested_RealHourly ( buffer_eqagHR, MOA_NCDFDataRaw(UnitNum)%Hourly_Ten_meter_humidity)
      ELSE
         Ios = BAD         
      ENDIF
      
      IF ( Ios == nfok) THEN
         MOA_NCDFDataRaw(UnitNum)%dsid(idx) = dsid
      ELSE
         CALL WriteReport (MOAIO_E_FAILED_MOA_OPEN, & 
              FuncName = "MOA_NCDF_GetDataset()", &
              StrVar = " Error during NetCDF dataset read : "//DSname ) 
         Stat = BAD
      ENDIF

    END SUBROUTINE MOA_NCDF_GetDataset

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE Convert_EQAG_Nested_Real ( Data_In, Data_Out )

!**************************************************************************
! !F90
!
! !Name:
!   Convert_EQAG_Nested_Real
!
! !Desription:
!
!    Purpose - convert a 2 dimensional equal angle grid to 1 dimensional nested grid
!
!
! !Input Parameters:
!    Data_In  - 2 dimensional array of equal angle data
!
! !Output Parameters:
!    Data_Out - 1 dimensional array of nested data
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      REAL, INTENT ( IN)  :: Data_In(NumReg_CERES05Deg,NumBands_CERES05Deg)
      REAL, INTENT ( OUT) :: Data_Out(NumReg_Global_CERESNested)
      
      INTEGER  :: I, X, Y, IReg
      REAL :: Data_Tmp(NumReg_Global_CERES05Deg)

! *** Transfer input data to 1 dimensional array

       IReg = 1
       DO Y = 1, NumBands_CERES05Deg  ! 360
        DO X = 1, NumReg_CERES05Deg   ! 720
         Data_Tmp(IReg) = Data_In(X,Y)
	 IReg=IReg+1
	ENDDO
       ENDDO
      
! *** Call regridding routine
      
      CALL Regrid_DRV ( Index_CERES05Deg, Index_CERESNESTED, Data_Tmp, & ! I
                         Default_MOA_REAL, DPerAllow_CERES05Deg,         & ! I
                                                              Data_Out)     ! O
!							      
    END SUBROUTINE Convert_EQAG_Nested_Real
        
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE Convert_EQAG_Nested_RealMP ( Data_In, Data_Out )

!**************************************************************************
! !F90
!
! !Name:
!   Convert_EQAG_Nested_RealMP
!
! !Desription:
!
!    Purpose - convert a 2 dimensional equal angle grid to 1 dimensional nested grid
!
!
! !Input Parameters:
!    Data_In  - 2 dimensional array of equal angle data
!
! !Output Parameters:
!    Data_Out - 1 dimensional array of nested data
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      REAL, INTENT ( IN)  :: Data_In(NumReg_CERES05Deg,NumBands_CERES05Deg,NL)
      REAL, INTENT ( OUT) :: Data_Out(NumReg_Global_CERESNested, NL)
      
      INTEGER  :: I, X, Y, IReg, level
      REAL :: Data_Tmp(NumReg_Global_CERES05Deg, NL)

! *** Transfer input data to 1 dimensional array

       DO level = 1, NL
        IReg = 1
        DO Y = 1, NumBands_CERES05Deg
         DO X = 1, NumReg_CERES05Deg
          Data_Tmp(IReg,level) = Data_In(X,Y, level)
 	 IReg=IReg+1
         ENDDO
        ENDDO

! *** Call regridding routine
      
        CALL Regrid_DRV ( Index_CERES05Deg, Index_CERESNESTED, Data_Tmp(:,level), & ! I
                                    Default_MOA_REAL, DPerAllow_CERES05Deg,         & ! I
                                                                Data_Out(:,level))     ! O
       ENDDO
!             
!
    END SUBROUTINE Convert_EQAG_Nested_RealMP

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE Convert_EQAG_Nested_RealWD ( Data_In, Data_Out )

!**************************************************************************
! !F90
!
! !Name:
!   Convert_EQAG_Nested_RealWD
!
! !Desription:
!
!    Purpose - convert a 2 dimensional equal angle grid to 1 dimensional nested grid
!
!
! !Input Parameters:
!    Data_In  - 2 dimensional array of equal angle data
!
! !Output Parameters:
!    Data_Out - 1 dimensional array of nested data
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      REAL, INTENT ( IN)  :: Data_In(NumReg_CERES05Deg,NumBands_CERES05Deg,NW)
      REAL, INTENT ( OUT) :: Data_Out(NumReg_Global_CERESNested, NW)
      
      INTEGER  :: I, X, Y, IReg, level
      REAL :: Data_Tmp(NumReg_Global_CERES05Deg, NW)

! *** Transfer input data to 1 dimensional array

       DO level = 1, NW
        IReg = 1
        DO Y = 1, NumBands_CERES05Deg
         DO X = 1, NumReg_CERES05Deg
          Data_Tmp(IReg,level) = Data_In(X,Y, level)
 	 IReg=IReg+1
         ENDDO
        ENDDO

! *** Call regridding routine
      
        CALL Regrid_DRV ( Index_CERES05Deg, Index_CERESNESTED, Data_Tmp(:,level), & ! I
                                    Default_MOA_REAL, DPerAllow_CERES05Deg,         & ! I
                                                                Data_Out(:,level))     ! O
       ENDDO
!             
!
    END SUBROUTINE Convert_EQAG_Nested_RealWD
        
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE Convert_EQAG_Nested_RealHourly ( Data_In, Data_Out )

!**************************************************************************
! !F90
!
! !Name:
!   Convert_EQAG_Nested_RealHourly
!
! !Desription:
!
!    Purpose - convert a 2 dimensional equal angle grid to 1 dimensional nested grid
!
!
! !Input Parameters:
!    Data_In  - 2 dimensional array of equal angle data
!
! !Output Parameters:
!    Data_Out - 1 dimensional array of nested data
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      REAL, INTENT ( IN)  :: Data_In(NumReg_CERES05Deg,NumBands_CERES05Deg,3)
      REAL, INTENT ( OUT) :: Data_Out(NumReg_Global_CERESNested, 3)
      
      INTEGER  :: I, X, Y, IReg, level
      REAL :: Data_Tmp(NumReg_Global_CERES05Deg, 3)

! *** Transfer input data to 1 dimensional array

       DO level = 1, 3
        IReg = 1
        DO Y = 1, NumBands_CERES05Deg
         DO X = 1, NumReg_CERES05Deg
          Data_Tmp(IReg,level) = Data_In(X,Y, level)
 	 IReg=IReg+1
         ENDDO
        ENDDO

! *** Call regridding routine
      
        CALL Regrid_DRV ( Index_CERES05Deg, Index_CERESNESTED, Data_Tmp(:,level), & ! I
                                    Default_MOA_REAL, DPerAllow_CERES05Deg,         & ! I
                                                                Data_Out(:,level))     ! O
       ENDDO
!             
!
    END SUBROUTINE Convert_EQAG_Nested_RealHourly
        
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE Convert_EQAG_Nested_Int ( Data_In, Data_Out )

!**************************************************************************
! !F90
!
! !Name:
!   Convert_EQAG_Nested_Real
!
! !Desription:
!
!    Purpose - convert a 2 dimensional equal angle grid to 1 dimensional nested grid
!
!
! !Input Parameters:
!    Data_In    - 2 dimensional array of equal angle data
!
! !Output Parameters:
!    Data_Out - 1 dimensional array of nested data
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, INTENT ( IN)  :: Data_In(NumReg_CERES05Deg,NumBands_CERES05Deg)
      INTEGER, INTENT ( OUT) :: Data_Out(NumReg_Global_CERESNested)
      
      INTEGER  :: I, X, Y, IReg
      REAL :: Data_Tmp(NumReg_Global_CERES05Deg)
      REAL :: Data_Tmp2(NumReg_Global_CERESNested)

! *** Transfer input data to 1 dimensional array

       IReg = 1
       DO Y = 1, NumBands_CERES05Deg
        DO X = 1, NumReg_CERES05Deg
         Data_Tmp(IReg) = Data_In(X,Y)
	 IReg=IReg+1
	ENDDO
       ENDDO
      
! *** Call regridding routine
      
      CALL Regrid_DRV ( Index_CERES05Deg, Index_CERESNESTED, Data_Tmp, & ! I
                         Default_MOA_REAL, DPerAllow_CERES05Deg,         & ! I
                                                             Data_Tmp2)     ! O

      DO IReg = 1,NumReg_Global_CERESNested 
       IF (Data_Tmp2(IReg) == Default_MOA_REAL) THEN
         Data_Out(IReg) = Default_MOA_INT
       ELSE 
         Data_Out(IReg) = INT(Data_Tmp2(IReg))
       ENDIF
      ENDDO
    
    END SUBROUTINE Convert_EQAG_Nested_Int
        
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE MOA_NCDF_Close_Wrapper ( UnitNums )
    
!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDF_Close_Wrapper
!
! !Desription:
!
!    Purpose - closes up to two open MOA files
!
!
! !Input Parameters:
!    UnitNum    - Unit numbers assigned by Toolkit to MOA file(s)
!
! !Output Parameters:
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, INTENT ( IN) :: UnitNums ( 2)
      
      INTEGER i
      
      DO I = 1, 2
         IF ( UnitNums ( I) /= -9) THEN
            CALL MOA_NCDF_Close ( UnitNums ( I) ) 
         END IF
      END DO

    END SUBROUTINE MOA_NCDF_Close_Wrapper
      
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE MOA_NCDF_Deallocate ( UnitNum )

!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDF_Deallocate
!
! !Desription:
!
!    Purpose - Deallocates MOA structure of open MOA file
!
!
! !Input Parameters:
!    UnitNum    - Unit numbers of open MOA file
!
! !Output Parameters:
!
! !Team-Unique Header:
!
!  Modules Used:
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   June 12, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
      
      INTEGER, INTENT ( IN) :: UnitNum
      
      IF ( MOA_NCDFinUse(UnitNum) ) THEN
         
	 DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Header_PresLevel)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Header_WindLevel)
!         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Longitude)
!         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Latitude)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Region_number )
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_pressure)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_geopotential_height)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_skin_temperature)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Air_mass_index)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Temperature_profile)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Humidity_profile)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_U_wind)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Surface_V_wind)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% U_Wind_profile)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% V_Wind_profile)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Troposphere_pressure)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Ozone_profile)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Column_integrated_ozone)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Aerosol_optical_depth)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Column_averaged_relative_humidity)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Precipitable_water)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Total_cloud_fraction)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Upper_troposphere_relative_humidity)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Estimated_inversion_strength)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Potential_temperature)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Skin_temperature)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Two_meter_temperature)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Ten_meter_temperature)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Two_meter_humidity)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Hourly_Ten_meter_humidity)
         DEALLOCATE ( MOA_NCDFDataRaw(UnitNum)% Daily_average_two_meter_temperature)
      
      ENDIF
    
    END SUBROUTINE MOA_NCDF_Deallocate

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE MOA_NCDF_Close ( UnitNum )
    
!**************************************************************************
! !F90
!
! !Name:
!   MOA_NCDF_Close
!
! !Desription:
!
!    Purpose - closes an open MOA file
!
!
! !Input Parameters:
!    UnitNum    - Unit number assigned by Toolkit to MOA file(s)
!
! !Output Parameters:
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, OPTIONAL :: UnitNum
      
      INTEGER :: i
      
      IF ( PRESENT (UnitNum)) THEN
         CALL MOA_NCDF_Deallocate(UnitNum)
         MOA_NCDFinUse(UnitNum) = .FALSE.
         MOA_NCDFDataRaw(UnitNum)%ncid = -1
         MOA_NCDFDataRaw(UnitNum)%file = ""
      ELSE
         DO i = 1,MaxMOA_NCDF
            CALL MOA_NCDF_Deallocate(i)
            MOA_NCDFDataRaw(i)%ncid = -1
            MOA_NCDFDataRaw(i)%file = ""
         ENDDO
         MOA_NCDFinUse(:) = .FALSE.
      ENDIF

    END SUBROUTINE MOA_NCDF_Close
            
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE MOA_Header_Get ( moanum, MOA_Header )

!**************************************************************************
! !F90
!
! !Name:
!   MOA_Header_Get
!
! !Desription:
!
!    Purpose - assigns header values from a specific open file
!
!
! !Input Parameters:
!    moanum    - Number indicating open MOA file
!
! !Output Parameters:
!    MOA_Header - MOA_Header structure
!
! !Team-Unique Header:
!
!  Modules Used:
!    ceres_status - (CERESlib) Parameters: BAD, OK
!    MSG          - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER,                INTENT (IN)    :: moanum
      TYPE (MOA_HEADER_TYPE), INTENT (INOUT) :: MOA_Header

      MOA_Header%DataDate = MOA_NCDFDataRaw(moanum)%Header_DataDate 
      MOA_Header%ProcDate = MOA_NCDFDataRaw(moanum)%Header_ProcDate
      MOA_Header%GridIdx_Input = MOA_NCDFDataRaw(moanum)%Header_GridIdxIn
      MOA_Header%GridIdx_Output = MOA_NCDFDataRaw(moanum)%Header_GridIdxOut
      MOA_Header%NumReg_MOA = MOA_NCDFDataRaw(moanum)%Header_NumReg
      MOA_Header%Pres_MP = MOA_NCDFDataRaw(moanum)%Header_PresLevel
      MOA_Header%WindLev = MOA_NCDFDataRaw(moanum)%Header_WindLevel

    END SUBROUTINE MOA_Header_Get

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
      
    SUBROUTINE MOA_Data_Get_Reg ( moanum, RegIndex, MOA )

!**************************************************************************
! !F90
!
! !Name:
!   MOA_Data_Get_Reg 
!
! !Desription:
!
!    Purpose - assigns values to a MOA structure from 
!              the global arrays for specific hour and coordinates
!
!
! !Input Parameters:
!    moanum    - index of open MOA file    
!    RegIndex  - region index of data to read    
!
! !Output Parameters:
!    MOA - output MOA structure
!
! !Team-Unique Header:
!
!  Modules Used:
!
!  External Files Used:
!
! !Revision History:
!  Version 1.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, INTENT(IN) :: moanum
      INTEGER,INTENT (IN) :: RegIndex
      
      TYPE (MOA_TYPE), INTENT (OUT) :: MOA

      INTEGER :: i, stat

! *** Put data from Netcdf MOA datasets into MOA_TYPE structure
!     To read regions into MOA_TYPE, all datasets must be read in
      
      IF ( .NOT. MOA_NCDFDataRaw(moanum) % all_read ) THEN
         DO i = 1,NumDS_NCDF
            CALL MOA_NCDF_GetDataset ( moa_dataset_names(i), moanum, stat )
         ENDDO
         MOA_NCDFDataRaw(moanum) % all_read = .TRUE.
      ENDIF
      
      MOA % Temp_MP           = MOA_NCDFDataRaw(moanum) % Temperature_profile(RegIndex,:)
      MOA % SfcPres           = MOA_NCDFDataRaw(moanum) % Surface_pressure(RegIndex)      
      MOA % RegIdx            = MOA_NCDFDataRaw(moanum) % Region_number(RegIndex)
      MOA % SfcGeoPt          = MOA_NCDFDataRaw(moanum) % Surface_geopotential_height(RegIndex)
      MOA % SfcSknTemp        = MOA_NCDFDataRaw(moanum) % Surface_skin_temperature (RegIndex)
      MOA % SfcUwind          = MOA_NCDFDataRaw(moanum) % Surface_U_wind(RegIndex)
      MOA % SfcVwind          = MOA_NCDFDataRaw(moanum) % Surface_V_wind(RegIndex)
      MOA % Humid_MP          = MOA_NCDFDataRaw(moanum) % Humidity_profile(RegIndex,:)
      MOA % UWind_MP          = MOA_NCDFDataRaw(moanum) % U_Wind_profile(RegIndex,:)
      MOA % VWind_MP          = MOA_NCDFDataRaw(moanum) % V_Wind_profile(RegIndex,:)
      MOA % TropHt            = MOA_NCDFDataRaw(moanum) % Troposphere_pressure(RegIndex)  
      MOA % AirMassIdx        = MOA_NCDFDataRaw(moanum) % Air_mass_index(RegIndex) 
      MOA % Precip_H2O        = MOA_NCDFDataRaw(moanum) % Precipitable_water(RegIndex)
      MOA % RelHum_ColAvg     = MOA_NCDFDataRaw(moanum) % Column_averaged_relative_humidity(RegIndex)
      MOA % PO3               = MOA_NCDFDataRaw(moanum) % Ozone_profile(RegIndex,:) 
      MOA % CO3               = MOA_NCDFDataRaw(moanum) % Column_integrated_ozone(RegIndex)
      MOA % CA_OptDepth       = MOA_NCDFDataRaw(moanum) % Aerosol_optical_depth(RegIndex)
      MOA % Cld_Frac          = MOA_NCDFDataRaw(moanum) % Total_cloud_fraction(RegIndex)
      MOA % UTRH              = MOA_NCDFDataRaw(moanum) % Upper_troposphere_relative_humidity(RegIndex)
      MOA % EIS               = MOA_NCDFDataRaw(moanum) % Estimated_inversion_strength(RegIndex)
      MOA % PotTemp           = MOA_NCDFDataRaw(moanum) % Potential_temperature(RegIndex)
      MOA % DailyTwoMeterTemp = MOA_NCDFDataRaw(moanum) % Daily_average_two_meter_temperature(RegIndex)
!
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "TMPU",              & ! I
                                            Pres_MP, MOA%Temp_MP)     ! I/O
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "SPHU",              & ! I
                                            Pres_MP, MOA%Humid_MP)    ! I/O
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "POZ3",              & ! I
                                           Pres_MP, MOA%PO3)          ! I/O
   
    END SUBROUTINE MOA_Data_Get_Reg
!
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
    SUBROUTINE MOA_NCDFRead_Region (UnitNum, GridIdx,            & ! I
                                             MOA, Stat,          & ! O
                                                        Ireg)      ! I
!
!**************************************************************************
!
! !F90
!
! !Name:
!   MOA_NCDFRead_Region
!
! !Description:
!   Routine ID -
!
!   Purpose - To read one region's worth of MOA data and return a
!             successful/unsuccessful flag to calling routine
!
!
! !Input Parameters:
!   Ireg      - Region number for which to read MOA data
!   GridIdx   - Grid index for MOA file
!   UnitNum   - Input unit number for MOA file
!
! !Output Parameters:
!   MOA     - Record of MOA data
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Internal Routines Used:
!   
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf       - nf90_inq_varid, nf90_get_var
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_READ
!                                   MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_REGNUM_INVALID
!
! !Revision History:
!  Version 3.0   April 17, 2023 - SCCR #2167
!                Thomas.E.Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                Rita.A.Smith, SSAI, Rita.A.Smith@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.1   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                 1. Added code to handle MOA data on the CERES Nested 1x1-deg
!                    Grid.
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                1. Added code to handle multiple MOA Grids
!
! !END
!
!**************************************************************************
!
!
      INTEGER,         INTENT (IN)  :: Ireg
      INTEGER,         INTENT (IN)  :: GridIdx
      TYPE (MOA_TYPE), INTENT (OUT) :: MOA
      INTEGER,         INTENT (OUT) :: Stat
      INTEGER,         INTENT (IN)  :: UnitNum
!
      INTEGER :: NumReg_Global_TARGET
      INTEGER :: Lat, Long, LatIndex, LongIndex

!
! *** Initialize success flag to indicate success
!
      Stat = OK
!
      
      IF (GridIdx == Index_CERES05Deg ) THEN
        NumReg_Global_TARGET = NumReg_Global_CERESNested
      ELSE
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_NCDFRead_Region()",          & ! I
                            StrVar = " occured with Grid_Index :%i3"//   & ! I
                                     " grid(35) for GEOS-IT, "//         & ! I
                                 "- All others are Unavailable",         & ! I
                            Ivar1 = GridIdx)
      END IF

!
     IF ( Ireg < 1  .OR.  Ireg > NumReg_Global_TARGET) THEN
!     *** Region number out of range
!
	  CALL WriteReport (MOAIO_E_FAILED_REGNUM_INVALID,              & ! I
              FuncName = "NCDFRead_Region()",                           & ! I
              StrVar = " occured with Region Number :%i8 "//            & ! I
                       " -Max Region Number for Grid(%i1) is %i5",      & ! I
              Ivar1 = Ireg, Ivar2 = Ireg,                               & ! I
              Ivar3 = NumReg_Global_TARGET)
      END IF
!
      CALL MOA_Data_Get_Reg ( UnitNum, Ireg, MOA )
!      
!
      END SUBROUTINE MOA_NCDFRead_Region


! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_LatLon (UnitNum, GridIdx,                & ! I
                                               MOA, Stat,              & ! O
                                                          Lat,Long)      ! I
!
!**************************************************************************
!
! !F90
!
! !Name:
!   MOA_NCDFRead_LatLon
!
! !Description:
!   Routine ID -
!
!   Purpose - To read one region's worth of MOA data and return a
!             successful/unsuccessful flag to calling routine
!
!
! !Input Parameters:
!   Lat       - Latitude for which to read MOA data
!   Long      - Longitude for which to read MOA data
!   GridIdx   - Grid index for MOA file
!   UnitNum   - Input unit number for MOA file
!
! !Output Parameters:
!   MOA     - Record of MOA data
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Internal Routines Used:
!   
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf       - nf90_inq_varid, nf90_get_var
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_READ
!                                   MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_REGNUM_INVALID
!
! !Revision History:
!  Version 3.0   May 22, 2019 - SCCR #
!                Thomas Caldwell, Thomas.E.Caldwell@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.1   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                 1. Added code to handle MOA data on the CERES Nested 1x1-deg
!                    Grid.
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                1. Added code to handle multiple MOA Grids
!
! !END
!
!**************************************************************************
!
!
      REAL,            INTENT (IN)  :: Lat
      REAL,            INTENT (IN)  :: Long
      INTEGER,         INTENT (IN)  :: GridIdx
      TYPE (MOA_TYPE), INTENT (OUT) :: MOA
      INTEGER,         INTENT (OUT) :: Stat
      INTEGER,         INTENT (IN)  :: UnitNum
!
      INTEGER :: NumReg_Global_TARGET !,irow,lev

      INTEGER :: IReg

!
! *** Initialize success flag to indicate success
!
      Stat = OK
!
      IF (GridIdx == Index_CERES05Deg ) THEN
        NumReg_Global_TARGET = NumReg_Global_CERESNested
      ELSE
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_NCDFRead_LatLon()",                & ! I
                            StrVar = " occured with Grid_Index :%i3"//   & ! I
                                     " grid(35) for GEOS-IT, "//        & ! I
                                 "- All others are Unavailable",         & ! I
                            Ivar1 = GridIdx)
      END IF

!
!
      CALL LatLong_Reg (Lat, Long, GridIdx,                   & ! I
                                               IReg)            ! O
!      
      CALL MOA_Data_Get_Reg ( UnitNum, IReg, MOA )
!      
!
      END SUBROUTINE MOA_NCDFRead_LatLon


! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_Hourly_Region (UnitNum, GridIdx,        & ! I
                                             Param_Flag, Ireg, Hour,  & ! I
					              Stat, Data_Out)   ! O
!
!**************************************************************************
!
! !F90
!
! !Name:
!   MOA_NCDFRead_Hourly_Region
!
! !Description:
!   Routine ID -
!
!   Purpose - To read the hourly parameters for a specific region
!
!
! !Input Parameters:
!   Ireg      - Region number for which to read MOA data
!   GridIdx   - Grid index for MOA file
!   UnitNum   - Input unit number for MOA file
!
! !Output Parameters:
!   MOA     - Record of MOA data
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Internal Routines Used:
!   
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf       - nf90_inq_varid, nf90_get_var
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_READ
!                                   MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_REGNUM_INVALID
!
! !Revision History:
!  Version 3.0   May 22, 2019 - SCCR #
!                Thomas Caldwell, Thomas.E.Caldwell@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.1   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                 1. Added code to handle MOA data on the CERES Nested 1x1-deg
!                    Grid.
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                1. Added code to handle multiple MOA Grids
!
! !END
!
!**************************************************************************
!
!
      INTEGER,         INTENT (IN)  :: UnitNum
      INTEGER,         INTENT (IN)  :: GridIdx
      CHARACTER(LEN=4),INTENT (IN)  :: Param_Flag
      INTEGER,         INTENT (IN)  :: Ireg
      INTEGER,         INTENT (IN)  :: Hour

      INTEGER,         INTENT (OUT) :: Stat
      REAL,            INTENT (OUT) :: Data_Out
!
      INTEGER :: NumReg_Global_TARGET !,irow,lev
      INTEGER :: NCDF_Hour
      INTEGER :: LatIndex, LongIndex, LatInt, LongInt

!
! *** Initialize success flag to indicate success
!
      Stat = OK
!
      IF (GridIdx == Index_CERES05Deg ) THEN
       NumReg_Global_TARGET = NumReg_Global_CERESNested
      ELSE
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,             & ! I
                            FuncName = "MOA_NCDFRead_Hourly_Region()",  & ! I
                            StrVar = " occured with Grid_Index :%i3"//  & ! I
                                     " grid(1) for GEOS 2x2.5, "//       & ! I
                                     " grid(2) for GEOS 1x1, "//         & ! I
                                     " grid(3) for GEOS 1x1.25, "//      & ! I
                                     " grid(4) for NCEP, "//            & ! I
                                     " grid(5-9) for CERES Grid "//     & ! I
                                   "- All others are Unavailable",      & ! I
                            IVar1 = GridIdx)                             ! I
      ENDIF
!
      IF ( Ireg < 1  .OR.  Ireg > NumReg_Global_TARGET) THEN
!     *** Region number out of range
!
          CALL WriteReport (MOAIO_E_FAILED_REGNUM_INVALID,              & ! I
              FuncName = "NCDFRead_Hourly_Region()",                            & ! I
              StrVar = " occured with Region Number :%i8 "//            & ! I
                       " -Max Region Number for Grid(%i1) is %i5",      & ! I
              Ivar1 = Ireg, Ivar2 = Ireg,                           & ! I
              Ivar3 = NumReg_Global_TARGET)
       END IF
!
! *** Increment hour index to netCDF expectation      
      
       NCDF_Hour = Hour + 1
      
       IF (Param_Flag == "TSKN") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Skin_temperature(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "T02M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Two_meter_temperature(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "T10M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Ten_meter_temperature(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "Q02M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Two_meter_humidity(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "Q10M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Ten_meter_humidity(IReg,NCDF_Hour)
       ENDIF

      END SUBROUTINE MOA_NCDFRead_Hourly_Region

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_Hourly_LatLon (UnitNum, GridIdx,        & ! I
                                             Param_Flag,              & ! I
					     Lat, Long, Hour,         & ! I
					              Stat, Data_Out)   ! O
!
!**************************************************************************
!
! !F90
!
! !Name:
!   MOA_NCDFRead_Hourly_LatLon
!
! !Description:
!   Routine ID -
!
!   Purpose - To read one region's worth of MOA data and return a
!             successful/unsuccessful flag to calling routine
!
!
! !Input Parameters:
!   Ireg      - Region number for which to read MOA data
!   UnitNum   - Input unit number for MOA file
!
! !Output Parameters:
!   MOA     - Record of MOA data
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Internal Routines Used:
!   
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf       - nf90_inq_varid, nf90_get_var
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_READ
!                                   MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_REGNUM_INVALID
!
! !Revision History:
!  Version 3.0   May 22, 2019 - SCCR #
!                Thomas Caldwell, Thomas.E.Caldwell@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.1   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                 1. Added code to handle MOA data on the CERES Nested 1x1-deg
!                    Grid.
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                1. Added code to handle multiple MOA Grids
!
! !END
!
!**************************************************************************
!
!
      INTEGER,         INTENT (IN)  :: UnitNum
      INTEGER,         INTENT (IN)  :: GridIdx
      CHARACTER(LEN=4),INTENT (IN)  :: Param_Flag
      REAL,            INTENT (IN)  :: Lat
      REAL,            INTENT (IN)  :: Long
      INTEGER,         INTENT (IN)  :: Hour

      INTEGER,         INTENT (OUT) :: Stat
      REAL,            INTENT (OUT) :: Data_Out
!
      INTEGER :: NumReg_Global_TARGET !,irow,lev

      INTEGER :: IReg, NCDF_Hour
!
! *** Initialize success flag to indicate success 
!
      Stat = OK
!
      IF (GridIdx == Index_CERES05Deg ) THEN
       NumReg_Global_TARGET = NumReg_Global_CERESNested
      ELSE
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,             & ! I
                            FuncName = "MOA_NCDFRead_Hourly_Region()",  & ! I
                            StrVar = " occured with Grid_Index :%i3"//  & ! I
                                     " grid(1) for GEOS 2x2.5, "//       & ! I
                                     " grid(2) for GEOS 1x1, "//         & ! I
                                     " grid(3) for GEOS 1x1.25, "//      & ! I
                                     " grid(4) for NCEP, "//            & ! I
                                     " grid(5-9) for CERES Grid "//     & ! I
                                   "- All others are Unavailable",      & ! I
                            IVar1 = GridIdx)                             ! I
      ENDIF
!
! *** Convert input lat and long to hourly grid coordinates
!
      CALL LatLong_Reg (Lat, Long, GridIdx,                   & ! I
                                               IReg)            ! O

! *** Increment hour index to netCDF expectation      
      
       NCDF_Hour = Hour + 1
!
       IF (Param_Flag == "TSKN") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Skin_temperature(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "T02M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Two_meter_temperature(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "T10M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Ten_meter_temperature(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "Q02M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Two_meter_humidity(IReg,NCDF_Hour)
       ENDIF

       IF (Param_Flag == "Q10M") THEN
          Data_Out = MOA_NCDFDataRaw(UnitNum) % Hourly_Ten_meter_humidity(IReg,NCDF_Hour)
       ENDIF
!
      END SUBROUTINE MOA_NCDFRead_Hourly_LatLon

! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Get_NCDFHourly_Data ( Lat, Long, Hour, UnitNum1, UnitNum2,  & ! I
                                                   Grid_Idx, Param_Flag,     & ! I
                                                           Stat, Hourly_Data)  ! O
!
!**************************************************************************
!
! !F90
!  Name: Get_NCDFHourly_Data
!
! !Description:
!   Routine ID -
!
!   Purpose : Given latitudinal and longitudinal coordinates and the hour,
!             gets or interpolates the proper Skin Temperature Data value.
!
! !Input Parameters:
!   Grid_Idx  - Flag indicating which grid current MOA Data is using
!   Lat       - Latitude (deg)  [0. : 180.]
!   Long      - Longitude (deg) [0. : 360.]
!   Hour      - Hour represented by groups of 6 [0. : .6]
!   UnitNum1   - Unit number of the first MOA file
!   UnitNum2   - Unit number of the second MOA file
!
! !Output Parameters:
!   Hourly_Data - The Skin Temperature Value for the given latitude and
!             longitude coordinates and hour.
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
! !Key Local Parameters:
!   HourSet1         - First set of hourly data contained in MOA files
!   HourSet2         - Second set of hourly data contained in MOA files
!   Hourly_Unitnum1     - Unit number of first MOA file
!   Hourly_Unitnum2     - Unit number of second MOA file

!
!  Internal Module Parameters Used:
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_E_FAILED_COLAT_INVALID,
!                                   MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_LONG_INVALID,
!                                   MOAIO_E_FAILED_REGNUM_INVALID,
!                                   MOAIO_W_FAILED_MOA_READ
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   MSG          - Sbr. WriteReport
!
! !Revision History:
!  Modification of Get_MW_Data subroutine
!                July 21, 1999
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!
!  Get_MW_Data subroutine
!   Version 2.0  May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!
! !END
!
!**************************************************************************
!
!
     INTEGER, INTENT ( IN)  :: Grid_Idx
     INTEGER, INTENT ( IN)  :: Hour
     REAL,    INTENT ( IN)  :: Lat, Long
     INTEGER, INTENT ( IN)  :: UnitNum1
     INTEGER, INTENT ( IN)  :: UnitNum2

     CHARACTER(LEN=4),INTENT (IN)  :: Param_Flag

     INTEGER, INTENT (OUT)  :: Stat
     REAL,    INTENT (OUT)  :: Hourly_Data
!
     INTEGER :: HourSet1, HourSet2
     REAL    :: Hourly_Data1, Hourly_Data2
     INTEGER :: Hourly_Unitnum1, Hourly_Unitnum2
     INTEGER :: SKT_Offset
!
     Hourly_Data1 = REAL4_DFLT
     Hourly_Data2 = REAL4_DFLT
     Hourly_Data  = REAL4_DFLT
!
      IF ( Hour < 0 .OR. Hour > 3) THEN
!     *** Hour out of range
!
          CALL WriteReport (MOAIO_W_FAILED_HOUR_INVALID,         & ! I
                            FuncName = "Get_NCDFHourly_Data()",         & ! I
                            StrVar = "Hour Invalid") ! I
      END IF
!
     IF ( Grid_Idx == Index_CERES05Deg) THEN ! Edition5 MOA
!
      IF (Hour == 0) THEN     ! read zero hour of current file
       Hourly_Unitnum1 = UnitNum1
       HourSet1 = 0
      ELSEIF ( Hour > 0 .AND. Hour < 3) THEN ! hours 1 .. 3 
       Hourly_Unitnum1 = UnitNum1
       HourSet1 = Hour
      ELSEIF (Hour == 3) THEN ! read zero hour of next file
       Hourly_Unitnum1 = UnitNum2
       HourSet1 = 0
      ENDIF
!
      CALL MOA_NCDFRead_Hourly_LatLon (Hourly_Unitnum1, Grid_Idx,          & ! I
                                       Param_Flag, Lat, Long, HourSet1,    & ! I
					             Stat, Hourly_Data1)     ! O
      Hourly_Data = Hourly_Data1
!
     ELSE 
!
!     *** Index Number is Something we can't use
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,               & ! I
                            FuncName = "Get_NCDFHourly_Data()",                   & ! I
                            StrVar = " occured with Grid_Index :%i3"//    & ! I
                                 "- All others are Unavailable",          & ! I
                                                 Ivar1 = Grid_Idx)
!
     ENDIF
!
!
      END SUBROUTINE Get_NCDFHourly_Data
!
!            
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Region_LatLon ( RegionNum, Grid_Idx,                & ! I
                                                     Lat, Lon)         ! O
!
!**************************************************************************
!
! !F90
!  Name: Region_LatLon
!
! !Description:
!   Routine ID -
!
!   Purpose: Given a MOA Region Number, get latitudinal and
!            longitudinal coordinates for the proper grid.
!
! !Input Parameters:
!   RegionNum - MOA Region number
!   Grid_Idx  - Flag indicating which grid current MOA Data is using
!
! !Output Parameters:
!   Lat      - Latitude (deg) [0. : 180.]
!   Lon      - Longitude (deg)  [0. : 360.]
!
! !Team-Unique Header:
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_REGNUM_INVALID
!  Modules Used:
!   MSG          - Sbr. WriteReport
!
! !Revision History:
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, INTENT ( IN) :: RegionNum
      INTEGER, INTENT ( IN) :: Grid_Idx
      REAL, INTENT (OUT)    :: Lat, Lon
!
      INTEGER         :: LatReg, NorthPole, NumReg_TARGET
      INTEGER         :: NumBands_TARGET
      INTEGER         :: NumReg_Global_TARGET
      REAL, PARAMETER :: Padding = 0.0001
      REAL            :: RegHgt_TARGET, RegWidth_TARGET
      REAL, PARAMETER :: MinLat = 0., MaxLat = 180.
      REAL, PARAMETER :: MinLong = 0., MaxLong = 360.
      INTEGER         :: Reg, SouthPole
!
      IF  ( Grid_Idx == Index_CERES05Deg) THEN
!     *** Calculations to find the Region Number
!         in the CERES nested grid system
!
          Reg = nested_number ( RegionNum)
          CALL get_nested_centroid ( Reg,                                 & ! I
                                          Lat, Lon)                         ! O
!
      ELSE
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "Region_LatLon()",                & ! I
                            StrVar = " occured with CERES Grid "//      & ! I
                                 "- All others are Unavailable",         & ! I
                            Ivar1 = Grid_Idx)
      END IF
!
      IF ( Lat < MinLat   .OR.   Lat > MaxLat) THEN
!     *** Colatitude out of range
!
          CALL WriteReport (MOAIO_E_FAILED_COLAT_INVALID,               & ! I
                            FuncName = "Region_LatLon()",           & ! I
                            StrVar = "occured at Latitude : ",          & ! I
                            Rvar1 = Lat)
      ELSE IF ( Lon < MinLong   .OR.   Lon > MaxLong) THEN
!     *** Longitude out of range
!
          CALL WriteReport (MOAIO_E_FAILED_LONG_INVALID,                & ! I
                            FuncName = "Region_LatLon()",           & ! I
                            StrVar = "occured at Longitude : ",         & ! I
                            Rvar1 = Lon)
      END IF
!
      END SUBROUTINE Region_LatLon
    
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_Reg_Interp (UnitNum1, UnitNum2,            & ! I
                                                    GridIdx, Hour,       & ! I
                                                            MOA, Stat,   & ! O
                                                             IReg)         ! I
!
!**************************************************************************
!
! !F90
!
! !Name:
!   MOA_NCDFRead_Reg_Interp
!
! !Description:
!   Routine ID -
!
!   Purpose - To read one region's worth of MOA data from 2 consective 6 hour
!             MOA files and return a successful/unsuccessful flag to calling
!             routine given the region of interest.
!
!
! !Input Parameters:
!   UnitNum1  - Input unit number for first MOA file
!   UnitNum2  - Input unit number for second MOA file
!   Hour      - Hour to interpolate to ( must be between the data date hours
!                                        of the two MOA files)
!   IReg      - MOA ( CERES Nested) region Number [0 : 44012] for grid index 5
!
! !Output Parameters:
!   MOA     - Record of MOA data
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Internal Routines Used:
!   LatLong_Reg
!   MOA_FileInterp_Hour
!   MOA_Interpolate_Reg
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf         - h5dopen_f, h5dread_f
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_READ
!
! !Revision History:
!  Version 3.0   May 22, 2019 - SCCR #
!                Thomas Caldwell, Thomas.E.Caldwell@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.6   October 29, 1999 - SCCR #172
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Code moved from RegridMOA Subsystem
!  Version 2.0   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Initial Version
!
! !END
!
!**************************************************************************
!
!
      INTEGER,         INTENT ( IN) :: UnitNum1, UnitNum2
      INTEGER,         INTENT ( IN) :: GridIdx
      INTEGER,         INTENT ( IN) :: IReg
      INTEGER,         INTENT ( IN) :: Hour
      TYPE (MOA_TYPE), INTENT (OUT) :: MOA
      INTEGER,         INTENT (OUT) :: Stat
!
      INTEGER         :: Ios, Int_Hour, lev, HourRead
      INTEGER         :: Hourly_Unitnum1, Hourly_Unitnum2 !, Nat_Reg
      REAL            :: Lat, Long
      REAL            :: Hourly_SKT, Hourly_T2m, Hourly_T10M  
      REAL            :: Hourly_Q2m, Hourly_Q10M  
      TYPE (MOA_TYPE) :: MOA1, MOA2

      LOGICAL :: Interp

!
! *** Initialize write success flag to indicate success
!
      Stat = OK
!
!
! *** Check file info and determine interpolation hour ( 1-3)
      
      CALL MOA_NCDF_FileInterp_Hour ( UnitNum1, UnitNum2, Hour,    & ! I
                                                    Stat, Int_Hour)
!
!
      Interp = .FALSE.

      IF (Int_Hour == 0) THEN     ! read zero hour of current file
       Hourly_Unitnum1 = UnitNum1
       HourRead = 0
      ELSEIF (Int_Hour > 0 .AND. Int_Hour < 3) THEN     ! interpolate between hours
       Hourly_Unitnum1 = UnitNum1
       HourRead = Int_Hour
       Interp = .TRUE.
      ELSEIF (Int_Hour == 3) THEN ! read first hour of next file
       Hourly_Unitnum1 = UnitNum2
       HourRead = 0
      ENDIF

! *** Read MOA file 1 for data
      CALL MOA_NCDFRead_Region (UnitNum1, GridIdx,           & ! I
                                                MOA1, Ios,   & ! O
                                                    Ireg)      ! I

      IF ( Ios /= OK) THEN
!     *** Read error
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_MOA_READ,                & ! I
                            FuncName = "NCDFRead_Reg_Interp()",      & ! I
                            StrVar = "occured at record number: ",  & ! I
                            Ivar1 = Ireg)                             ! I
          RETURN
      END IF
!

     IF (Interp) THEN
!
! *** Read second MOA file for interpolation
!
!
! *** Read MOA file 2 for data
      CALL MOA_NCDFRead_Region (UnitNum2, GridIdx,          & ! I
                                          MOA2, Ios,        & ! O
                                                   Ireg)      ! I
					    
      IF ( Ios /= OK) THEN
!     *** Read error
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_MOA_READ,                & ! I
                            FuncName = "MOARead_Reg_Interp()",      & ! I
                            StrVar = "occured at record number: ",  & ! I
                            Ivar1 = Ireg)                             ! I
          RETURN
      END IF
!

! *** INTERPOLATE MOA DATA
!
!
      CALL MOA_Interpolate_Reg ( Int_Hour, MOA1, MOA2,              & ! I
                                                       Stat, MOA)     ! O


     ELSE   ! No interpolation, pass first read into output
     
      MOA = MOA1

     ENDIF

!
! *** Check MOA profile for invalid values
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "TMPU", 		    & ! I
				Pres_MP, MOA%Temp_MP)     ! I/O
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "SPHU",              & ! I
			       Pres_MP, MOA%Humid_MP)     ! I/O
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "POZ3",               & ! I
			       Pres_MP, MOA%PO3)          ! I/O

     
! *** Get Hourly grid Data for lower hourly profile
!
      !
      ! GEOSIT to CERES05Deg grid conversion has been removed from this block of code
	 
	 CALL MOA_NCDFRead_Hourly_Region (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'TSKN', Ireg, HourRead,                & ! I
					              Ios, Hourly_SKT)                ! O

         CALL MOA_NCDFRead_Hourly_Region (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'T02M', Ireg, HourRead,                & ! I
					              Ios, Hourly_T2M)                ! O

         CALL MOA_NCDFRead_Hourly_Region (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'T10M', Ireg, HourRead,                & ! I
					              Ios, Hourly_T10M)               ! O

         CALL MOA_NCDFRead_Hourly_Region (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'Q02M', Ireg, HourRead,                & ! I
					              Ios, Hourly_Q2M)                ! O

         CALL MOA_NCDFRead_Hourly_Region (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'Q10M', Ireg, HourRead,                & ! I
					              Ios, Hourly_Q10M)               ! O

         MOA % SfcSknTemp = Hourly_SKT ! skin temp
         MOA % Temp_MP(1:2) = Hourly_T2M ! hourly 2m temp
         MOA % Temp_MP(3) = Hourly_T10M   ! hourly 10m temp 

         MOA % Humid_MP(1:2) = Hourly_Q2M ! hourly 2m sphu
         MOA % Humid_MP(3)   = Hourly_Q10M ! hourly 10m sphu  
!
!
      END SUBROUTINE MOA_NCDFRead_Reg_Interp
      
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_NCDFRead_LatLon_Interp (UnitNum1, UnitNum2,         & ! I
                                                    GridIdx, Hour,       & ! I
                                                    MOA, Stat,           & ! O
                                                             Lat,Long)     ! I
!
!**************************************************************************
!
! !F90
!
! !Name:
!   MOA_NCDFRead_Reg_Interp
!
! !Description:
!   Routine ID -
!
!   Purpose - To read one region's worth of MOA data from 2 consective 6 hour
!             MOA files and return a successful/unsuccessful flag to calling
!             routine given the region of interest.
!
!
! !Input Parameters:
!   UnitNum1  - Input unit number for first MOA file
!   UnitNum2  - Input unit number for second MOA file
!   Hour      - Hour to interpolate to ( must be between the data date hours
!                                        of the two MOA files)
!   IReg      - MOA ( CERES Nested) region Number [0 : 44012] for grid index 5
!
! !Output Parameters:
!   MOA     - Record of MOA data
!   Stat    - Flag indicating success of read
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Internal Routines Used:
!   LatLong_Reg
!   MOA_FileInterp_Hour
!   MOA_Interpolate_Reg
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf         - h5dopen_f, h5dread_f
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_READ
!
! !Revision History:
!  Version 3.0   May 22, 2019 - SCCR #
!                Thomas Caldwell, Thomas.E.Caldwell@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.6   October 29, 1999 - SCCR #172
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Code moved from RegridMOA Subsystem
!  Version 2.0   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Initial Version
!
! !END
!
!**************************************************************************
!
!
      INTEGER,         INTENT ( IN) :: UnitNum1, UnitNum2
      INTEGER,         INTENT ( IN) :: GridIdx
      REAL,            INTENT ( IN) :: Lat
      REAL,            INTENT ( IN) :: Long
      INTEGER,         INTENT ( IN) :: Hour
      TYPE (MOA_TYPE), INTENT (OUT) :: MOA
      INTEGER,         INTENT (OUT) :: Stat
!
      INTEGER         :: Ios, Int_Hour, lev, HourRead
      INTEGER         :: Hourly_Unitnum1, Hourly_Unitnum2
      REAL            :: Hourly_SKT, Hourly_T2m, Hourly_T10M  
      REAL            :: Hourly_Q2m, Hourly_Q10M  
      TYPE (MOA_TYPE) :: MOA1, MOA2

      LOGICAL :: Interp

!
! *** Initialize write success flag to indicate success
!
      Stat = OK
!
!
! *** Check file info and determine interpolation hour ( 1-3)
      
      CALL MOA_NCDF_FileInterp_Hour ( UnitNum1, UnitNum2, Hour,    & ! I
                                                    Stat, Int_Hour)
!
!
      Interp = .FALSE.

      IF (Int_Hour == 0) THEN     ! read zero hour of current file
       Hourly_Unitnum1 = UnitNum1
       HourRead = 0
      ELSEIF (Int_Hour > 0 .AND. Int_Hour < 3) THEN     ! interpolate between hours
       Hourly_Unitnum1 = UnitNum1
       HourRead = Int_Hour
       Interp = .TRUE.
      ELSEIF (Int_Hour == 3) THEN ! read first hour of next file
       Hourly_Unitnum1 = UnitNum2
       HourRead = 0
      ENDIF



! *** Read MOA file 1 for data
      CALL MOA_NCDFRead_LatLon (UnitNum1, GridIdx,    & ! I
                                 MOA1, Ios,           & ! O
                                        Lat, Long)      ! I

      IF ( Ios /= OK) THEN
!     *** Read error
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_MOA_READ,                & ! I
                            FuncName = "NCDFRead_LatLon_Interp()")                             ! I
          RETURN
      END IF
!

     IF (Interp) THEN

!
! *** Read second MOA file for interpolation
!
!
! *** Read MOA file 2 for data
      CALL MOA_NCDFRead_LatLon (UnitNum2, GridIdx,   & ! I
                                   MOA2, Ios,        & ! O
                                       Lat, Long)      ! I
					    
      IF ( Ios /= OK) THEN
!     *** Read error
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_MOA_READ,                & ! I
                            FuncName = "MOARead_Reg_Interp()")                             ! I
          RETURN
      END IF
!
!
!
! *** INTERPOLATE MOA DATA
!
!
      CALL MOA_Interpolate_Reg ( Int_Hour, MOA1, MOA2,              & ! I
                                                       Stat, MOA)     ! O

     ELSE   ! No interpolation, pass first read into output
     
      MOA = MOA1

     ENDIF

!
! *** Check MOA profile for invalid values
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "TMPU", 		    & ! I
				Pres_MP, MOA%Temp_MP)     ! I/O
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "SPHU",              & ! I
			       Pres_MP, MOA%Humid_MP)     ! I/O
!
      CALL MOA_Profile_Check (MOA%SfcPres, NL, "POZ3",               & ! I
			       Pres_MP, MOA%PO3)          ! I/O

     
! *** Get Hourly grid Data for lower hourly profile
!
!
      ! GEOSIT to CERES05Deg grid conversion has been removed from this block of code

         CALL MOA_NCDFRead_Hourly_LatLon (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'TSKN', Lat, Long, HourRead,           & ! I
					              Ios, Hourly_SKT)                ! O

         CALL MOA_NCDFRead_Hourly_LatLon (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'T02M', Lat, Long, HourRead,           & ! I
					              Ios, Hourly_T2M)                ! O

         CALL MOA_NCDFRead_Hourly_LatLon (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'T10M', Lat, Long, HourRead,           & ! I
					              Ios, Hourly_T10M)               ! O

         CALL MOA_NCDFRead_Hourly_LatLon (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'Q02M',  Lat, Long, HourRead,          & ! I
					              Ios, Hourly_Q2M)                ! O

         CALL MOA_NCDFRead_Hourly_LatLon (Hourly_Unitnum1, Index_CERES05Deg,        & ! I
                                             'Q10M', Lat, Long, HourRead,           & ! I
					              Ios, Hourly_Q10M)               ! O

         MOA % SfcSknTemp = Hourly_SKT ! skin temp

         MOA % Temp_MP(1:2) = Hourly_T2M ! hourly 2m temp
         MOA % Temp_MP(3) = Hourly_T10M   ! hourly 10m temp 
!
         MOA % Humid_MP(1:2) = Hourly_Q2M ! hourly 2m sphu
         MOA % Humid_MP(3)   = Hourly_Q10M ! hourly 10m sphu  
!
!
      END SUBROUTINE MOA_NCDFRead_LatLon_Interp
!
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - -

      SUBROUTINE MOA_NCDF_FileInterp_Hour ( UnitNum1, UnitNum2, Hour_In,    &
                                                   Stat, Hour_Out)
!
!**************************************************************************
!
! !F90
!
! !Name:
!    MOA_NCDF_FileInterp_Hour
!
! !Description:
!   Routine ID -
!
!   Purpose - To perform checks and calculate the interpolation hour
!             between the two files and return a successful/unsuccessful
!             flag to calling routine
!
!
! !Input Parameters:
!   UnitNum1  - Input unit number for first MOA file
!   UnitNum2  - Input unit number for second MOA file
!   Hour_In   - Hour to interpolate to ( must be between the data date hours
!                                        of the two MOA files)
!
! !Output Parameters:
!   Hour_Out  - Hour used to reference data from file (0 to 6)
!   Stat      - Flag indicating success
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   netcdf       - nf90_inq_path
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_WRITE,
!                                   MOAIO_W_FAILED_GRIDIDX_INVALID,
!                                   MOAIO_W_FAILED_INTERP_INVALID,
!                                   MOAIO_W_FAILED_HOUR_INVALID
!
! !Revision History:
!  Version 3.0   May 22, 2019 - SCCR #
!                Thomas Caldwell, Thomas.E.Caldwell@nasa.gov
!                   Based on preexisting MOA interface code
!  Version 2.6   October 29, 1999 - SCCR #172
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Code moved from RegridMOA Subsystem
!  Version 2.0   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, INTENT (  IN) :: UnitNum1, UnitNum2, Hour_In
      INTEGER, INTENT ( OUT) :: Stat
      INTEGER, INTENT ( OUT) :: Hour_Out
!
      INTEGER         :: Hour1, Hour2, Ios
!
      TYPE (MOA_HEADER_TYPE) :: MOA_Header1, MOA_Header2
!
      call MOA_Header_Get ( UnitNum1, MOA_Header1 )
      call MOA_Header_Get ( UnitNum2, MOA_Header2 )
      
      CALL MOA_FileInterp_Check ( MOA_Header1, MOA_Header2, Hour_In,       &
                                                             Ios)
      IF ( Ios /= OK) THEN
          
	  Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_MOA_INTERPOLATE,        & ! I
                            FuncName = "NCDF_FileInterp_Hour()")      ! I
      END IF
!
!
! *** Initialize IO status variable
      Stat = OK
!
! *** Retrieve filenames to get data hours
!
      read ( MOA_Header1%DataDate(12:13),'(i2)') Hour1
      read ( MOA_Header2%DataDate(12:13),'(i2)') Hour2

!
! *** Calculate Interpolation hour ( 0 - 3)
!
      IF ( Hour_In == Hour1) THEN
          Hour_Out = 0
      ELSE IF ( Hour_In == Hour2 .OR. Hour_In == 24) THEN
          Hour_Out = 3
      ELSE
          Hour_Out = Hour_In
          DO WHILE ( Hour_Out >= 3)
              Hour_Out = Hour_Out - 3
          END DO
      END IF
!
!
      END SUBROUTINE MOA_NCDF_FileInterp_Hour
      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_Interpolate_Reg ( IHr, MOA1, MOA2,                  & ! I
                                                        Stat, MOA_Int)     ! O
!
!**************************************************************************
!
! !F90
!  Name: MOA_Interpolate_Reg
!
! !Description:
!   Routine ID -
!
!   Purpose: Temporally Interpolate between two MOA structures
!
! !Input Parameters:
!   Hour       - Index Number for the MOA Grid
!   MOA1       - A MOA region for a file
!   MOA2       - A MOA region for the next file
!
! !Output Parameters:
!   MOA_Int - An intepolated MOA file for the hour of interest
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types:      MOA_TYPE
!   Parameters: NL, NW
!
!  Internal Routines Used:
!   MOA_Read
!
!  Modules Used:
!   ceres_defaults - Parameters: REAL4_DFLT
!   ceres_status   - Parameters: OK
!   MSG            - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_E_FAILED_GRIDIDX_INVALID
!
! !Revision History:
!  Version 2.0   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER        , INTENT ( IN)  :: IHr
      TYPE (MOA_TYPE), INTENT ( IN)  :: MOA1
      TYPE (MOA_TYPE), INTENT ( IN)  :: MOA2
      INTEGER        , INTENT (OUT)  :: Stat
      TYPE (MOA_TYPE), INTENT (OUT)  :: MOA_Int
!
      INTEGER :: I
!
!
      Stat = OK
!
! *** Region Records ( Flags)
!
      IF ( MOA1 % RegIdx /= MOA2 % RegIdx) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_GRIDIDX_INVALID,         & ! I
                            FuncName = "MOARead_Reg_Interp()",      & ! I
                            StrVar = "Grid Regions are not the same") ! I
          RETURN
      END IF

      MOA_Int % RegIdx = MOA1 % RegIdx
!
!
! *** Region Records ( data / integers)
      SELECT CASE ( IHr)
          CASE ( 0:1)
              MOA_Int % AirMassIdx = MOA1%AirMassIdx
          CASE ( 2:3)
              MOA_Int % AirMassIdx = MOA2%AirMassIdx
      END SELECT

! *** Region Records ( data / reals)
!
      MOA_Int%SfcPres = MOA_Rval_Interp (IHr, MOA1%SfcPres, MOA2%SfcPres,3)
      MOA_Int%SfcGeoPt = MOA_Rval_Interp (IHr, MOA1%SfcGeoPt, MOA2%SfcGeoPt,3)
      MOA_Int%SfcSknTemp = MOA_Rval_Interp (IHr, MOA1%SfcSknTemp,              &
                                                 MOA2%SfcSknTemp,3)
      MOA_Int%SfcUwind = MOA_Rval_Interp( IHr, MOA1%SfcUwind, MOA2%SfcUwind,3)
      MOA_Int%SfcVwind = MOA_Rval_Interp( IHr, MOA1%SfcVwind, MOA2%SfcVwind,3)
      DO I = 1, NL
!     *** Temperature, Humidity & Ozone Profiles
          MOA_Int%Temp_MP(I) = MOA_Rval_Interp( IHr, MOA1%Temp_MP(I),          &
                                                     MOA2%Temp_MP(I),3)
          MOA_Int%Humid_MP(I) = MOA_Rval_Interp( IHr, MOA1%Humid_MP(I),        &
                                                      MOA2%Humid_MP(I),3)
      END DO
!
      DO I = 1, NW
!     *** U and V vector Wind Profiles
          MOA_Int%UWind_MP(I) = MOA_Rval_Interp( IHr, MOA1%UWind_MP(I),        &
                                                      MOA2%UWind_MP(I),3)
          MOA_Int%VWind_MP(I) = MOA_Rval_Interp( IHr, MOA1%VWind_MP(I),        &
                                                      MOA2%VWind_MP(I),3)
      END DO

      MOA_Int%TropHt = MOA_Rval_Interp( IHr, MOA1%TropHt, MOA2%TropHt,3)
      MOA_Int%Precip_H2O = MOA_Rval_Interp( IHr, MOA1%Precip_H2O,              &
                                                 MOA2%Precip_H2O,3)
      MOA_Int%RelHum_ColAvg = MOA_Rval_Interp( IHr, MOA1%RelHum_ColAvg,        &
                                                    MOA2%RelHum_ColAvg,3)
      DO I = 1, NL
!     *** Ozone Profile
          MOA_Int%PO3(I) = MOA_Rval_Interp( IHr, MOA1%PO3(I), MOA2%PO3(I),3)
      END DO
      MOA_Int%CO3 = MOA_Rval_Interp( IHr, MOA1%CO3, MOA2%CO3,3)
      MOA_Int%CA_OptDepth = MOA_Rval_Interp( IHr, MOA1%CA_OptDepth,            &
                                                  MOA2%CA_OptDepth,3)
!
      MOA_Int%Cld_Frac = MOA_Rval_Interp( IHr, MOA1%Cld_Frac, MOA2%Cld_Frac,3)
!
      MOA_Int%UTRH = MOA_Rval_Interp( IHr, MOA1%UTRH, MOA2%UTRH,3)
      MOA_Int%EIS = MOA_Rval_Interp( IHr, MOA1%EIS, MOA2%EIS,3)
      MOA_Int%PotTemp = MOA_Rval_Interp( IHr, MOA1%PotTemp, MOA2%PotTemp,3)
      MOA_Int%DailyTwoMeterTemp = MOA_Rval_Interp( IHr, MOA1%DailyTwoMeterTemp, &
                                                       MOA2%DailyTwoMeterTemp,3)

!
      END SUBROUTINE MOA_Interpolate_Reg
!
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_FileInterp_Check ( Header1, Header2, Hour_In,    &
                                                                Stat)
!
!**************************************************************************
!
! !F90
!
! !Name:
!    MOA_FileInterp_Check
!
! !Description:
!   Routine ID -
!
!   Purpose - To perform checks and calculate the interpolation hour
!             between the two files and return a successful/unsuccessful
!             flag to calling routine
!
!
! !Input Parameters:
!   Ireg    - Region number for which to write MOA data.
!   MOA     - Record of MOA data
!   UnitNum - Output unit number for MOA file
!
! !Output Parameters:
!   Stat    - Flag indicating success of write
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   ceres_time   - Parameters: day_of_year
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_MOA_WRITE,
!                                   MOAIO_W_FAILED_GRIDIDX_INVALID,
!                                   MOAIO_W_FAILED_INTERP_INVALID,
!                                   MOAIO_W_FAILED_HOUR_INVALID
!
! !Revision History:
!  Version 2.6   October 29, 1999 - SCCR #172
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Code moved from RegridMOA Subsystem
!  Version 2.0   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!**************************************************************************
!
!
      TYPE (MOA_HEADER_TYPE), INTENT ( IN) :: Header1
      TYPE (MOA_HEADER_TYPE), INTENT ( IN) :: Header2
      INTEGER, INTENT (  IN) :: Hour_In
      INTEGER, INTENT ( OUT) :: Stat
!
      INTEGER         :: Hour1, Hour2
      INTEGER         :: Year1, Year2, Month1, Month2, Day1, Day2
      INTEGER         :: int_check, Jdate1, Jdate2
!
! *** Initialize IO status variable
      Stat = OK
!
      IF ( Header1%GridIdx_Output /= Index_CERES05Deg) THEN
!     *** GridIndex error for file 1
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,         & ! I
                            FuncName = "MOA_FileInterp_Check()",    & ! I
                            StrVar = "Interpolation attempted " //  & ! I
                                     "on non Edition5 MOA file.")     ! I
      END IF
      IF ( Header2%GridIdx_Output /= Index_CERES05Deg) THEN
!     *** GridIndex error for file 2
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,         & ! I
                            FuncName = "MOA_FileInterp_Check()",    & ! I
                            StrVar = "Interpolation attempted " //  & ! I
                                     "on non Edition5 MOA file.")     ! I
      END IF
!
      READ ( Header1%DataDate (  1: 4), '(i4)') Year1
      READ ( Header2%DataDate (  1: 4), '(i4)') Year2
      READ ( Header1%DataDate (  6: 7), '(i2)') Month1
      READ ( Header2%DataDate (  6: 7), '(i2)') Month2
      READ ( Header1%DataDate (  9:10), '(i2)') Day1
      READ ( Header2%DataDate (  9:10), '(i2)') Day2
!
      READ ( Header1%DataDate ( 12:13), '(i2)') Hour1
      READ ( Header2%DataDate ( 12:13), '(i2)') Hour2
!
      Jdate1 = day_of_year (Year1, Month1, Day1)
      Jdate2 = day_of_year (Year2, Month2, Day2)
!
! *** Check to insure files are consecutive
      int_check = BAD
      IF      ( Hour1 ==  0 .AND. Hour2 ==  3) THEN
!     *** Check for consecutive hours
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 ==  3 .AND. Hour2 ==  6) THEN
!     *** Check for consecutive hours
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 ==  6 .AND. Hour2 == 9) THEN
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 ==  9 .AND. Hour2 == 12) THEN
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 == 12 .AND. Hour2 == 15) THEN
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 == 15 .AND. Hour2 == 18) THEN
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 == 18 .AND. Hour2 == 21) THEN
          IF ( Year1 == Year2 .AND. Jdate1 == Jdate2) THEN
!         *** Check for same year and day of data
              int_check = OK
          END IF
      ELSE IF ( Hour1 == 21 .AND. Hour2 ==  0) THEN
!         *** Check for same year and next day of data
          IF ( Year1 == Year2 .AND. Jdate1 + 1 == Jdate2) THEN
              int_check = OK
          ELSE IF ( Year2 == Year1 + 1) THEN
!         *** End of Year check
              IF ( Month1 == 12 .AND. Day1 == 31 .AND.              &
                   Month2 ==  1 .AND. Day2 ==  1) THEN
                  int_check = OK
              END IF
          END IF
      END IF
      IF ( int_check == BAD) THEN
!     *** Hours for input files are not 3 hour from each other
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_INTERP_INVALID,          & ! I
                            FuncName = "MOA_FileInterp_Check()",     & ! I
                            StrVar = "Hours are not consecutive")     ! I
      END IF
!
! *** Check to insure hour is between file hours
      IF ( Hour1 == 21 .AND. Hour2 == 0) THEN
          IF ( Hour_In < 21 .OR. Hour_In > 24) THEN
              Stat = BAD
	      CALL WriteReport (MOAIO_W_FAILED_HOUR_INVALID,        & ! I
                            FuncName = "MOA_FileInterp_Check()",     & ! I
                            StrVar = "Hour is not between files")     ! I
          END IF
      ELSE IF ( Hour_In < Hour1 .OR. Hour_In > Hour2) THEN
!     *** Interp Hour is not between the 3 hours
          Stat = BAD
          CALL WriteReport (MOAIO_W_FAILED_HOUR_INVALID,            & ! I
                            FuncName = "MOA_FileInterp_Check()",     & ! I
                            StrVar = "Hour is not between files")     ! I
      END IF
!
!
      END SUBROUTINE MOA_FileInterp_Check
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_Profile_Check (SurfPres, Num_Levels, Flag, Pres_Out,  & ! I
                                             			Dat_Out)   ! I/O
!
!*****************************************************************************
!
! !F90
!
! !Name:
!  MOA_Profile_Check
!
! !Description:
!   Routine ID -
!
!   Purpose - To check for default data above the surface level in the vertical
!	      profiles and fill the missing levels by interpolation
!
!
! !Input Parameters:
!   SurfPres - Surface pressure of the current region
!   Num_Levels - Number of levels or profile data
!   Flag - Indicates the type of profile data
!   Pres_Out - Pressure levels of profile data
!
! !Output Parameters:
!   Dat_Out     - Revised profile data
!
! !Team-Unique Header:
!
!  Modules Used:
!   None
!
! !Revision History:
!   Version 3.0   October 29, 1999
!                 Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!
! !END
!
!*****************************************************************************
!
!
      REAL,      INTENT ( INOUT) :: Dat_Out ( :)
      CHARACTER, INTENT ( IN)    :: Flag * 4
      REAL,      INTENT ( IN)    :: SurfPres
      REAL,      INTENT ( INOUT) :: Pres_Out (:)
      INTEGER,   INTENT ( IN)    :: Num_Levels
!
      INTEGER :: nlev, I, J
!
    IF ( FLAG == "TMPU" .OR. FLAG == "SPHU" .OR. FLAG == "POZ3") THEN
      DO I = 4, Num_Levels
!     *** Rebuild any profile data on the Num_Levels levels
          
	  IF ( Pres_Out (I) < SurfPres) THEN
             IF ( Dat_Out(I) == Default_MOA) THEN
!            *** For Temperature values default above surface
                 J = I
                 DO WHILE ( Dat_Out( J) == Default_MOA .AND.               &
                            J < Num_Levels)
                     J = J + 1
                 END DO
                 IF ( Dat_Out ( 1) /= Default_MOA .AND.                   &
                      Dat_Out ( J) /= Default_MOA) THEN
                  IF ( Flag == "TMPU") THEN
!                *** Interpolate the Temperature Data
                     Dat_Out( I) = g_eln_inter (Dat_Out ( 1),        &
                                                     Dat_Out ( J),        &
                                                     SurfPres,             &
                                                     Pres_Out ( J),  &
                                                     Pres_Out ( I) )
     		  ENDIF
                  IF ( Flag == "SPHU" .OR. Flag == "POZ3") THEN
!                *** Interpolate the Humidity Data
                     Dat_Out( I) = e_log_inter (Dat_Out ( 1),      &
                                                      Dat_Out ( J),      &
                                                      SurfPres,            &
                                                      Pres_Out ( J), &
                                                      Pres_Out ( I) )
		  ENDIF

                 END IF
             END IF
          END IF
      END DO
    ENDIF
!
!
      END SUBROUTINE MOA_Profile_Check
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE CERES_to_MOA ( RegionNum, Grid_Idx,                & ! I
                                                     MOA_Reg)         ! O
!
!**************************************************************************
!
! !F90
!  Name: CERES_to_MOA
!
! !Description:
!   Routine ID -
!
!   Purpose: Given a CERES Region Number, get colatitudinal and
!            longitudinal coordinates. This is then passed to
!            LatLong_Reg to determine the TARGET region number.
!
! !Input Parameters:
!   RegionNum - CERES Region number
!   Grid_Idx  - Flag indicating which grid current MOA Data is using
!
! !Output Parameters:
!   MOA_Reg - MOA Region Number [ 1 : Number of Global Regions ]
!
! !Team-Unique Header:
!
!  Modules Used:
!    reference_grid - Sbr. get_centroid
!
!  Internal Routines Used:
!    Routines: LatLong_Reg
!
!  External Files:
!  NONE
!
! !Revision History:
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER, INTENT ( IN) :: Grid_Idx
      INTEGER, INTENT (OUT) :: MOA_Reg
      INTEGER, INTENT ( IN) :: RegionNum
!
      REAL    :: Lat, Long
!
! *** Get center colatitudinal and longitudinal coordinates of CERES region
      CALL get_centroid ( RegionNum,                                 & ! I
                                     Lat, Long)                        ! O
!
! *** Determine MOA region number
      CALL LatLong_Reg ( Lat, Long, Grid_Idx,                        & ! I
                                              MOA_Reg)                 ! O
!
!
      END SUBROUTINE CERES_to_MOA
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE LatLong_Reg (Lat, Long, Grid_Idx,                   & ! I
                                                   MOA_Reg)            ! O
!
!**************************************************************************
!
! !F90
!  Name: LatLong_Reg
!
! !Description:
!   Routine ID -
!
!   Purpose : Given latitudinal and longitudinal coordinates,
!             determine the TARGET region number.
!
! !Input Parameters:
!   Grid_Idx  - Flag indicating which grid current MOA Data is using
!   Lat       - Latitude (deg) [0. : 180.]
!   Long      - Longitude (deg)  [0. : 360.]
!
! !Output Parameters:
!   MOA_Reg - Region number
!
! !Team-Unique Header:
!
!  Modules Used:
!   MSG          - Sbr. WriteReport
!
! !Key Local Parameters:
!   BegRegNum            - Number of first TARGET region (longitude=0)
!                          contained within a colatitudinal band
!   IColat               - Index of colatitudinal band
!   MaxLat               - Maximum colatitudinal value
!   MaxLong              - Maximum longitudinal value
!   MinLat               - Minimum colatitudinal value
!   MinLong              - Minimum longitudinal value
!   NumBands_TARGET      - Number of colatitudinal bands on the global
!   NumReg_TARGET        - Number of TARGET regions contained within a
!                          colatudinal band
!   NumReg_Global_TARGET - Number of TARGET regions on the globe
!   Padding              - Value added to longitude coordinate and subtracted
!                          from colatitude coordinate to ensure that the
!                          Southern and Western boundaries are included in
!                          a region, and that the Northern and Eastern
!                          boundaries are not, i.e., if a coordinate is on
!                          the border of two regions, the padding of the
!                          coordinate value will make it fall
!                          into the correct region.
!   RegHgt_TARGET        - Size of the TARGET region (deg)
!   Shift                - A value used to shift the region to center the data
!                          point
!
! !Error Handling:
!   1. Exit via error message utility if a latitudinal or a longitudinal
!      coordinate is out of range.
!   2. Exit via error message utility if resulting region number exceeds
!      NumReg_Global.
!   3. Exit via error message utility if Grid_Idx is incorrect or
!      unavailiable
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_E_FAILED_COLAT_INVALID,
!                                   MOAIO_E_FAILED_GRIDIDX_INVALID
!                                   MOAIO_E_FAILED_LONG_INVALID,
!                                   MOAIO_E_FAILED_REGNUM_INVALID
!
! !Revision History:
!  Version 2.3   June 23, 1999  -  SCCR# 148
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                1. Added code to handle CERES Nested 1x1-deg Grid
!  Version 2.2   January 15, 1998
!                Edward A. Kizer, SAIC, e.kizer@larc.nasa.gov
!                1. Change in Subroutine LatLong_Reg to correct variable type.
!                   DelLong was declared as an INTEGER which caused rounding
!                   of the number needed.  DelLong is now declared as a REAL
!  Version 2.0   May 23, 1997
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!  Version 1.0   5/30/95
!                Lisa Coleman, LESC
!                Initial version of code
!
! !END
!
!**************************************************************************
!
!
     REAL,    INTENT ( IN) :: Lat, Long
     INTEGER, INTENT ( IN) :: Grid_Idx
     INTEGER, INTENT (OUT) :: MOA_Reg
!
     INTEGER, ALLOCATABLE :: BegReg_TARGET ( :)
     INTEGER              :: I, IColat
     REAL                 :: DelLong
     REAL, PARAMETER      :: MinLat = 0., MaxLat = 180.
     REAL, PARAMETER      :: MinLong = 0., MaxLong = 360.
     INTEGER              :: nested_num, NorthPole
     INTEGER              :: NumBands_TARGET, NumReg_Global_TARGET
     INTEGER              :: NumReg_TARGET
     REAL, PARAMETER      :: Padding = 0.0001
     REAL                 :: RegHgt_TARGET
     REAL                 :: Shift
     INTEGER              :: SouthPole
!
! *** Verify that input latitudinal and longitudinal coordinates are
!     within range.
!
      IF ( Lat < MinLat   .OR.   Lat > MaxLat) THEN
!     *** Colatitude out of range
!
          CALL WriteReport (MOAIO_E_FAILED_COLAT_INVALID,               & ! I
                            FuncName = "LatLong_Reg()",                 & ! I
                            StrVar = "occured at Latitude : ",          & ! I
                            Rvar1 = Lat)
      ELSE IF ( Long < MinLong   .OR.   Long > MaxLong) THEN
!     *** Longitude out of range
!
          CALL WriteReport (MOAIO_E_FAILED_LONG_INVALID,                & ! I
                            FuncName = "LatLong_Reg()",                 & ! I
                            StrVar = "occured at Longitude : ",         & ! I
                            Rvar1 = Long)
      END IF
!
! *** Determine which grid system to use.
!
      IF ( Grid_Idx == Index_CERESNESTED .OR. Grid_Idx == Index_CERES05Deg) THEN
!     *** Calculations to find the Region Number
!         in the CERES nested grid system
!
          nested_num = nested_region_number ( Lat, Long)
          MOA_Reg = sequence_number ( nested_num)
          RETURN
!
      ELSE
!     *** Index Number is Something other than what we can Produce
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,             & ! I
                            FuncName = "LatLong_Reg()",                 & ! I
                            StrVar = " occured with Grid_Index :%i3",   & ! I
                            IVar1 = Grid_Idx)                             ! I
      END IF
!
!
      END SUBROUTINE LatLong_Reg
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      FUNCTION MOA_Rval_Interp ( Hour, MOA_Val1, MOA_Val2, Increment)      & ! I
                                              RESULT (MOA_ValOut)            ! O
!
!**************************************************************************
!
! !F90
!  Name: MOA_Rval_Interp
!
! !Description:
!   Routine ID -
!
!   Purpose: Perform the interpolation of all real values within the MOA
!            structure
!
! !Input Parameters:
!   Hour       - Index Number for the MOA Grid
!   MOA_Val1   - A MOA region for a file
!   MOA_Val2   - A MOA region for the next file
!
! !Output Parameters:
!   MOA_ValOut - An intepolated MOA file for the hour of interest
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types:      MOA_TYPE
!   Parameters: NL, NW
!
!  Internal Routines Used:
!   MOA_Read
!
!  Modules Used:
!   ceres_defaults - Parameters: REAL4_DFLT
!   ceres_status   - Parameters: OK
!   MSG            - Sbr. WriteReport
!
!  External Files:
!   NONE
!
! !Revision History:
!  Version 2.0   June 23, 1999  -  SCCR# 148
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!**************************************************************************
!
!
      INTEGER :: Hour
      REAL    :: MOA_Val1, MOA_Val2
      INTEGER :: Increment
      REAL    :: MOA_ValOut
!
      REAL, PARAMETER :: Default_MOA = REAL4_DFLT
      REAL            :: Slope
!
!
      IF ( Hour > Increment) THEN
          CALL WriteReport (MOAIO_W_FAILED_HOUR_INVALID,         & ! I
                            FuncName = "MOA_Rval_Interp()",      & ! I
                            StrVar = "Hour Invalid") ! I
          RETURN
      END IF
!
!
      IF ( MOA_Val1 == Default_MOA .OR. MOA_Val2 == Default_MOA ) THEN
          MOA_ValOut = Default_MOA
      ELSE
          Slope = (MOA_Val2 - MOA_Val1) / Increment
          MOA_ValOut = Slope * (Hour) + MOA_Val1
      END IF
!
!
      END FUNCTION MOA_Rval_Interp
!
! - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - -
!
      SUBROUTINE MOA_Check ( MOA, Stat)
!
!**************************************************************************
!
! !F90
!
! !Name:
!    MOA_Check
!
! !Description:
!   Routine ID -
!
!   Purpose - To perform checks of the MOA structure for invalid values
!
!
! !Input Parameters:
!   MOA     - Record of MOA data
!
! !Output Parameters:
!   Stat    - Flag indicating success of write
!
! !Team-Unique Header:
!
!  Internal Module Variables/Types Used:
!   Types: MOA_TYPE
!
!  Modules Used:
!   ceres_status - Parameters: BAD, OK
!   MSG          - Sbr. WriteReport
!
!  External Files:
!   PGS_MOAIO_26500.f - Parameters: MOAIO_W_FAILED_GRIDIDX_INVALID,
!
! !Revision History:
!  Version 3.0   April 11, 2001  -  SCCR#
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!**************************************************************************
!
!
      TYPE (MOA_TYPE), INTENT ( IN) :: MOA
      INTEGER, INTENT ( OUT) :: Stat
!
      INTEGER :: I
!
! *** Initialize IO status variable
      Stat = OK
!
      IF ( MOA%RegIdx < RegIdx_Min .OR. MOA%RegIdx > RegIdx_Max) THEN
!     *** Out of range error
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "RegIdx Value out of allowable range.")    ! I
      END IF
!
      IF ( MOA%SfcPres < SfcPres_Min .OR. MOA%SfcPres > SfcPres_Max) THEN
!     *** Out of range error
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "SfcPres Value out of allowable range.")    ! I
      END IF
!
      IF ( MOA%SfcGeoPt < SfcGeoPt_Min .OR. MOA%SfcGeoPt > SfcGeoPt_Max) THEN
!     *** Out of range error
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "SfcGeoPt Value out of allowable range.")    ! I
      END IF
!
      IF ( MOA%SfcSknTemp < MinSkinTemp .OR. MOA%SfcSknTemp > MaxSkinTemp) THEN
!     *** Out of range error
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "SfcSknTemp Value out of allowable range.")    ! I
      END IF
!
      IF ( MOA%SfcUwind < SfcUwind_Min .OR. MOA%SfcUwind > SfcUwind_Max) THEN
!     *** Out of range error
         Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "SfcUwind Value out of allowable range.")    ! I
      END IF
!
      IF ( MOA%SfcVwind < SfcVwind_Min .OR. MOA%SfcVwind > SfcVwind_Max) THEN
!     *** Out of range error
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "SfcVwind Value out of allowable range.")    ! I
      END IF
!
      DO I = 1, NL
      IF ( MOA%Temp_MP(I) < Temp_MP_Min .OR. MOA%Temp_MP(I) > Temp_MP_Max) THEN
!     *** Out of range error
	IF ( MOA%Temp_MP(I) /= Default_MOA) THEN

          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "Temp_MP Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%Humid_MP(I) < Humid_MP_Min .OR. MOA%Humid_MP(I) > Humid_MP_Max) THEN
!     *** Out of range error
	
	IF ( MOA%Humid_MP(I) /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "Humid_MP Value out of allowable range.")    ! I
	ENDIF
      END IF
      ENDDO
!
      DO I = 1, NW
      IF ( MOA%UWind_MP(I) < UWind_MP_Min .OR. MOA%UWind_MP(I) > UWind_MP_Max) THEN
!     *** Out of range error
	IF ( MOA%UWind_MP(I) /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "UWind_MP Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%VWind_MP(I) < VWind_MP_Min .OR. MOA%VWind_MP(I) > VWind_MP_Max) THEN
!     *** Out of range error
	IF ( MOA%VWind_MP(I) /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "VWind_MP Value out of allowable range.")    ! I
	ENDIF
      END IF
      ENDDO
!
      IF ( MOA%TropHt < TropHt_Min .OR. MOA%TropHt > TropHt_Max) THEN
!     *** Out of range error
	IF ( MOA%TropHt /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "TropHt Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%AirMassIdx < AirMassIdx_Min .OR. MOA%AirMassIdx > AirMassIdx_Max) THEN
!     *** Out of range error
	IF ( MOA%AirMassIdx /= Default_MOA_INT) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "AirMassIdx Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%Precip_H2O < Precip_H2O_Min .OR. MOA%Precip_H2O > Precip_H2O_Max) THEN
!     *** Out of range error
	IF ( MOA%Precip_H2O /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "Precip_H2O Value out of allowable range.")    ! I
	ENDIF
      END IF
!
!
!
      IF ( MOA%RelHum_ColAvg < RelHum_ColAvg_Min .OR. MOA%RelHum_ColAvg > RelHum_ColAvg_Max) THEN
!     *** Out of range error
	IF ( MOA%RelHum_ColAvg /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "RelHum_ColAvg Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      DO I = 1, NL
       IF ( MOA%PO3(I) < PO3_Min .OR. MOA%PO3(I) > PO3_Max) THEN
!     *** Out of range error
	IF ( MOA%PO3(I) /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "PO3 Value out of allowable range.")    ! I
	ENDIF
       END IF
      ENDDO
!
      IF ( MOA%CO3 < CO3_Min .OR. MOA%CO3 > CO3_Max) THEN
!     *** Out of range error
	IF ( MOA%CO3 /= Default_MOA) THEN
          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "CO3 Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%CA_OptDepth < CA_OptDepth_Min .OR. MOA%CA_OptDepth > CA_OptDepth_Max) THEN
!     *** Out of range error
	IF ( MOA%CA_OptDepth /= Default_MOA) THEN

          Stat = BAD
          CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "CA_OptDepth Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%Cld_Frac < Cld_Frac_Min .OR. MOA%Cld_Frac > Cld_Frac_Max) THEN
!     *** Out of range error
	IF ( MOA%Cld_Frac /= Default_MOA) THEN
          Stat = BAD

	  CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "Cld_Frac Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%UTRH < UTRH_Min .OR. MOA%UTRH > UTRH_Max) THEN
!     *** Out of range error
	IF ( MOA%UTRH /= Default_MOA) THEN
          Stat = BAD

	  CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "UTRH Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      IF ( MOA%EIS < EIS_Min .OR. MOA%EIS > EIS_Max) THEN
!     *** Out of range error
	IF ( MOA%EIS /= Default_MOA) THEN
          Stat = BAD

	  CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "EIS Value out of allowable range.")    ! I
	ENDIF
      END IF
      IF ( MOA%PotTemp < PotTemp_Min .OR. MOA%PotTemp > PotTemp_Max) THEN
!     *** Out of range error
	IF ( MOA%PotTemp /= Default_MOA) THEN
          Stat = BAD

	  CALL WriteReport (MOAIO_E_FAILED_GRIDIDX_INVALID,              & ! I
                            FuncName = "MOA_Check()",    	       & ! I
                            StrVar = "PotTemp Value out of allowable range.")    ! I
	ENDIF
      END IF
!
      END SUBROUTINE MOA_Check

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Expand_Nested_to_Square_REAL_05Deg (Data_In, Data_Out)
!
!
!*************************************************************************
! !IDL
!
! !Name:
!    Expand_Nested_to_Square_REAL_05Deg
!
! !Description
!    Routine ID -
!
!    Purpose -
!
! !Input Parameters:
!    Data_In  - data array in
!
! !Output Parameters:
!    Data_Out - data array out
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    None
!
!  Modules Used:
!    None
!
! !Revision History:
!  Version 1.0  February 29, 1996
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!               Initial version of code
!
! !END
!
!*************************************************************************
!
!
!     INTEGER :: NumReg_CERES05Deg( NumBands_CERES05Deg) = (/   &
!				    (	1, zone =   1,   2 ),  &
!				    (  90, zone =   3,  20 ),  &
!				    ( 180, zone =  21,  40 ),  &
!				    ( 360, zone =  41,  90 ),  &
!				    ( 720, zone =  91, 270 ),  &  
!				    ( 360, zone = 271, 320 ),  &
!				    ( 180, zone = 321, 340 ),  &
!				    (  90, zone = 341, 358 ),  &
!				    (	1, zone = 359, 360 )	/)


      REAL, INTENT ( IN)  :: Data_In  ( NumReg_Global_CERESNESTED)
      REAL, INTENT ( OUT) :: Data_Out (NumReg_Global_CERES05Deg)

      REAL    :: real_buffer_sq  (NumReg_Global_CERES05Deg)
      INTEGER :: I, reg_out, zone, reg_in, reg



! *** replicate 1st and 2nd regions data
      real_buffer_sq (  1: 720) = Data_In( 1)
      real_buffer_sq (  721: 1440) = Data_In( 2)
       
       reg_in = 3
       reg_out = 1441
       DO zone = 1, 18
        DO reg = 1, 90
	real_buffer_sq(reg_out:reg_out+7)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 8
	ENDDO
       ENDDO
      
       DO zone = 1, 20
        DO reg = 1, 180
	real_buffer_sq(reg_out:reg_out+3)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 4
	ENDDO
       ENDDO

       DO zone = 1, 50
        DO reg = 1, 360
	real_buffer_sq(reg_out:reg_out+1)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 2
	ENDDO
       ENDDO

       DO zone = 1, 180
        DO reg = 1, 720
	real_buffer_sq(reg_out)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 1
	ENDDO
       ENDDO

       DO zone = 1, 50
        DO reg = 1, 360
	real_buffer_sq(reg_out:reg_out+1)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 2
	ENDDO
       ENDDO

       DO zone = 1, 20
        DO reg = 1, 180
	real_buffer_sq(reg_out:reg_out+3)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 4
	ENDDO
       ENDDO
  
       DO zone = 1, 18
        DO reg = 1, 90
	real_buffer_sq(reg_out:reg_out+7)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 8
	ENDDO
       ENDDO

! *** replicate last regions data
      real_buffer_sq (  257761: 258480) = Data_In( 176043)
      real_buffer_sq (  258481: 259200) = Data_In( 176044)

     Data_Out = real_buffer_sq


END SUBROUTINE Expand_Nested_to_Square_REAL_05Deg 

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Expand_Nested_to_Square_REAL_1Deg (Data_In, Data_Out)
!
!
!*************************************************************************
! !IDL
!
! !Name:
!    Expand_Nested_to_Square_REAL_1Deg
!
! !Description
!    Routine ID -
!
!    Purpose -
!
! !Input Parameters:
!    Data_In  - data array in
!
! !Output Parameters:
!    Data_Out - data array out
!
! !Team-Unique Header:
!
!  Key Local Parameters:
!    None
!
!  Modules Used:
!    None
!
! !Revision History:
!  Version 1.0  February 29, 1996
!               Edward A. Kizer, SAIC, e.a.kizer@larc.nasa.gov
!               Initial version of code
!
! !END
!
!*************************************************************************
!
!
!     INTEGER :: NumReg_CERES1Deg( NumBands_CERES1Deg) = (/   &
!                                   (   1, zone =   1,   1 ),  &
!                                    (  45, zone =   2,  10 ),  &
!                                    (  90, zone =  11,  20 ),  &
!                                    ( 180, zone =  21,  45 ),  &
!                                    ( 360, zone =  46, 135 ),  &
!                                    ( 180, zone = 136, 160 ),  &
!                                    (  90, zone = 161, 170 ),  &
!                                    (  45, zone = 171, 179 ),  &
!                                    (   1, zone = 180, 180 )    /)


      REAL, INTENT ( IN)  :: Data_In  ( NumReg_Global_CERESNESTED)
      REAL, INTENT ( OUT) :: Data_Out (NumReg_Global_CERES1Deg)

      REAL    :: real_buffer_sq  (NumReg_Global_CERES1Deg)
      INTEGER :: I, reg_out, zone, reg_in, reg



! *** replicate 1st and 2nd regions data
      real_buffer_sq (  1: 360) = Data_In( 1)
       
       reg_in = 2
       reg_out = 361
       DO zone = 2, 10
        DO reg = 1, 45
	real_buffer_sq(reg_out:reg_out+7)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 8
	ENDDO
       ENDDO
      
       DO zone = 11, 20
        DO reg = 1, 90
	real_buffer_sq(reg_out:reg_out+3)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 4
	ENDDO
       ENDDO

       DO zone = 21, 45
        DO reg = 1, 180
	real_buffer_sq(reg_out:reg_out+1)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 2
	ENDDO
       ENDDO

       DO zone = 46, 135
        DO reg = 1, 360
	real_buffer_sq(reg_out)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 1
	ENDDO
       ENDDO

       DO zone = 136, 160
        DO reg = 1, 180
	real_buffer_sq(reg_out:reg_out+1)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 2
	ENDDO
       ENDDO

       DO zone = 161, 170
        DO reg = 1, 90
	real_buffer_sq(reg_out:reg_out+3)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 4
	ENDDO
       ENDDO
  
       DO zone = 171, 179
        DO reg = 1, 45
	real_buffer_sq(reg_out:reg_out+7)= Data_In(reg_in)
	reg_in = reg_in + 1
	reg_out = reg_out + 8
	ENDDO
       ENDDO

! *** replicate last regions data
      real_buffer_sq ( 64441: 64800) = Data_In( 44012)

     Data_Out = real_buffer_sq


END SUBROUTINE Expand_Nested_to_Square_REAL_1Deg 

!
!
END MODULE MOA_IO_NESTED
 
 
 
