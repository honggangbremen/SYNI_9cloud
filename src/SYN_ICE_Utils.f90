MODULE RDICEAGEMAP
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   RDICEAGEMAP
!
! !Description:
!
!    Purpose - Read and process noaa ice maps
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
!    open_noaa_ice        - Open NOAA ice map file
!    read_noaa_ice        - Read NOAA ice map file 
!    reproj               - Reproject Polar Stereographic (SH &NH) into a 2160x1080 10'global map 
!    get_afwa_ice_age_map - Read and reproject AFWA ice age map into 360x180 1deg map
!    get_ice_age_map      - Drive processing of NOAA ice maps
!    deresolve            - Deresolve 2160x1080 10'global map into a 360x180 1deg map
!    find_10m_lon         - Compute equivalent longitude on 10m map
!    find_10m_lat         - Compute equivalent latitude on 10m map
!
!  Modules Used:
!   SYN_IO_Utils       - (SARBlib)  Used by Sbr. open_noaa_ice, read_noaa_ice
!   SynSARB_IO_Params  - (SARBlib)  Used by Sbr. open_noaa_ice, read_noaa_ice
!
! !Revision History:
!  Version 2.0   December 11, 2021 - SCCR #1878
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Added get_afwa_ice_age_map subroutine for processing August 2021 forward
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
USE ceres_defaults, ONLY : REAL4_DFLT
USE PCF, ONLY: GetFileName
USE SYN_IO_Utils
USE SynSARB_IO_Params
USE netcdf
!
IMPLICIT NONE
!
INTEGER ios
INTEGER iunit_north , iunit_south ,iunit_idx

INTEGER (KIND = 2),ALLOCATABLE, DIMENSION(:,:) :: jdx,idx
INTEGER (KIND = 2),ALLOCATABLE, DIMENSION(:,:) :: icanh,scenh,icash,scesh
INTEGER (KIND = 1),ALLOCATABLE, DIMENSION(:,:) :: iceage

INTEGER, PARAMETER :: BegRec_Sce = 2573
INTEGER, PARAMETER :: BegRec_IcA = 5657
!
INTEGER (KIND = 1), DIMENSION(360,180), SAVE :: iceage_1deg  
! OUTPUT BYTE MAP (0=FIRSTYR or None, 1=MultiYear)
!
CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE open_noaa_ice 
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   open_noaa_ice
!
! !Description:
!    Purpose - Open NOAA ice map file
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
!  Version 1.0    October 25, 2007 - SCCR #663
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Commented out record length and version number 
!                 from ice index file. They are invalid for 
!                 sequential access files
!
! !END
!
!*****************************************************************************
!
CHARACTER (LEN = 13) :: Name_Sbr = 'open_noaa_ice'
INTEGER :: VerID = 1
INTEGER :: RECL = 1024
!
CALL SARB_GenOpen (LID_ICE_North, Flag_File_Read,                  & ! I
                   Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                                  iunit_north,     & ! O
                                                     VerID, RECL)    ! I (Opt)
!
CALL SARB_GenOpen (LID_ICE_South, Flag_File_Read,                  & ! I
                   Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                                  iunit_south,     & ! O
                                                     VerID, RECL)    ! I (Opt)
!
CALL SARB_GenOpen (LID_ICE_IDX, Flag_File_Read,                 & ! I
                   Flag_File_Seq, Flag_File_Unformat, Name_Sbr, & ! I
                                                       iunit_idx) ! O
!                                                iunit_idx,     & ! O
!                                                 VerID, RECL)    ! I (Opt)
END SUBROUTINE open_noaa_ice
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE read_noaa_ice
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   read_noaa_ice
!
! !Description:
!    Purpose - Read NOAA ice map file
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
!
INTEGER i, IRec
CHARACTER*120 :: Name_Sbr = 'read_noaa_ice'

!These Are big arrays can't have them hanging around after the
!360x180 map is made !

 IF ( .NOT. ALLOCATED ( iceage )) ALLOCATE ( iceage(2160,1080) )
 IF ( .NOT. ALLOCATED ( idx )   ) ALLOCATE ( idx(2160,1080) )
 IF ( .NOT. ALLOCATED ( jdx )   ) ALLOCATE ( jdx(2160,1080) )
 IF ( .NOT. ALLOCATED ( icanh ) ) ALLOCATE ( icanh(512,512) )
 IF ( .NOT. ALLOCATED ( scenh ) ) ALLOCATE ( scenh(512,512) )
 IF ( .NOT. ALLOCATED ( icash ) ) ALLOCATE ( icash(512,512) )
 IF ( .NOT. ALLOCATED ( scesh ) ) ALLOCATE ( scesh(512,512) )

 READ(iunit_idx, IOSTAT = ios) jdx,idx
!
 IF (ios /= 0) &
   CALL SARB_RdWrtError (LID_ICE_IDX, ios, Name_Sbr) 
 CALL Fred_CloseFile (iunit_idx, 1)

 DO I = 1,512
   IRec = BegRec_IcA+(I-1)
   READ (iunit_north, REC = IRec, IOSTAT = Ios) icanh (1:512,I)
!   
   IF (Ios /= 0) &
    CALL SARB_RdWrtError (LID_ICE_North, Ios, Name_Sbr, IRec = IRec)
!
   IRec = BegRec_Sce+(I-1)
   READ (iunit_north, REC = IRec, IOSTAT = Ios) scenh (1:512,I)
!   
   IF (Ios /= 0) &
    CALL SARB_RdWrtError (LID_ICE_North, Ios, Name_Sbr, IRec = IRec)
 END DO
 
 CALL Fred_CloseFile (iunit_north, 1)

 DO I = 1,512
   IRec = BegRec_IcA+(I-1)
   READ (iunit_south, REC = BegRec_IcA+(I-1), IOSTAT = Ios) icash (1:512,I)
!
   IF (Ios /= 0) &
    CALL SARB_RdWrtError (LID_ICE_South, Ios, Name_Sbr, IRec = IRec)
!   
   IRec = BegRec_Sce+(I-1)
   READ (iunit_south, REC = BegRec_Sce+(I-1), IOSTAT = Ios) scesh (1:512,I)
!
   IF (Ios /= 0) &
    CALL SARB_RdWrtError (LID_ICE_South, Ios, Name_Sbr, IRec = IRec)   
 END DO
 
 CALL Fred_CloseFile (iunit_south, 1)

END SUBROUTINE read_noaa_ice
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE reproj(in_n,in_s,out)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   reproj
!
! !Description:
!    Purpose - Reproject Polar Stereographic (SH &NH) into a 2160x1080 10'global map
!
! 
! !Input Parameters:
!    in_n - North ice map
!    in_s - South ice map
!
! !Output Parameters:
!    out - Regridded 10' map
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

INTEGER (kind=2), DIMENSION(512,512) :: in_n,in_s
INTEGER (kind=1), DIMENSION(2160,1080) :: out
INTEGER i,j,ix,jx

 DO i = 1,540
   DO j = 1,2160
      jx=jdx(j,i)
      ix=idx(j,i)
      IF (jx /=0 .AND. ix /= 0) out(j,i)=in_n(jx,ix)
   END DO
 END DO
 DO i = 541,1080
   DO j = 1,2160
      jx=jdx(j,i)
      ix=idx(j,i)
      IF (jx /=0 .AND. ix /= 0) out(j,i)=in_s(jx,ix)
   END DO
 END DO

out(:,540:541) = -1 !!! fill missing patch at EQUATOR

END SUBROUTINE reproj
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE get_ice_age_map
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   get_ice_age_map
!
! !Description:
!    Purpose - Drive processing of NOAA ice maps
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
!
 CALL open_noaa_ice
 CALL read_noaa_ice

!--- NH Ice Age Map
! 0= First Year
! 1= Multi Year
 WHERE (scenh == 3) icanh = 1  ! multi-year ice
 WHERE (scesh == 3) icash = 1  ! multi-year ice
 WHERE (icanh > 1 .OR. icanh < 0) icanh = -1
 WHERE (icash > 1 .OR. icash < 0) icash = -1

! Reproject Polar Stereographic (SH &NH) into a 2160x1080 10'global map
 CALL reproj(icanh,icash,iceage)

!Deresolve 2160x1080 10'global map into a 360x180 1deg map.
 CALL deresolve 

 DEALLOCATE ( iceage )
 DEALLOCATE (idx)
 DEALLOCATE (jdx)
 DEALLOCATE ( icanh)
 DEALLOCATE ( scenh)
 DEALLOCATE ( icash)
 DEALLOCATE ( scesh)

END SUBROUTINE get_ice_age_map
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE get_afwa_ice_age_map
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   get_afwa_ice_age_map
!
! !Description:
!    Purpose - Drive processing of AFWA ice maps
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
!  Version 1.0   December 11, 2021 - SCCR #1878
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      INTEGER :: acces_mode, Ios, Namelength, Version, fileID
      CHARACTER ( LEN = 255) :: file
      CHARACTER ( LEN = 1), PARAMETER :: Version_char(9) =      &
                         (/'1','2','3','4','5','6','7','8','9'/)      
      LOGICAL :: filepresent
!
      INTEGER :: sfstart, sds_index, irow, start(3), edge(3), stride(3), i, j 
      CHARACTER (LEN = 20), PARAMETER :: afwa_name = 'AFWA_ICE_AGE_DAYS   '
      
      REAL :: data_buf(360), data_map(1,180,360)
      CHARACTER*120 :: Name_Sbr = 'get_afwa_ice_age_map'

!
      acces_mode = 0
!
      CALL GetFileName(LogicID = LID_ICE_AFWA,      &
                       Filename= file, &
                       Status  = Ios ,             &
                       Incpath = 1 ,               &
                       Version = 1)
!
!
      Ios = nf90_open(file, acces_mode, fileID) 

      Ios = nf90_inq_varid(fileID, afwa_name,sds_index )

      DO irow = 1, 180
!     *** Loop through all latitude zones
          start(1) = 1
          start(2) = irow 
          start(3) = 1   ! *** [ 0 - 3]  or [ 0 - 7]
          stride  = 1
          edge(1) = 360
          edge(2) = 1
          edge(3) = 1
!
          Ios = nf90_get_var(fileID, sds_index, data_buf, start, edge, stride)
          data_map ( 1,irow,1:360) = data_buf
      END DO
               
      Ios = nf90_close ( fileID)
 
      
!--- Ice Age Map
! 0= First Year
! 1= Multi Year

      iceage_1deg = 0
      DO i = 1, 180 
       DO j = 1, 360
        IF (data_map(1,i,j) /= REAL4_DFLT) THEN
	 IF ((i <= 40) .OR. (i >= 130)) THEN
	  IF (data_map(1,i,j) > 200) iceage_1deg(j,i) = 1
         ENDIF
        ENDIF
       ENDDO
      ENDDO


END SUBROUTINE get_afwa_ice_age_map
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE deresolve
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   deresolve
!
! !Description:
!    Purpose - Deresolve 2160x1080 10'global map into a 360x180 1deg map
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
!
INTEGER (Kind=1) gb(36)
INTEGER nn,ix1,ix2,iy1,iy2,ilat,ilon
iceage_1deg = 0 

LATITUDE: DO ilat=1,180
LONGITUDE: DO ilon=1,360
 CALL find_10m_lon(ilon, ix1,ix2)
 CALL find_10m_lat(ilat, iy1,iy2)
 gb(1:36) =  RESHAPE( iceage(ix1:ix2,iy1:iy2) ,(/36/) )

 nn = COUNT( MASK = gb .eq. 1  )
 IF ( nn > 1 ) iceage_1deg(ilon,ilat) = 1

  END DO LONGITUDE
END DO LATITUDE

END SUBROUTINE deresolve
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE find_10m_lon(ilon, ix1,ix2)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   find_10m_lon
!
! !Description:
!    Purpose - Compute equivalent longitude on 10m map
!
! 
! !Input Parameters:
!    ilon - longitude
!
! !Output Parameters:
!    ix1, ix2 - Output boundaries for 10m longitude
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
IMPLICIT NONE 

INTEGER ilon
INTEGER ix1,ix2

ix1=(ilon-1)*6 +1
ix2=ix1+5

END SUBROUTINE find_10m_lon
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
SUBROUTINE find_10m_lat(ilat,iy1,iy2)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   find_10m_lat
!
! !Description:
!    Purpose - Compute equivalent latitude on 10m map
!
! 
! !Input Parameters:
!    ilat - longitude
!
! !Output Parameters:
!    iy1, iy2 - Output boundaries for 10m latitude
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
IMPLICIT NONE 

INTEGER ilat
INTEGER iy1,iy2

iy1=(ilat-1)*6 +1
iy2=iy1+5

END SUBROUTINE find_10m_lat
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE RDICEAGEMAP
