MODULE CRHSARB
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   CRHSARB
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
!    - 
!    -  
!        
!
!  Modules Used:
!                                            
!  External Files Used: 
!    
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
! *****************************************************************************
!
    USE SYN_IO_Utils ,only:  Fred_OpenFile, Fred_CloseFile
    USE PACKALB
    USE SARB_IO_Utils
    USE SynSARB_IO_Params
    USE CREATECRHSARB ,only: nir_16_exists
!
    IMPLICIT NONE

    INTEGER iunit_crh_out
!
    INTEGER, PARAMETER :: nregion = 2332800
    INTEGER, PARAMETER :: nrzone = 2160*6   !12960
!
!! HERE ARE THE THRESHOLDS FOR SCENE SELECTIONS FROM THE CRH OVHSUN ALBEDO
    REAL, PARAMETER :: snowgrain_th = -1.897 , snow_th = 0.70 , &
                  green_desert_th = 0.50 , ice_th =0.40

!  PACKED SARB HISTORY ARRAY, TIME ORDERED...
    INTEGER*1 ,ALLOCATABLE, SAVE :: albhistory(:,:)   !!(31,nrzone)

    INTEGER*1 igbp(nrzone)     !IGBP MAP
    INTEGER*1 snow_ice(nrzone) !SNOW OR ICE MAP

    INTEGER, PARAMETER :: maxast = 25 !! Altered Scene Types
    REAL ptv(maxast,31,360)
    REAL p17(360) ! native ocean IGBP%

    CONTAINS
!
!*****************************************************************************
!
  SUBROUTINE open_igbp_zone
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   open_igbp_zone
!
! !Description:
!    Purpose - 
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
    CHARACTER (LEN = 14) :: Name_Sbr = 'open_igbp_zone'
    INTEGER :: RECL = nrzone
    INTEGER :: VerID = 1
!
    CALL SARB_GenOpen (LID_IGBP, Flag_File_Read,                       & ! I
                       Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                              IUnit_IGBP,              & ! O
                                              VerID, RECL)               ! I (Opt)
!
  END SUBROUTINE open_igbp_zone
!
!*****************************************************************************
!
  SUBROUTINE open_sarbcrh
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   open_sarbcrh
!
! !Description:
!    Purpose - 
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
  INTEGER :: RECL = 31*nrzone
  INTEGER :: VerID = 1
  CHARACTER (LEN = 12) :: Name_Sbr = 'open_sarbcrh'
 
  IF ( allocated(albhistory) ) DEALLOCATE ( albhistory )
! 
  ALLOCATE ( albhistory(31,nrzone) )
!
  CALL SARB_GenOpen (LID_SCRH, Flag_File_Read,                       & ! I
                     Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                              iunit_crh_out,         & ! O
                                               VerID, RECL)            ! I (Opt)
!
  END SUBROUTINE open_sarbcrh
!
!*****************************************************************************
!
  SUBROUTINE getigbp(ilat)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   getigbp
!
! !Description:
!    Purpose - 
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
    INTEGER ilat ,ios
    INTEGER, SAVE :: ilat_old
    CHARACTER*120 :: Name_Sbr = 'getigbp'
!
    DATA ilat_old /-1/
!
    IF ( ilat == ilat_old ) RETURN

    READ(IUnit_IGBP, REC = ilat , IOSTAT = ios) igbp

    IF ( ios /= 0 ) &
     CALL SARB_RdWrtError (13, ios, Name_Sbr, IRec = ilat)

    ilat_old=ilat
!
  END SUBROUTINE getigbp
!
!*****************************************************************************
!
  SUBROUTINE sarbcrh_history_file_io(iband, ilat)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   getigbp
!
! !Description:
!    Purpose - 
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
!  Version 4.0   August 18, 2006 - SCCR #634  
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Commented out:
!                IF ( ilat == ilat_old(iband) ) RETURN
!
! !END
!
!*****************************************************************************
!
!
    INTEGER iband, ilat,ios
    INTEGER, SAVE :: ilat_old(4)
    CHARACTER*120 :: Name_Sbr = 'sarbcrh_history_file_io'
    DATA ilat_old /-1, -1, -1, -1/
!
!   IF ( ilat == ilat_old(iband) ) RETURN

    READ(iunit_crh_out, REC = ilat + (iband-1)*180, IOSTAT = ios) albhistory
    
    IF ( ios /= 0 ) &
      CALL SARB_RdWrtError (100, ios, Name_Sbr, IRec = ilat + (iband-1)*180)
    
    ilat_old(iband) = ilat
!
  END SUBROUTINE sarbcrh_history_file_io
!
!*****************************************************************************
!
  SUBROUTINE make_seaice(ice1,g1)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   make_seaice
!
! !Description:
!    Purpose - 
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
    INTEGER*1, DIMENSION (36):: ice1, g1
    REAL ss 
    INTEGER i, imake, nmake, n17
!
    n17 = count(  MASK = g1 .eq. 17  )
    IF ( n17 < 1 ) RETURN ! no water to make ice

    ss =   sum(ice1*0.01, MASK = ice1 >= 0 .AND. ice1 <= 100 )  / 36.
    nmake = nint( ss *36 )

    imake=0
    DO i = 1,nmake
      IF ( g1(i) == 17 ) THEN
        g1(i) = 20  ! CONVERT WATER  to SEA ICE
        imake=imake+1
        IF ( imake >= nmake ) EXIT
      ENDIF
    END DO
!
  END SUBROUTINE make_seaice
!
!*****************************************************************************
!
  SUBROUTINE create_contents(content,content_vector,ktot)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   create_contents
!
! !Description:
!    Purpose - 
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
    INTEGER i,ntot,nn,ktot
    REAL v1
    INTEGER*1 , PARAMETER :: bomit = -1  !! DO NOT CHANGE necessary 
    INTEGER*1  content(36)
    INTEGER count(36)
    REAL content_vector(36,2)
!
    content_vector = bomit
    ntot=0
    ktot=0
!
    DO i=1,36
      count=0
      v1= minval(content, MASK = content /= bomit)
!
      WHERE ( content == v1 ) 
        count = 1
        content =bomit
      END WHERE
!
      nn = sum(count(1:36) )
      ntot=ntot+nn
      content_vector(i,1)= v1
      content_vector(i,2)= 100.* float(nn) /36.0
      ktot=i
      IF ( ntot == 36 ) EXIT
    END DO
!
  END SUBROUTINE create_contents
!
!*****************************************************************************
!
  SUBROUTINE modify_igbp(ilat, myday)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   modify_igbp
!
! !Description:
!    Purpose - 
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
    INTEGER*1, DIMENSION(nrzone) :: g10 ,ice 
    INTEGER*1, DIMENSION(2160,6) :: g2,  ice2  ,igbp2
    INTEGER*1, DIMENSION(36)     :: g1,  ice1  ,igbp1
    REAL, DIMENSION(nrzone) :: vis,nir
    REAL, DIMENSION(2160,6) :: vis2,nir2
    REAL, DIMENSION(36)     :: vis1,nir1 ,ln_nir_vis
!
    EQUIVALENCE ( g10   ,g2)
    EQUIVALENCE ( ice   ,ice2)
    EQUIVALENCE ( vis   ,vis2)
    EQUIVALENCE ( nir   ,nir2)
!
    REAL content_vector(36,2),value
    INTEGER k, ktot, i, j
    INTEGER it,ix1,ix2
    INTEGER iband,ilat,ilon,ios
    INTEGER ida ,idb,ide
    INTEGER, OPTIONAL :: myday
!
    ptv=0.000
    idb=1 
    ide=31
    IF ( PRESENT(myday) )  THEN
     idb=myday ; ide=idb
    ENDIF
!
    DAY : DO ida = idb,ide
      CALL getigbp(ilat)
      igbp2 =  reshape( igbp ,(/2160,6/) )
      g10=igbp  !!IGBP STATIC MAP (1-18)
      
      iband=1 !VIS
      CALL sarbcrh_history_file_io(iband,ilat)
!
      DO k = 1 , nrzone
        vis(k)=  unpka( albhistory(ida,k) ) 
      END DO
!
      iband=2 !NIR
      CALL sarbcrh_history_file_io(iband,ilat)
!
      DO k = 1 , nrzone 
        nir(k)=  unpka( albhistory(ida,k) ) 
      END DO
!
      iband=3 !SNOW SNOW (0,1,-1) 
      CALL sarbcrh_history_file_io(iband,ilat)
      WHERE ( albhistory(ida,1:nrzone) == 1 ) g10 = 19 !! FRESH SNOW 
!
      iband=4 !ICE ICE(0:100,-1)
      CALL sarbcrh_history_file_io(iband,ilat)
      ice = albhistory(ida,1:nrzone)

      LONGITUDE: DO ilon=1,360
        CALL find_10m_lon(ilon, ix1,ix2)
        igbp1(1:36) =  reshape( igbp2(ix1:ix2,1:6) ,(/36/) ) !Static IGBP
        vis1(1:36)  =  reshape(  vis2(ix1:ix2,1:6) ,(/36/) ) !0.63 OHS Daily TOA Albedo
        nir1(1:36)  =  reshape(  nir2(ix1:ix2,1:6) ,(/36/) ) !1.6  OHS Daily TOA Albedo
        ice1(1:36)  =  reshape(  ice2(ix1:ix2,1:6) ,(/36/) ) !ICE ICE(0:100,-1)
        g1(1:36)    =  reshape(    g2(ix1:ix2,1:6) ,(/36/) ) !IGBP with SNOW(19) 
    
        p17(ilon) = 100.* count(  MASK = igbp1 .eq. 17  ) / 36.  ! Native Ocean/water Percentage
    
!! USE Microwave Sea Ice Percentage to set # of 10' Grid boxes to sea ice
        CALL make_seaice(ice1,g1) 
!
!  Modify 10' scene type according to thresholds of vis and nir toa ovhsun albedo.
        IF (Sat_Name == 'Terra' ) THEN
    
          nir_16_exists = .true.

!  Only Use Microwave Sea Ice determination for SAH Pert.
!          WHERE ( igbp1 == 17 .AND. vis1 > ice_th .AND. vis1 < snow_th )  g1 = 20 !!  ADD SEA ICE OVER OCEAN

          IF (nir_16_exists .EQV. .TRUE.) THEN
            DO j = 1, 36
             IF ( vis1(j) <= 0.0 .OR. vis1(j) > 1.0 )  THEN 
              ln_nir_vis(j) = 1.0
             ELSE 
              ln_nir_vis(j) = log(nir1(j)/vis1(j))
             ENDIF
            END DO
    
!           WHERE (  vis1 >=  snow_th  .AND. ln_nir_vis  <=  snowgrain_th )  g1 = 21 !!  Large Grain SNOW Anywhere
!           WHERE (  vis1 >=  snow_th  .AND. ln_nir_vis   >  snowgrain_th )  g1 = 22 !!  Small Grain SNOW Anywhere 

! ONLY rely on SceneID for (NOT Albedo threshold for snow determination SAH pert consistency)

	    IF (  p17(ilon) < 10.0 .OR. p17(ilon) > 99.0 ) THEN  !NATIVE ALL Land Or ALL Ocean  NO ALBEDO THRESHOLD
             WHERE (  (g1 ==  15 .or. g1 ==  19 ) .AND. ln_nir_vis <=  snowgrain_th )  g1 = 21 !!  Large Grain SNOW Anywhere
             WHERE (  (g1 ==  15 .or. g1 ==  19 ) .AND. ln_nir_vis  >  snowgrain_th )  g1 = 22 !!  Small Grain SNOW Anywhere 
	    ELSE ! NATIVE COASTAL gridboxes use Albedo threshold where Microwave data is suspect
	     WHERE ( igbp1 == 17 .AND. vis1 > ice_th  )  g1 = 20 !!  ADD SEA ICE OVER OCEAN
             WHERE ( ((g1 ==  15 .or. g1 ==  19 ) .or. vis1 >=  snow_th ) .AND. ln_nir_vis <=  snowgrain_th )  g1 = 21 !!  Large Grain SNOW Anywhere
             WHERE ( ((g1 ==  15 .or. g1 ==  19 ) .or. vis1 >=  snow_th ) .AND. ln_nir_vis  >  snowgrain_th )  g1 = 22 !!  Small Grain SNOW Anywhere 
	    ENDIF

            WHERE ( (igbp1 == 7 .OR. igbp1 == 16) .AND. ln_nir_vis  >=  green_desert_th .AND. vis1 < snow_th) g1 = 23 !! GREEN DESERT
            WHERE ( (igbp1 == 7 .OR. igbp1 == 16) .AND. ln_nir_vis  <   green_desert_th .AND. vis1 < snow_th) g1 = 24 !! BARREN DESERT
          ENDIF
        ENDIF
!
        CALL create_contents(g1,content_vector,ktot)
!
        DO k=1,ktot
          DO it =1,maxast
            IF ( content_vector(k,1) == it ) ptv(it,ida,ilon) = content_vector(k,2)
          END DO
        END DO
!
      END DO LONGITUDE

    END DO DAY
!
  END SUBROUTINE modify_igbp
!
!*****************************************************************************
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
!
  INTEGER ilon
  INTEGER ix1,ix2

  ix1=(ilon-1)*6 +1
  ix2=ix1+5

  END SUBROUTINE find_10m_lon
!
!*****************************************************************************
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
  INTEGER ilat
  INTEGER iy1,iy2

  iy1=(ilat-1)*6 +1
  iy2=iy1+5

  END SUBROUTINE find_10m_lat
!
!*****************************************************************************
!
END MODULE CRHSARB
