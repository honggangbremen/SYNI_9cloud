MODULE MATCH_ED4v13
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   MATCH_ED4v13
!
! !Description:
!
!    Purpose - To interface Synoptic SARB with Edition4 MATCH files
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
!    open_matchday_netcdf           - Opens Ed4 MATCH netcdf files using PCF LIDs
!    ingest_match_netcdf_month_zone - Reads MATCH data for specific day and zone 
!    get_mpro_gridboxhr             - Processes MATCH data into SARB parameters 
!    idate_chop                     - Separates date string into year, month, and day 
!    match_lat                      - References CERES latitudes to MATCH latitudes 
!    match_lon                      - References CERES longitudes to MATCH longitudes  
!    print_mpro_gridboxhr           - Diagnostic routine for printing MATCH values 
!    conform_to_match_ps            - Applies surface pressure boundary condition to MATCH
!
!  Modules Used:
!   NETCDF - (CERESlib)  Used by Sbr. open_matchday_netcdf, 
!                                     ingest_match_netcdf_month_zone
!   PCF    - (CERESlib)  Used by Sbr. open_matchday_netcdf
!
! !Revision History:
!  Version 1.1   November 7, 2024 - SCCR #2462
!                 Thomas Caldwell, ADnet, Thomas.E.Caldwell@nasa.gov
!                 Added call to list MATCH files in metadata output
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
      USE IO, ONLY: AddInputFile
      USE NETCDF
      Use msg, ONLY: WriteReport

      IMPLICIT NONE

      INCLUDE 'PGS_IGBPUTIL_25721.f'
 
      INTEGER, PARAMETER :: nlon=192 ,nlat=94,nlev=28,ntime=1 ,nhr=24,nday=31
      INTEGER, PARAMETER :: nf=4,np=11,npc=7 
      INTEGER, PARAMETER :: mlev = 200 
      REAL,    PARAMETER :: ptrop =200.

      CHARACTER*9 varlab(0:np,2)
        
      INTEGER :: IOS,iday,ihr		
 
      TYPE MVPTYPE
!      Inputs...
       CHARACTER*512 matchnetcdf
       INTEGER iday    ! Day of Month
       INTEGER ihour   ! Hour of Day
 
       REAL rlat       ! Latitude :Input
       REAL rlon       ! Longitude : Input
       INTEGER nv1     ! # of levels of PP input array : Input
       REAL pp(mlev)   ! Pressure level array for output profile : Input
       LOGICAL iprint  ! True = PRINT  , False = silent
!      Outputs... 
       INTEGER iday_loaded
       REAL rlat_loaded
       REAL mvp(mlev,np)       !OUTPUT : fractional weighting by layer for 'np'RAW match constituents
       REAL mvp_comb(mlev,npc) !OUTPUT : fractional weighting by layer for 'npc'COMBINED match constituents
       INTEGER np	       !OUTPUT: # of raw constitunets
       INTEGER npc	       !OUTPUT: # of Combined constituents 
       INTEGER mlev	       !OUTPUT: Max # of levels 
       CHARACTER*7 lab11(np)   !OUTPUT: label for Raw constituents
       CHARACTER*7 lab7(npc)   !OUTPUT: Label for Combined Constituents
       REAL aots_all(0:np)     !OUTPUT: Aerosol Optical Depths by raw constituent
       REAL aots_comb(0:npc)   !OUTPUT: Aerosol Optical Depths by Combined constituent
       INTEGER ityp_comb(npc)  !OUTPIT: Fu-liou Aerosol type index for combined consttuents. 
      END TYPE MVPTYPE

      TYPE  (MVPTYPE) mpro   !!! INPUT & OUTPUT DATA STRUCTURE

      INTEGER :: NCID 
      INTEGER :: NDIMS,NVARS !,NGATTS,RECDIM
      INTEGER :: START(4),COUNT(4),VDIMS(4) !ALLOW UP TO 4 DIMENSIONS
      INTEGER :: NDSIZE,NVDIM   !,NTP,NVS
      INTEGER :: idate1,idx,ivarid 
      !INTEGER :: iyr,imo,ida
      INTEGER :: base_time  !,IDATE(ntime)  
      INTEGER :: ilat,ilon,itime

      TYPE MATCHNCDFDAT_TYPE
       REAL*4 Gps(nhr,nday,nlon)
       REAL*4 Gv4p(nhr,nday,nlon,nlev,np)	
       REAL*4 Gv4t(nhr,nday,nlon,0:np)  
      END TYPE MATCHNCDFDAT_TYPE

      TYPE (MATCHNCDFDAT_TYPE) mgd
		
      DATA varlab / &
      'PS',         &
      'DSTQ01',     & ! 1
      'DSTQ02',     & ! 2
      'DSTQ03',     & ! 3
      'DSTQ04',     & ! 4 
      'SO4' ,       & ! 5 
      'SSLT',       & ! 6
      'BCPHI',      & ! 7
      'BCPHO',      & ! 8
      'OCPHI',      & ! 9
      'OCPHO',	    & ! 10
      'VOLC' ,      & ! 11     
      'AEROD',      & ! 0
      'DSTODX01',   & ! 1
      'DSTODX02',   & ! 2
      'DSTODX03',   & ! 3 
      'DSTODX04',   & ! 4
      'SO4OD',      & ! 5
      'SSLTOD',     & ! 6 old var name 'TAUSSLT      '
      'BCPHIOD',    & ! 7
      'BCPHOOD',    & ! 8 
      'OCPHIOD',    & ! 9
      'OCPHOOD',    & ! 10
      'VOLCOD'/       ! 11    
       
      DATA mpro%lab11 / &
      ' DSTQ01',        & !1  1
      ' DSTQ02',        & !2  2+
      ' DSTQ03',        & !3  2+
      ' DSTQ04',        & !4  2+
      '    SO4' ,       & !5  3
      '   SSLT',        & !6  4
      '  BCPHI',        & !7  5+
      '  BCPHO',        & !8  5+
      '  OCPHI',        & !9  6
      '  OCPHO',        & !10
      '   VOLC' /         !11  7


      DATA mpro%lab7 / &
      ' DustSm',       & !1  
      ' DustLg',       & !2  2:4 
      '    SO4',       & !3  5
      '   SSLT',       & !4  6
      '   Soot',       & !5  7:8  
      '  Solub',       & !6  9
      '  Insol'/         !7  10      

      REAL rlatca(nlat),rlonca(nlon)
      REAL hybi(nlev+1),press(nlev)
	
      DATA rlatca /                                                   &
      -88.54195,-86.65317,-84.75323, -82.85078,-80.94736,-79.0434,    &
      -77.13935, -75.23505, -73.33066, -71.42619,-69.52167,-67.617,   &
      -65.71251, -63.8079, -61.90326, -59.99861,-58.09395,-56.1892,   &
      -54.2846, -52.37991, -50.47522, -48.57052, -46.66582,-44.7611,  &
      -42.8564, -40.95169, -39.04697, -37.14225, -35.23753,-33.3328,  &
      -31.42808, -29.52336, -27.61863, -25.7139, -23.80917,-21.9044,  &
      -19.99971,-18.09498,-16.19024,-14.28551, -12.38078,-10.4760,    &
      -8.571308, -6.666573, -4.761838, -2.857103, -0.952368,0.95236,  &
      2.857103, 4.761838, 6.666573,8.571308,10.47604,12.38078,14.285, &
      16.19024, 18.09498, 19.99971,21.90444,23.80917,25.7139, 27.618, &
      29.52336, 31.42808, 33.33281,35.23753,37.14225,39.04697,40.951, &
      42.8564, 44.76111, 46.66582,48.57052,50.47522,52.37991, 54.284, &
      56.18928, 58.09395, 59.99861,61.90326,63.8079,65.71251, 67.617, &
      69.52167, 71.42619, 73.33066,75.23505,77.13935,79.04349,80.947, &
       82.85078, 84.75323, 86.65317,88.54195 /
	
      DATA rlonca /                                                    &
      0,1.875,3.75,5.625,7.5,9.375,11.25,13.125,15,16.875,18.75,       &
      20.625,22.5,24.375,26.25,28.125,30,31.875,33.75,35.625,37.5,     &
      39.375,41.25,43.125,45,46.875,48.75,50.625,52.5,54.375,56.25,    &
      58.125,60,61.875,63.75,65.625,67.5,69.375,71.25,73.125,75,       &
      76.875,78.75,80.625,82.5,84.375,86.25,88.125,90,91.875,93.75,    &
      95.625,97.5,99.375,101.25,103.125,105,106.875,108.75,110.625,    &
      112.5,114.375,116.25,118.125,120,121.875,123.75,125.625,127.5,   &
      129.375,131.25,133.125,135,136.875,138.75,140.625,142.5,144.375, &
      146.25,148.125,150,151.875,153.75,155.625,157.5,159.375,161.25,  &
      163.125,165,166.875,168.75,170.625,172.5,174.375,176.25,178.125, &
      180,181.875,183.75,185.625,187.5,189.375,191.25,193.125,195,     &
      196.875,198.75,200.625,202.5,204.375,206.25,208.125,210,211.875, &
      213.75,215.625,217.5,219.375,221.25,223.125,225,226.875,228.75,  &
      230.625,232.5,234.375,236.25,238.125,240,241.875,243.75,245.625, &
      247.5,249.375,251.25,253.125,255,256.875,258.75,260.625,262.5,   &
      264.375,266.25,268.125,270,271.875,273.75,275.625,277.5,279.375, &
      281.25,283.125,285,286.875,288.75,290.625,292.5,294.375,296.25,  &
      298.125,300,301.875,303.75,305.625,307.5,309.375,311.25,313.125, &
      315,316.875,318.75,320.625,322.5,324.375,326.25,328.125,330,     &
      331.875,333.75,335.625,337.5,339.375,341.25,343.125,345,346.875, &
      348.75,350.625,352.5,354.375,356.25,358.125/

      DATA hybi / &
      0.00100,0.00657,0.01386,0.02309,0.03469,0.0492,  &
      0.06723,0.08945,0.11654,0.14916,0.18783,0.23286, &
      0.28421,0.34137,0.40334,0.4686,0.53529,0.60135,  &
      0.66482,0.72401,0.77773,0.82527,0.86642,0.90135, &
      0.93054,0.95459,0.97418,0.99,1.0/

      DATA press /0.0027,0.0101,0.0183,0.0288,0.0418,0.058,&
      0.0782,0.1028,0.1326,0.1682,0.2101,0.2582,           &
      0.3125,0.372, 0.4357,0.5017,0.5681,0.6329,           &
      0.6943,0.7508,0.8014,0.8458,0.8838,0.9159,           &
      0.9425,0.9644,0.9821,0.995/
      
      INTEGER icxA(np)  
      INTEGER icxT(np)  !!! Pointer to combine 10 constituents into 7
      INTEGER icxS(np)
      
      DATA icxA /1, 2, 2, 2, 0, 4, 5 ,5 ,6, 7,   3/ ! Entire propile
      DATA icxT /0, 0, 0, 0, 6, 0, 0 ,0 ,0, 0,   0/ !Troposphere
      DATA icxS /0, 0, 0, 0, 3, 0, 0 ,0 ,0, 0,   0/ !Stratosphere 

      CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE open_matchday_netcdf
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   open_matchday_netcdf
!
! !Description:
!    Purpose - Opens Ed4 MATCH netcdf files using PCF LIDs
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
!  Modules Used: 
!    PCF      - (CERESlib) Sbr. GetFileName
!    NETCDF   - (CERESlib) Sbr. nf90_open
!                                 
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
! 	

      USE pcf, ONLY : GetFileName
      INTEGER , PARAMETER :: MATCHPCFID = 4
  
      CALL GETFILENAME(LogicID = MATCHPCFID ,      &
                       FILENAME= mpro%matchnetcdf, &
                       STATUS  = ios ,             &
                       INCPATH = 1 ,               &
                       VERSION = mpro%iday)

      IF (IOS /= 0) THEN
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (IGBPUTIL_E_IGBP_OPEN,                       & ! I
                            FuncName = "open_matchday_netcdf ()",       & ! I
                            StrVar = " IOS = %i4 ",                     & ! I
                            IVar1 = IOS)                                  ! I
!
      END IF
!      
      ios = nf90_open(trim(mpro%matchnetcdf), nf90_nowrite,NCID)
 
      IF (IOS /= 0) THEN
!     *** Fatal error.  Terminate processing.
!
          CALL WriteReport (IGBPUTIL_E_IGBP_OPEN,                       & ! I
                            FuncName = "open_matchday_netcdf ()",       & ! I
                            StrVar = " IOS = %i4 ",                     & ! I
                            IVar1 = IOS)                                  ! I
!
      END IF

      CALL AddInputFile(MATCHPCFID, mpro%iday)  ! add to input list for metadata


      END SUBROUTINE open_matchday_netcdf
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE ingest_match_netcdf_month_zone
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   ingest_match_netcdf_month_zone
!
! !Description:
!    Purpose - Reads MATCH data for specific day and zone
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
!  Modules Used: 
!    NETCDF   - (CERESlib) Sbr. nf90_inquire_variable, nf90_inquire_dimension
!                               nf90_get_var, nf90_close  
!  Input Files:
!    NONE
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
    INTEGER      :: ipo,ip,j ,ilev
    CHARACTER*31 :: labdim(10)
    REAL         :: GV4P1(nlon,nlev,nhr), GV4T1(nlon,nhr)
 
 
    ilat = match_lat(mpro%rlat)
 
    iday = mpro%iday
      
    IF ( NCID <= 0 ) RETURN

!----Date

    ios = nf90_inq_varid(NCID,'date          ',ivarid )
    ios = nf90_inquire_variable(ncid,ivarid, ndims = nvdim)
    ios = nf90_inquire_variable(ncid,ivarid, dimids = vdims(1:nvdim))

    START(1:nvdim) = 1

    DO  J=1,NVDIM     
     ios = nf90_inquire_dimension(ncid,vdims(j),labdim(j),COUNT(j))
    ENDDO
   
 !---- Contituents

   PRF_OD      : DO ipo = 1,2
   CONSTITUENT : DO ip=0,np
     
      ios = nf90_inq_varid(NCID,trim(varlab(ip,ipo)) ,ivarid )
      
      ios = nf90_inquire_variable(ncid,ivarid, ndims = nvdim)
      ios = nf90_inquire_variable(ncid,ivarid, dimids = vdims(1:nvdim))

      START(1:nvdim) = 1 
          
      DO  J=1,NVDIM
       ios = nf90_inquire_dimension(ncid,vdims(j),labdim(j),COUNT(j))	
       
        IF ( labdim(j) == 'time' .AND. count(j) .NE. 24 ) stop ' WRONG FILE TIME DIMENSION'
	
	IF ( labdim(j) == 'time' .AND. count(j) == 24 )  THEN !Hourly Use selected hour
	 start(j) = 1
	 count(j) = 24	 
 	ENDIF
	
	IF ( labdim(j) == 'lat' .AND. count(j) == 94 )  THEN !Hourly Use selected hour
	 start(j) = ilat
	 count(j) = 1	 
 	endif		
      ENDDO       
	
      IF (IPO ==1 .and. ip == 0 ) THEN

         ios = nf90_get_var(ncid,ivarid, GV4T1,start=start,count=count  )
      
         IF ( ios .NE. 0 ) stop ' bad nf90_get_var/ GPS'
       
         DO ihr=1,24
          mgd%Gps(iday,ihr,1:nlon) = GV4T1(1:nlon,ihr)
          mgd%Gps(iday,ihr,1:nlon) = mgd%Gps(iday,ihr,1:nlon)/100. ! to Pa to hPa
         ENDDO
      	 
      ELSEIF ( IPO == 1 .AND. IP .GT. 0 ) THEN

          ios = nf90_get_var(ncid,ivarid, Gv4p1,start=start,count=count)
      
          IF ( ios .NE. 0 ) stop ' bad nf90_get_var/ Gv4p'

	  DO ihr=1,24		
	   DO ilon=1,nlon  
	   mgd%Gv4p(iday,ihr,ilon ,1:nlev,ip) = GV4P1(ilon,1:nlev,ihr)
	   ENDDO
	  ENDDO
	  
      ELSEIF ( IPO == 2 ) THEN
 
          ios = nf90_get_var(ncid,ivarid, Gv4t1,start=start,count=count)
          IF ( ios .NE. 0 ) STOP ' bad nf90_get_var/ Gv4t'
	   
	  DO ihr=1,24
	   mgd%Gv4t(iday,ihr,1:nlon,ip) = GV4T1(1:nlon,ihr)
	   WHERE( mgd%Gv4t(iday,ihr,1:nlon,ip) < 0 .OR. mgd%Gv4t(iday,ihr,1:nlon,ip) > 100) mgd%Gv4t(iday,ihr,1:nlon,ip)= 0.0	  
          enddo
	
      ENDIF
	  
   ENDDO CONSTITUENT
   ENDDO PRF_OD	

		
   ios = nf90_close(NCID)

   IF (IOS /= 0) THEN
!   *** Fatal error.  Terminate processing.
!
         CALL WriteReport (IGBPUTIL_E_IGBP_CLOSE,                       & ! I
                           FuncName = "ingest_match_netcdf_month_zone ()",       & ! I
                           StrVar = " IOS = %i4 ",                     & ! I
                           IVar1 = IOS)                                  ! I
!
    END IF
	
   mpro%rlat_loaded = mpro%rlat
   mpro%iday_loaded = mpro%iday	

   END SUBROUTINE ingest_match_netcdf_month_zone
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   SUBROUTINE get_mpro_gridboxhr
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   get_mpro_gridboxhr
!
! !Description:
!    Purpose - Processes MATCH data into SARB parameters
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
!  Notes: 
!  It is a revision to fix a logic bug and a correction for non constant
!  pressure thickness ...F.Rose Nov2004 
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
     INTEGER k,ip,i,ilev_trop
     REAL psfc,p1,p2,px,yyy,dp,pt,pb,sdp,plast
     REAL*8 tta(np) , tta_comb(npc)
     REAL*8 ttt(np) 
     REAL*8 tts(np) 
     REAL*4 amr(np)

     REAL vslice(nlev,np)
     REAL asm1(np)
     REAL nsm1
     REAL dpp(mlev)
     REAL chksumAOT


     ilat = match_lat(mpro%rlat)
     ilon = match_lon(mpro%rlon)
	
     mpro%aots_all(0:np) = mgd%Gv4t(mpro%iday,mpro%ihour, ilon, 0:np)
      
     chksumAOT= SUM(mpro%aots_all(1:np))  ! Alternate Sum constituent to get total AOT

     mpro%aots_all(0) = chksumAOT

     psfc      = mpro%pp(mpro%nv1)

     mpro%np   = np
     mpro%npc  = npc
     mpro%mlev = mlev

     DO i =1,mpro%nv1 
      IF ( mpro%pp(i) >= ptrop ) EXIT 
      ilev_trop = i
     ENDDO

     DO k=1,nlev
      DO ip=1,np
       vslice(k,ip) = mgd%Gv4p(mpro%iday,mpro%ihour,ilon, k,ip)  !upbv( ipout(ilon,ilat,k,ip) ) !UNPACK to array
      ENDDO
     ENDDO

!  Vertical integration into Percentage profiles....for desired pressure levels
!  defining model layers...

     mpro%mvp =0.0

     plast = 0.000!  mpro%pp(1)

     OUTLEV : DO i = 1, mpro%nv1-1
      p1 = mpro%pp(i)
      p2 = mpro%pp(i+1)
      dpp(i) = p2-p1

      asm1=0
      nsm1=0

      hybi(1) = 0.00 ! TOA at zero pressure

      INLEV: DO k = 1,nlev
       px = press(k) *psfc
       pt = hybi(k)  *psfc
       pb= psfc
       IF ( k .NE. nlev) pb= hybi(k+1) *psfc
       dp =0


       IF(   (p1 >= pt .AND. p2 <= pb ) .OR. &
             (pt >= p1 .AND. pb <= p2 ) .OR. &
             (p1 >= pt .OR. p2 >= pb )  .AND. ( p1<=pb .OR. p2<= pb) ) THEN

         dp = pb-plast  ! without 
         IF (dp == 0 ) cycle

         IF ( p2 < pb ) dp = p2-plast   

         plast = plast+dp

         !WHERE( vslice(k,1:np) < 1.0E-33 ) vslice(k,1:np) = 1.0E-33
 
         asm1(1:np)= asm1(1:np)+ vslice(k,1:np) * dp
         nsm1= nsm1+dp

       ENDIF


      ENDDO INLEV 

      mpro%mvp(i,1:np) =  asm1(1:np)/nsm1
   
     ENDDO OUTLEV 


     DO ip=1,np
      IF ( SUM( mpro%mvp(1:mpro%nv1-1,ip)) < 1.0E-30 ) mpro%mvp(ilev_trop,ip) =1.0E-30
     ENDDO

!
!!! Renormalize and Combine constituents.
!

     tta_comb =0 
     tta=0
     ttt=0
     tts=0

     mpro%mvp_comb  = 0 
     mpro%aots_comb = 0
     mpro%aots_comb ( 0) = mpro%aots_all( 0)	 ! TOTAL

     dpp(1:1:mpro%nv1-1) = dpp(1:1:mpro%nv1-1)/ SUM(dpp(1:1:mpro%nv1-1) )


     DO ip=1,np

      mpro%mvp(1:mpro%nv1-1 ,ip )= mpro%mvp(1:mpro%nv1-1 ,ip)* dpp(1:mpro%nv1-1)

      amr(ip) = mpro%aots_all(ip)/SUM( mpro%mvp(          1:mpro%nv1-1 ,ip)    )

      IF ( amr(ip) > 0 ) mpro%mvp(1:mpro%nv1-1 ,ip )= mpro%mvp(1:mpro%nv1-1 ,ip)* amr(ip)

      tta(ip) = SUM( mpro%mvp(	   1:mpro%nv1-1 ,ip)	) ! Entire Atmosphere
      tts(ip) = SUM( mpro%mvp(	   1:ilev_trop  ,ip)	) ! Stratosphere
      ttt(ip) = SUM( mpro%mvp(ilev_trop+1:mpro%nv1-1 ,ip)	) ! Troposphere
  
       IF ( icxA(ip) > 0 .AND. tta(ip) > 0 ) THEN 
        tta_comb(  icxA(ip) ) = tta_comb(  icxA(ip) ) + tta(ip) 
        mpro%mvp_comb( 1:mpro%nv1-1, icxA(ip)  ) =  &
        mpro%mvp_comb( 1:mpro%nv1-1, icxA(ip)  ) + mpro%mvp( 1:mpro%nv1-1, ip  )
        mpro%aots_comb ( icxA(ip)) = mpro%aots_comb ( icxA(ip)) + mpro%aots_all( ip) * tta(ip)/tta(ip)   ! TOTAL
       ENDIF
 
       IF ( icxS(ip) > 0 .AND. tts(ip) > 0) THEN 
        tta_comb( icxS(ip) ) = tta_comb( icxS(ip) ) + tts(ip)
        mpro%mvp_comb( 1:ilev_trop, icxS(ip)  ) =  &
        mpro%mvp_comb( 1:ilev_trop, icxS(ip)  ) + mpro%mvp( 1:ilev_trop, ip  )
        mpro%aots_comb ( icxS(ip)) = mpro%aots_comb ( icxS(ip)) + mpro%aots_all( ip) * tts(ip)/tta(ip)   !  STRAT
       ENDIF
 
       IF ( icxT(ip) > 0 .AND. ttt(ip) > 0) THEN 
        tta_comb( icxT(ip) ) = tta_comb( icxT(ip) ) + ttt(ip)
        mpro%mvp_comb( ilev_trop+1:mpro%nv1-1, icxT(ip)  ) =  &
        mpro%mvp_comb( ilev_trop+1:mpro%nv1-1, icxT(ip)  ) + mpro%mvp(ilev_trop+1:mpro%nv1-1, ip  )   
        mpro%aots_comb ( icxT(ip)) = mpro%aots_comb ( icxT(ip)) + mpro%aots_all( ip) * ttt(ip)/tta(ip)   !  TROP
       ENDIF

      ENDDO

      DO i = 1, mpro%nv1-1
       mpro%mvp(i,1:np)       = 100.*mpro%mvp     (i,1:np )/  tta(1:np)   
       mpro%mvp_comb(i,1:npc) = 100.*mpro%mvp_comb(i,1:npc)/  tta_comb(1:npc) 
      ENDDO


      mpro%aots_comb ( 1:npc) = mpro%aots_comb ( 1:npc) !/ tta_comb(1:npc)
      mpro%aots_comb ( 0)     = SUM(mpro%aots_comb ( 1:npc))

      mpro%ityp_comb(1:7) =  (/ 24, 25, 18, 1, 11, 10, 9 /) 

      END SUBROUTINE get_mpro_gridboxhr
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE idate_chop(idate,iyr,imo,ida)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   idate_chop
!
! !Description:
!    Purpose - Separates date string into year, month, and day
!
! 
! !Input Parameters:
!    idate - Data date in form of YYYYMMDD
!
! !Output Parameters:
!    iyr - Data year
!    imo - Data month
!    ida - Data day
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!
!  Input Files:
!    NONE
!
!  Notes: 
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
!
       INTEGER kkk,idate,iyr,imo,ida

       kkk=idate
       iyr = kkk/10000
       kkk = kkk-iyr*10000
       imo = kkk/100
       kkk = kkk-imo*100
       ida = kkk
 
       IF ( ida == 42 ) ida = 31
       IF ( ida == 41 .AND. imo /= 2) ida = 30
       IF ( ida == 41 .AND. imo == 2) ida = 29

       END SUBROUTINE idate_chop
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       INTEGER FUNCTION match_lat(rlat)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   match_lat
!
! !Description:
!    Purpose - References CERES latitudes to MATCH latitudes
!
! 
! !Input Parameters:
!    rlat - CERES latitude
!
! !Output Parameters:
!    match_lat - MATCH latitude
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!
!  Input Files:
!    NONE
!
!  Notes: 
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
        INTEGER jj,j
        REAL dlat1,dlat,rlat
 
        jj=-1 ; dlat=1E+30
 
        DO j=1,nlat
         dlat1= abs( rlat - rlatca(j) )
         IF ( dlat1 < dlat ) THEN
          jj=j
          dlat = dlat1
         ENDIF
        ENDDO

        match_lat=jj
 
       END FUNCTION match_lat
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       INTEGER FUNCTION match_lon(rlon)	
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   match_lon
!
! !Description:
!    Purpose - References CERES longitude to MATCH longitude
!
! 
! !Input Parameters:
!    rlon - CERES longitude
!
! !Output Parameters:
!    match_lon - MATCH longitude
!
! !Team-Unique Header:
!
!  Modules Used: (Parameters followed by "*" are defined by this routine)
!
!  Input Files:
!    NONE
!
!  Notes: 
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!
        INTEGER ii
        REAL rlon1,rlon
 
        rlon1=rlon
        IF ( rlon1 < 0) rlon1 = rlon1+360
        ii= 1.5 + (rlon1) *(nlon)/360.
        IF ( ii > nlon ) ii= ii-nlon
        match_lon = ii

       END FUNCTION match_lon
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       SUBROUTINE print_mpro_gridboxhr
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   print_mpro_gridboxhr
!
! !Description:
!    Purpose - Diagnostic routine for printing MATCH values
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
!  Notes: 
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!

       INTEGER i

       IF ( mpro%iprint ) THEN 

        print*,'sub. get_mpro_gridboxhr::'
        print*,'mpro%matchnetcdf', trim(mpro%matchnetcdf )
        print*,'mpro%iday' , mpro%iday
        print*,'mpro%ihour', mpro%ihour
        print*,'mpro%rlat ,ilat ', mpro%rlat,ilat
        print*,'mpro%rlon ,ilon ', mpro%rlon,ilon

        print*,'Match Psfc',mgd%Gps(mpro%iday,mpro%ihour,ilon)

!        print'(3x,20f9.4)',mgd%Gv4t(mpro%iday,mpro%ihour, ilon,0:np)
        print'(13x,20a9)', varlab( 1:np,2)

        !! At this point constituent profile Percentages can be inserted into FU Model.
        !! mpro%nv1 -1  =should equal== fi%nv 
        !!  mpro%npc =should equal==  fi%nac
        !!! fi%aprof(1:fi%nv , 1: fi%nac )  = mpro%mvp_comb(1:fi%nv ,1:fi%nac)

        !print'(21x,10a7,3x,7a7)' ,mpro%lab11(1:mpro%np),mpro%lab7(1:mpro%npc)
        print*,' '
        print'(21x,11a7,7x,7a7)' ,mpro%lab11(1:mpro%np),mpro%lab7(1:mpro%npc)

        DO i = 1, mpro%nv1-1
         print '(i5,2f8.2,11f7.1,7x,7f7.1)', i, mpro%pp(i),mpro%pp(i+1) ,mpro%mvp(i,1:mpro%np),mpro%mvp_comb(i,1:mpro%npc)
!          print '(i5,2f8.2,7f7.1)', i, mpro%pp(i),mpro%pp(i+1) ,mpro%mvp_comb(i,1:mpro%npc)
        ENDDO

        print'(10x,A4,12f7.4,1x,8f7.4)','AOTS:',mpro%aots_all(0:mpro%np),mpro%aots_comb(0:mpro%npc)

       ENDIF

       END SUBROUTINE print_mpro_gridboxhr
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       SUBROUTINE conform_to_match_ps
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   conform_to_match_ps
!
! !Description:
!    Purpose - Applies surface pressure boundary condition to MATCH
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
!  Notes: 
!
! !Revision History:
!  Version 1.0   October 1, 2013 - SCCR #1017
!                 Fred Rose, SSAI, Fred.G.Rose@nasa.gov
!                 Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Initial Version of Module
!
! !END
!
!*****************************************************************************
!

       INTEGER i

       ilon=match_lon(mpro%rlon)

       IF ( mpro%iprint )  THEN
        print*,'sub. conform_to_match_ps::'
        print*,'MATCH Psfc:',mgd%Gps(mpro%iday,mpro%ihour,ilon)
       ENDIF

       DO i=1,mpro%nv1
        IF ( mpro%pp(i) > mgd%Gps(mpro%iday,mpro%ihour,ilon) ) EXIT
       ENDDO

       mpro%pp(i) = mgd%Gps(mpro%iday,mpro%ihour,ilon)
       mpro%nv1=i

       END SUBROUTINE conform_to_match_ps
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
END MODULE MATCH_ED4v13
