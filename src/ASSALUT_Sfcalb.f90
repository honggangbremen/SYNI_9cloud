MODULE ASSALUT_Sfcalb
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   ASSALUT_SFCALB
!
! !Description:
!   Routine ID -
!
!   Purpose - Routines to handle processing of surface albedo lookup table
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
!   ASSALUT_LUT_Ingest - Opens, reads, and closes look up table
!   avg_cloud_condition -  
!   assalut_lut -  
!   assalut_load - Loads variables into assalut structure 
!        
!
!  Modules Used:
!    ceres_status - (CERESlib) Used by Sbr. FL_SfcAlb_LUT_Ingest
!    io           - (CERESlib) Used by Sbr. FL_SfcAlb_LUT_Ingest
!    msg          - (CERESlib) Used by Sbr. FL_SfcAlb_LUT_Ingest
!                                            
!  External Files Used: 
!    
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
! *****************************************************************************

 USE ceres_status, ONLY: DIRECT_FILE, UNFMT_FILE, READ_FILE
 USE io, ONLY: CloseFile, OpenFile
 USE msg, ONLY: WriteReport
 USE FL_IO_Params
 USE FLSA_LUT_Utils, ONLY: ilui, tf_table
 ! USE SARB_Var
 use nclouds_params ! HG 2025.0722
 USE Profile_Params, ONLY: MaxLev_Cld

 IMPLICIT NONE

 INTEGER, PARAMETER:: nalb=7

 INTEGER, PARAMETER :: nv=8 
 INTEGER, PARAMETER :: ncldfra = 2
 INTEGER, PARAMETER :: ncldhgt = 3
 INTEGER, PARAMETER :: ncldpsz = 4
 INTEGER, PARAMETER :: ncldtau = 12
 INTEGER, PARAMETER :: nel     = 3
 INTEGER, PARAMETER :: no3     = 2
 INTEGER, PARAMETER :: npw     = 5
 INTEGER, PARAMETER :: nu0     = 10

 INTEGER, PARAMETER :: nrec = ncldfra * ncldhgt * ncldpsz * ncldtau * nel * no3 * npw * nu0
 INTEGER, PARAMETER :: mxn=12
 REAL   , PARAMETER :: scale =10000. , rlogmin=1.0E-10

 INTEGER*2, SAVE :: itable(nalb,2,nrec)   
 LOGICAL, SAVE :: ichoose(nv,2**nv) 

 INTEGER, DIMENSION(nv) ::  nva,iva
 REAL stab(nalb,2)
 REAL anodes(mxn,nv),val(nv)

 DATA nva/ncldfra ,ncldhgt ,ncldpsz ,ncldtau ,nel ,no3 ,npw ,nu0/

 LOGICAL, SAVE :: inital
 DATA inital/.TRUE./

 TYPE assalut_TYPE

  SEQUENCE
! inputs
!  REAL cfra(MaxLev_Cld)   ! Cloud Fraction (0-1)
!  REAL chgt(MaxLev_Cld)   ! Cloud Top Height (m) (0- 14000)
!  REAL cpz (MaxLev_Cld)   ! Cloud Particle size( 5-30 water) (30-180 ice)
!  REAL ctau(MaxLev_Cld)   ! Cloud Tau ( 0-128)

! HG-2025.0722  
  REAL cfra(nclouds)   ! Cloud Fraction (0-1)
  REAL chgt(nclouds)   ! Cloud Top Height (m) (0- 14000)
  REAL cpz (nclouds)   ! Cloud Particle size( 5-30 water) (30-180 ice)
  REAL ctau(nclouds)   ! Cloud Tau ( 0-128)

  REAL elev	          ! Sfc Elevation (m) (0 -8000)
  REAL o3 	          ! Ozone DU ( 200 -500)
  REAL pw 	          ! Precipitable water (cm) (0-7)
  REAL u0 	          ! CosSol (0-1)
  REAL toa		  ! TOA ALBEDO (0-1)

! output
  REAL sfc     !! SFC ALBEDO (0-1)
  REAL cfra_mn
  REAL chgt_mn
  REAL cpz_mn
  REAL ctau_mn

 END TYPE assalut_TYPE

 TYPE ( assalut_TYPE ) assalut
 
 DATA  anodes(1:2 ,1) /0.0,1.0/  !! Cloud Frac
 DATA  anodes(1:3 ,2)/1000.000,5000.000,14000.000/ !! Cloud Height
 DATA  anodes(1:4 ,3)/8.000,30.000,30.01,180.000/ ! Cloud Particle size /Phase
 DATA  anodes(1:12,4)/-2.000,-1.000,-0.500, 0.000,0.500,1.000 ,1.500,2.000,2.500,3.000,4.000,5.000/ !lnCldtau
 DATA  anodes(1:3 ,5)/0.000,1250.000,4200.000/ !SfcElevation(m)
 DATA  anodes(1:2 ,6)/200.000,500.000/ !ozone(du)
 DATA  anodes(1:5, 7)/-2.000,-1.000,0.000,1.000,2.000/ !lnPw
 DATA  anodes(1:10,8)/0.100, 0.200, 0.300,0.400,0.500, 0.600 , 0.700 ,0.800,0.900, 1.000/ !Cos Sol

 INCLUDE 'PGS_FLSALUTIO_25724.f'

 CONTAINS
!
!***************************************************************************
!
 SUBROUTINE ASSALUT_LUT_Ingest(PCF_LID)
!
!***************************************************************************
!
! !F90
!
! !Name:
!   ASSALUT_LUT_Ingest 
! 
! !Description:
!   Routine ID - 5.1.4.5
!
!   Purpose - Opens, reads, and closes look up table
!             
!
! !Input Parameters:
!    PCF_LID  - Logical ID for mapping into the PCF for file name
!    
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!   ceres_status   - (CERESlib) Parameters: DIRECT_FILE, READ_FILE, UNFMT_FILE
!   FLSA_LUT_Utils - (SARBlib)  Sbr. tf_table
!                               Params: coefs, coefs_snow, ichoose, 
!                                       ichoose_snow, iws_loaded, ncoef, 
!                                       nrec, nv, nv_snow
!   io             - (CERESlib) Sbr. CloseFile, OpenFile
!   msg            - (CERESlib) Sbr. WriteReport
!
!  External Files Used:
!   PGS_FLSALUTIO_25724.f - Parameters: FLSALUTIO_E_LUT_CLOSE,
!                                       FLSALUTIO_E_LUT_OPEN,
!                                       FLSALUTIO_E_LUT_READ
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
 INTEGER, INTENT (IN) :: PCF_LID	
 INTEGER              :: IOS
 INTEGER, PARAMETER   :: ReportSucc_Flag = 1
 INTEGER 	      :: Rlength
 INTEGER 	      :: Unit_Num

 Rlength = nalb * nrec * 2 * 2
	 
 CALL OpenFile (LogicID = PCF_LID, mode = READ_FILE, access = DIRECT_FILE, & ! I
                format = UNFMT_FILE, reclen = RLength,                     & ! I
                report_success = ReportSucc_Flag,                          & ! I
                handle = Unit_Num, status = IOS)                             ! O
!
 IF (IOS /= 0) THEN
!  *** File opening error
  CALL WriteReport (FLSALUTIO_E_LUT_OPEN,                       & ! I
                      FuncName = "ASSALUT_LUT_Ingest ()")         ! I
 END IF
!
!
 READ (Unit_Num, rec=1, IOSTAT=IOS) itable

 IF (IOS /= 0) THEN
	CALL WriteReport (FLSALUTIO_E_LUT_READ, 		     & ! I
				FuncName = "ASSALUT_LUT_Ingest ()")    ! I
 END IF
!
!
 CALL CloseFile (Unit_Num, status=IOS)
!
 IF (IOS /= 0) THEN
!  *** File closing error
  CALL WriteReport (FLSALUTIO_E_LUT_CLOSE,                      & ! I
                 FuncName = "ASSALUT_LUT_Ingest ()")              ! I
 END IF
!
 END SUBROUTINE ASSALUT_LUT_Ingest
!
!***************************************************************************
!
 SUBROUTINE avg_cloud_condition (assalut)
!
!***************************************************************************
!
! !F90
!
! !Name:
!   avg_cloud_condition 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!
 INTEGER ic ,ipha
 REAL pzsum(2),phacnt(2)
 REAL wgt ,tmp
! REAL tauwgt(MaxLev_Cld),swgt,wgt2
 REAL tauwgt(nclouds),swgt,wgt2  !HG-2025.0722

 TYPE ( assalut_TYPE ), INTENT(INOUT) ::  assalut

 WHERE (assalut%ctau > exp(-2.) .AND. assalut%ctau < exp(3.) ) tauwgt =  (log(assalut%ctau) +2) /5.0
 WHERE (assalut%ctau <= exp(-2.) )  tauwgt = 0.0001
 WHERE (assalut%ctau >= exp( 3.) )  tauwgt = 1.00

 val(1) = SUM(assalut%cfra, mask = assalut%cfra > 0 .AND. assalut%cfra <= 1.0)
 swgt = SUM( assalut%cfra * tauwgt, mask = assalut%cfra > 0 .AND. assalut%cfra <= 1.0)
 
 IF ( val(1) > 0 ) THEN 

   val(2:4) = 0.0
   phacnt = 0
   pzsum = 0.0

   DO ic = 1, nclouds !MaxLev_Cld  !HG-2025.0722
  
!  This access of SARB VAR destroyed the modular nature of the code and made it UNworkable for SYNI(Beta4)  
!    assalut%cpz(ic) = -1.0
!  Phase_mn / LiquidPart_size / IcePart_size NOT from input structure
!     IF (Phase_mn(ic) < 1.5 .AND. Phase_mn(ic) >= 1.0) assalut%cpz(ic) = LiquidPart_size(InitPass,ic)
!     IF (Phase_mn(ic) >= 1.5 .AND. Phase_mn(ic) <= 2.0) assalut%cpz(ic) = IcePart_size(InitPass,ic)

     WHERE (assalut%cpz > 1.0E+10) assalut%cpz = -1.0
     IF ( assalut%cfra(ic) > 0 .AND. assalut%cfra(ic) <= 1.0) THEN
       wgt = assalut%cfra(ic) / val(1)
       wgt2= assalut%cfra(ic) * tauwgt(ic)/swgt
       tmp = anodes(1,4) 
       IF ( assalut%ctau(ic) > exp(tmp)  ) tmp = log(assalut%ctau(ic))
       val(4) = val(4) + tmp * wgt
       val(2) = val(2) + assalut%chgt(ic)*wgt2
       IF ( assalut%cpz(ic) <= 30.) ipha = 1
       IF ( assalut%cpz(ic)  > 30.) ipha = 2
       phacnt(ipha) = phacnt(ipha)+ wgt2
       pzsum(ipha) =  pzsum(ipha) + assalut%cpz(ic) * wgt2
     ENDIF
!	
   END DO
   
   ipha = 1
   IF ( phacnt(2) > phacnt(1) ) ipha = 2
   val(3) = pzsum(ipha) / phacnt(ipha) 
   assalut%cfra_mn = val(1)
   assalut%chgt_mn = val(2)
   assalut%cpz_mn  = val(3)
   assalut%ctau_mn = exp(val(4))
 ELSE
   val(2:4) = anodes(1,2:4) !!! Min Value node
   assalut%cfra_mn = 0.0
   assalut%chgt_mn = -1.0
   assalut%cpz_mn  = -1.0
   assalut%ctau_mn = -1.0
 ENDIF
!
 END SUBROUTINE avg_cloud_condition
!
!*********************************************************
!
 SUBROUTINE assalut_lut (assalut, sfcalb0) 
!
!***************************************************************************
!
! !F90
!
! !Name:
!   assalut_lut 
! 
! !Description:
!   Routine ID - 
!
!   Purpose - 
!             
!
!
! !Input Parameters:
!      - 
!        
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
!  Modules Used: 
!
!
!  External Files Used:
!
!
! !Revision History
!  Version 1.0   June 15 , 2005 
!                Initial version of code
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                
!
! !END
!
!***************************************************************************
!

 REAL swx , wx 
 REAL rmin,rmax
 INTEGER i,j,ik,k , iv ,in
 INTEGER  ivax(nv,2**nv)
 REAL, INTENT(OUT) :: sfcalb0
 REAL, DIMENSION(nv) :: wb,wt,rva
 INTEGER, DIMENSION(nv) :: ibase,itop
 
 TYPE ( assalut_TYPE ) assalut	

 IF ( inital ) CALL tf_table(nv,2**nv,ichoose);inital = .FALSE.

 val(1:nv)=anodes(1,1:nv)

 CALL avg_cloud_condition (assalut)

 val(5) = assalut%elev
 val(6) = assalut%o3
 IF ( assalut%pw > exp(val(7) )) val(7)= log(assalut%pw) 
 val(8) = assalut%u0

 VSET : DO iv=1,nv 
   rmin = anodes(1,iv)
   rmax = anodes(nva(iv),iv)

   IF (val(iv) <= rmin ) THEN 
     rva(iv) = 1
   ELSEIF (val(iv) >= rmax ) THEN
     rva(iv) = nva(iv)
   ELSE
     FNODES : DO in = 1,nva(iv)-1
       IF( val(iv) >= anodes(in,iv) .AND. val(iv) < anodes(in+1,iv) )  &
  	    rva(iv)= in + ( val(iv) - anodes(in,iv)) /                 &
                  (anodes(in+1,iv) - anodes(in,iv))
     END DO FNODES
   ENDIF

 END DO VSET

 ibase = floor(rva)
 WHERE( ibase < 1) ibase=1

 itop = ceiling(rva)
 WHERE( itop > nva) itop = nva

 wb = 1.0 - (rva - ibase)
 wt = 1.0 - wb


 swx = 0
 ik = 0
 stab = 0

 NODE : DO j = 1,2**nv
   wx = 1.0

   DIMNSION: DO i = 1,nv
     IF ( ichoose(i,j) ) THEN
      ivax(i,j) = itop(i)
      wx = wx * wt(i)
     ELSE
      ivax(i,j) = ibase(i)
      wx = wx * wb(i)
     ENDIF
   END DO DIMNSION

   IF ( wx > 1.0E-10 ) THEN
     ik = ik+1
     k = ilui(nva,nv,ivax(1,j))
     swx= swx + wx
     stab(1:nalb,1:2) =  stab(1:nalb,1:2) + (itable(1:nalb,1:2,k) / scale) * wx 
     IF (swx >= .9999) EXIT
   ENDIF

 END DO NODE

 IF ( swx .NE. 1.0000) stab = stab / swx !!! Incase of partial weightings

 DO i = 1,nalb - 1
  IF ( assalut%toa >= stab(i,2) .AND. assalut%toa < stab(i+1,2) .AND. stab(i,2) .NE. stab(i+1,2) ) THEN
    wx = (assalut%toa - stab(i,2)) / ( stab(i+1,2) - stab(i,2) )
    assalut%sfc = stab(i,1) + wx * ( stab(i+1,1) - stab(i,1) )
    EXIT
  ENDIF
 END DO

 IF ( assalut%toa < minval(stab(1:nalb,2)) ) assalut%sfc = -0.01
 IF ( assalut%toa > maxval(stab(1:nalb,2)) ) assalut%sfc = 1.01

!
 sfcalb0 = assalut%sfc   ! this line required for output
!
 END SUBROUTINE  assalut_lut
!
!****************************************************************************
!
END MODULE ASSALUT_Sfcalb










