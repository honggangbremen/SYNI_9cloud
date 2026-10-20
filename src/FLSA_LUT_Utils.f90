MODULE FLSA_LUT_Utils
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   FLSA_LUT_Utils
!
! !Description:
!   Routine ID -
!
!   Purpose - Contains routines for use with the Fu-Liou Surface Albedo (FLSA)
!             Lookup Table (LUT)
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
!    flsa_lut
!    FLSA_LUT_Ingest - Opens, reads, and closes FLSA LUT map
!    flsasnow_lut
!    ilui
!    rlui
!    tf_table
!
!  Modules Used:
!    ceres_status - (CERESlib) Used by Sbr. FL_SfcAlb_LUT_Ingest
!    io           - (CERESlib) Used by Sbr. FL_SfcAlb_LUT_Ingest
!    msg          - (CERESlib) Used by Sbr. FL_SfcAlb_LUT_Ingest
!                                            
!  External Files Used: 
!    PGS_FLSALUTIO_25724.f - Used by Sbr. FLSA_LUT_Ingest 
!
! !Revision History
!  Version 3.5   August 1, 2003 - SCCR #458
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Modified Func. flsa_lut
!                  2. Modified Func. flsasnow_lut
!                  3. Replaced values for coefs_snow array data statement
!                     to correct for a error caused by using a subarctic 
!                     winter atmosphere with a precipitable water value 
!                     of ~0.40 cm.
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Addition of variables associated with the snow scene type
!                  2. Addition of Sbr. flsasnow_lut
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Intitial version of code
!
! !END
!
!*******************************************************************************
!
    USE ceres_status, ONLY: DIRECT_FILE, UNFMT_FILE, READ_FILE
    USE io, ONLY: CloseFile, OpenFile
    USE msg, ONLY: WriteReport
    USE FL_IO_Params, ONLY: Default_REAL4_FL
!
    IMPLICIT NONE
!
    INTEGER nrec,nu0,npw,no3,nta,nspec,nalt,ntyp,ncoef,nv

    PARAMETER(nu0=10,npw=7,no3=3,nta=6,nspec=3,nalt=3,ntyp=4)
    PARAMETER(nrec=nu0*npw*no3*nta*nspec*nalt*ntyp)
    PARAMETER(ncoef=3,nv=7)
    REAL coefs(ncoef,nrec)
    LOGICAL ichoose(nv,2**nv) 
    INTEGER iws_loaded
    SAVE iws_loaded , coefs,ichoose
    DATA iws_loaded /0/

    INTEGER nu0_snow, nct_snow, ncoef_snow, nv_snow, nrec_snow
    PARAMETER(nu0_snow=10,nct_snow=10)
    PARAMETER(nrec_snow=nu0_snow*nct_snow)
    PARAMETER(ncoef_snow=3,nv_snow=2) !!!!!NCOEF
    REAL coefs_snow (ncoef_snow,nrec_snow)
    LOGICAL ichoose_snow (nv_snow,2**nv_snow) 
    SAVE  coefs_snow, ichoose_snow

    DATA coefs_snow /    &
     -0.36699095,  1.08674717, -0.20736353, -0.22018561,  0.72567779, -0.15021178, &
     -0.15888166,  0.58843797, -0.13278687, -0.12489641,  0.51550919, -0.12448887, &
     -0.10320890,  0.46998566, -0.11961292, -0.08814048,  0.43875015, -0.11640438, &
     -0.07704351,  0.41590616, -0.11411036, -0.06853549,  0.39848027, -0.11241116, &
     -0.06179541,  0.38468260, -0.11106282, -0.05632776,  0.37350979, -0.10999677, &
     -0.41243526,  1.16538119, -0.23374112, -0.24151205,  0.76359910, -0.16385026, &
     -0.17195342,  0.61321932, -0.14281596, -0.13385479,  0.53387702, -0.13282336, &
     -0.10972050,  0.48459971, -0.12699199, -0.09302824,  0.45088309, -0.12314683, &
     -0.08078305,  0.42630497, -0.12042382, -0.07140893,  0.40755638, -0.11836605, &
     -0.06400505,  0.39277992, -0.11680035, -0.05800528,  0.38080406, -0.11552736, &
     -0.53244334,  1.37911570, -0.31169841, -0.29950354,  0.86860931, -0.20350957, &
     -0.20769067,  0.68183959, -0.17146295, -0.15841493,  0.58472222, -0.15641658, &
     -0.12759733,  0.52496076, -0.14767507, -0.10647789,  0.48438248, -0.14197533, &
     -0.09108518,  0.45495731, -0.13795292, -0.07936624,  0.43262005, -0.13493763, &
     -0.07013468,  0.41502708, -0.13260289, -0.06268306,  0.40084073, -0.13074392, &
     -0.82623285,  1.94325066, -0.55697644, -0.45644739,  1.16681147, -0.32949987, &
     -0.30628419,  0.87781960, -0.25973308, -0.22665980,  0.72953296, -0.22723019, &
     -0.17751788,  0.63952076, -0.20860237, -0.14420010,  0.57905722, -0.19655319, &
     -0.12012886,  0.53562069, -0.18813142, -0.10192212,  0.50284612, -0.18185724, &
     -0.08768535,  0.47730535, -0.17710866, -0.07623753,  0.45676917, -0.17330611, &
     -1.40189195,  3.25474906, -1.30430961, -0.86830920,  2.04379535, -0.78219235, &
     -0.58352923,  1.47674239, -0.57182932, -0.42345625,  1.17311335, -0.46835864, &
     -0.32350501,  0.98831016, -0.40874493, -0.25580871,  0.86507225, -0.37048641, &
     -0.20713486,  0.77737457, -0.34403077, -0.17054205,  0.71190339, -0.32467455, &
     -0.14205283,  0.66117245, -0.30994502, -0.11927529,  0.62076122, -0.29835993, &
     -2.60605192,  6.62648296, -3.67530918, -1.92306566,  4.73484516, -2.50834608, &
     -1.40872896,  3.53648496, -1.86023724, -1.05213797,  2.75627446, -1.46344864, &
     -0.80771947,  2.23939848, -1.21087670, -0.63558322,  1.88340759, -1.04199815, &
     -0.51000226,  1.62781394, -0.92352974, -0.41527829,  1.43733633, -0.83686870, &
     -0.34173647,  1.29083240, -0.77125484, -0.28321439,  1.17514217, -0.72012240, &
     -8.14713573, 22.94310760,-15.68466568, -6.17575169, 16.69556808,-10.92066860, &
     -4.79344082, 12.92665005, -8.37894249, -3.77439785, 10.29849339, -6.69299459, &
     -3.00696492,  8.37320042, -5.48952675, -2.42369866,  6.93324518, -4.60325146, &
     -1.97648156,  5.84059572, -3.93759298, -1.62980485,  4.99997425, -3.42935634, &
     -1.35728860,  4.34276915, -3.03423524, -1.14002681,  3.82115126, -2.72208834, &
     -6.13164854,  9.39767647,  0.00000000, -5.20671320,  7.64909077,  0.00000000, &
     -4.49449253,  6.57322311,  0.00000000, -3.91970587,  5.77625179,  0.00000000, &
     -3.44474792,  5.14517212,  0.00000000, -3.04562378,  4.62752914,  0.00000000, &
     -2.70567656,  4.19308043,  0.00000000, -2.41308308,  3.82266641,  0.00000000, &
     -2.15908527,  3.50296426,  0.00000000, -1.93707681,  3.22459912,  0.00000000, &
    -17.89592361, 26.28153229,  0.00000000,-15.34802628, 21.51605034,  0.00000000, &
    -13.45832062, 18.65992355,  0.00000000,-11.95122910, 16.56675911,  0.00000000, &
    -10.71246719, 14.91865253,  0.00000000, -9.67455292, 13.57167912,  0.00000000, &
     -8.79162312, 12.44341183,  0.00000000, -8.03170586, 11.48218536,  0.00000000, &
     -7.37167406, 10.65285492,  0.00000000, -6.79212761,  9.92780972,  0.00000000, &
    -59.04664993, 84.81874084,  0.00000000,-50.47857285, 69.06404877,  0.00000000, &
    -44.42715073, 59.93613052,  0.00000000,-39.67837143, 53.33734512,  0.00000000, &
    -35.80409241, 48.17795944,  0.00000000,-32.57212448, 43.97987747,  0.00000000, &
    -29.82963181, 40.47295380,  0.00000000,-27.48365021, 37.50537109,  0.00000000, &
    -25.43045616, 34.92379761,  0.00000000,-23.63710976, 32.68038177,  0.00000000  /
!        
!
    INCLUDE 'PGS_FLSALUTIO_25724.f'
!
!
CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE FLSA_LUT_Ingest (PCF_LID, StreamID)                   ! I
!
!***************************************************************************
!
! !F90
!
! !Name:
!   FLSA_LUT_Ingest 
! 
! !Description:
!   Routine ID - 5.1.4.5
!
!   Purpose - To ingest the look up table for the Fu-Liou surface albedo 
!             two-stream or four-stream coefficients
!
!
! !Input Parameters:
!    PCF_LID  - Logical ID for mapping into the PCF for file name
!    StreamID - Indicates whether to ingest the two-stream coefficients or
!               the four-stream coefficients
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
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Intitial version of code
!
! !END
!
!***************************************************************************
!
      INTEGER, INTENT (IN)  :: PCF_LID
      INTEGER, INTENT (IN)  :: StreamID
!
      INTEGER            :: IOS
      INTEGER, PARAMETER :: ReportSucc_Flag = 1
      INTEGER            :: Rlength
      INTEGER            :: tftable_outerloop
      INTEGER            :: Unit_Num
!
!
      RLength = ncoef * nrec * 4
      CALL OpenFile (LogicID = PCF_LID, version=StreamID,                & ! I
                     mode = READ_FILE, access = DIRECT_FILE,             & ! I
                     format = UNFMT_FILE, reclen = RLength,              & ! I
                     report_success = ReportSucc_Flag,                   & ! I
                     handle = Unit_Num, status = IOS)                      ! O
!
      IF (IOS /= 0) THEN
!     *** File opening error
!
          CALL WriteReport (FLSALUTIO_E_LUT_OPEN,                       & ! I
                            FuncName = "FL_SfcAlb_LUT_Ingest ()")         ! I
!
      END IF
!
!
      READ (Unit_Num,rec=1, IOSTAT=IOS) coefs
!
      IF (IOS /= 0) THEN
!     *** File read error
!
          CALL WriteReport (FLSALUTIO_E_LUT_READ,                       & ! I
                            FuncName = "FLSA_LUT_Ingest ()")              ! I
!
      END IF
!
!
      CALL CloseFile (Unit_Num, status=IOS)
!
      IF (IOS /= 0) THEN
!     *** File closing error
!
          CALL WriteReport (FLSALUTIO_E_LUT_CLOSE,                      & ! I
                            FuncName = "FLSA_LUT_Ingest ()")              ! I
!
      END IF
!
!
      tftable_outerloop = 2**nv
      call tf_table (nv, tftable_outerloop, ichoose)                     ! I
!
      iws_loaded = StreamID
!
      tftable_outerloop = 2**nv_snow
      call tf_table (nv_snow, tftable_outerloop, ichoose_snow)           ! I
!
!
!
      END SUBROUTINE FLSA_LUT_Ingest
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
real function flsa_lut(toa,u0,pw,o3,ta,spec,psfc,wssa)
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   flsa_lut
!
! !Team-Unique Header:
!
! !Revision History
!  Version 3.5   August 1, 2003 - SCCR #458
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Added ichoose array to argument list for Func. rlui
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Intitial version of code
!
! !END
!
!*******************************************************************************
!
implicit none

real toa,u0,pw,o3,ta,spec,psfc,wssa
real rlogmin,rnode

integer iv
!integer ifile
integer nva(nv),iva(nv)
real vmm(2,nv),rng(nv),val(nv),rva(nv)

parameter(rlogmin=1.0E-10)

data nva/nu0,npw,no3,nta,nspec,nalt,ntyp/
data vmm/ 0.1 , 1.0, &
        -4 ,   2, &
        200, 500, &
                   -5,    0, &
                   -1,    1, &
    628, 1013, &
                    0.02,0.131 / !Sulfate,Continental , 1um Dust, 2um Dust
! ---
!if( ifile .ne. iws_loaded ) call flsa_lut_loader(ifile)
! ---
IF (u0 < 0.0 .OR. u0 == Default_REAL4_FL .OR. toa == Default_REAL4_FL) THEN
      flsa_lut = Default_REAL4_FL
  RETURN
    ENDIF
val(1:nv)=vmm(1,1:nv)
val(1)=u0
if(pw >= rlogmin) val(2)= log(pw) 
val(3)= o3
if(ta >= rlogmin) val(4)= log(ta)

val(5)= spec
val(6)= ifix ( psfc)
 val(7)= wssa

do iv=1,nv
if (val(iv) < vmm(1,iv) ) val(iv) = vmm(1,iv)
if (val(iv) > vmm(2,iv) ) val(iv) = vmm(2,iv)

rng = vmm(2,iv)-vmm(1,iv)
rnode=  (val(iv)-vmm(1,iv)) /rng(iv)
rva(iv) = 1+ rnode*(nva(iv)-1)
iva(iv) = nint(rva(iv))
enddo

flsa_lut = rlui(toa,nva,nv,rva, coefs,ncoef,nrec, ichoose)
!##flsa_lut = rlui(toa,nva,nv,rva, coefs,ncoef,nrec,psfc)
 
return
end function flsa_lut
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
real function flsasnow_lut(toa,u0,cldlntau,cldfrac)
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   flsasnow_lut
!
!
! !Input Parameters:
!    INPUT TOA ALBEDO FOR Cloud Sky Snow classified scene..
!    toa      - TOA ALBEDO (0-1)
!    u0       - Cosine Solar Zenith Angle (0-1)
!    cldlntau - Natural log of Cloud optical depth ( -4:5)
!    cldfrac  - Cloud Fraction(0-1)
!
! !Output Parameters:
!     Broadband Surface Albedo 
!
! !Team-Unique Header:
!
! !Revision History
!  Version 3.5   August 1, 2003 - SCCR #458
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Added ichoose_snow array to argument list for Func. rlui
!  Version 3.4   December 20, 2002 - SCCR #408
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 Intitial version of code
!
! !END
!
!***************************************************************************
!
implicit none

real toa, u0, cltau
real cldlntau, cldfrac

real rlogmin, rnode

integer iv
integer nva_snow ( nv_snow), iva_snow ( nv_snow)
real vmm_snow ( 2, nv_snow), rng ( nv_snow), val ( nv_snow)
real rva (nv_snow)
parameter (rlogmin=1.0E-10)

data nva_snow / nu0_snow, nct_snow /
data vmm_snow / 0.1 , 1.0, &
            -4.0 , 5.0  /

    IF (u0 < 0.0 .OR. u0 == Default_REAL4_FL .OR. toa == Default_REAL4_FL) then
      flsasnow_lut = Default_REAL4_FL
  RETURN
    ENDIF

!! WEIGHT lnCldtau By CLDFRAC
cltau= (cldfrac *cldlntau )+(1.0-cldfrac)*(-4.0)  
val(1:nv_snow)=vmm_snow(1,1:nv_snow)
val(1)=u0
        val(2)= cltau 


do iv=1,nv_snow
    if (val ( iv) < vmm_snow ( 1, iv) ) val ( iv) = vmm_snow ( 1, iv)
    if (val ( iv) > vmm_snow ( 2, iv) ) val ( iv) = vmm_snow ( 2, iv)

    rng            = vmm_snow ( 2, iv) - vmm_snow ( 1, iv)
    rnode          =  (val ( iv) - vmm_snow ( 1, iv)) / rng ( iv)
    rva ( iv)      = 1 + rnode * (nva_snow ( iv) - 1)
    iva_snow ( iv) = NINT (rva ( iv))
enddo

flsasnow_lut = rlui (toa, nva_snow, nv_snow, rva,                   &
                             coefs_snow, ncoef_snow, nrec_snow, ichoose_snow)
if (flsasnow_lut < 0.0 .OR. &
            flsasnow_lut > 1.0 ) flsasnow_lut = -9999.


return
end function flsasnow_lut
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
subroutine tf_table(n1,n2,ib)
implicit none
integer n1,n2,i,ii
logical ib(n1,n2)
do i=1,n2
do ii=1,n1
ib(ii,i)=btest(i-1,ii-1)
enddo
!print*,i,ib(1:n1,i)
enddo
return
end subroutine tf_table
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
real function rlui(toa,nva,nv,rva, coefs,ncoef,nrec, ichoose_gen)
!
!*******************************************************************************
!
! !F90
!
! !Name:
!   rlui
!
! !Team-Unique Header:
!
! !Revision History
!  Version 3.5   August 1, 2003 - SCCR #458
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. Added ichoose_gen array to argument list
!  Version 3.0   March 9, 2001 - SCCR #246  
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  Intitial version of code
!
! !END
!
!*******************************************************************************
!
implicit none
real toa,swx,top,sfc_toa,x
!###real toa,swx,top,sfc_toa,x,psfc
integer nv,ncoef,nrec,i,j,ik,k
integer  nva(nv),iva(nv,2**nv)
real wx
logical ichoose_gen(nv,2**nv) 

real coefs(ncoef,nrec)
real wb(nv),wt(nv),rva(nv)

integer ibase(nv),itop(nv)

 ibase = floor(rva)
 where( ibase < 1) ibase=1

 itop= ceiling(rva)
 where( itop > nva) itop=nva

wb=1.0-(rva-ibase)
wt=1.0-wb

!print'(A10,10f5.1)','Exact',rva(1:nv)
!print'(A10,10I5)','BASE',ibase
!print'(A10,10I5)','TOP',itop
!print'(A10,10f5.1)','Wb',wb
!print'(A10,10f5.1)','Wt',wt

swx=0
rlui =0
ik=0

NODE : do j=1,2**nv
wx=1.0

 DIMENSION: do i=1,nv

 if( ichoose_gen(i,j) ) then
  iva(i,j) = itop(i)
  wx=wx*wt(i)
 else
  iva(i,j) = ibase(i)
  wx=wx*wb(i)
 endif

 enddo DIMENSION

 if( wx > 1.0E-10 ) then
 ik=ik+1
 k=ilui(nva,nv,iva(1,j))

 sfc_toa=  coefs(1,k) + coefs(2,k)*toa + coefs(3,k)*toa*toa
 x = toa + sfc_toa
!## x = toa + sfc_toa *(psfc/1013.25)
 swx       = swx  +     wx
 rlui      = rlui + x * wx

!print'(2I4,6I3,I10,4f10.5)',ik,j,iva(1:nv,j),k,x,wx,swx,rlui
if (swx >= .9999)exit
endif

enddo NODE
rlui = rlui/swx
return
end function rlui
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
integer function ilui(nva,nv,iva)
implicit none
integer nv,i,k
integer  nva(nv),iva(nv)
integer  mva(nv)

mva(1)=nva(1)
do i=2,nv
 mva(i)=mva(i-1)*nva(i)
enddo

k= iva(1)
do i=2,nv
k= k+ (iva(i)-1)*mva(i-1)
enddo

ilui=k
return
end function ilui
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
END MODULE FLSA_LUT_Utils
