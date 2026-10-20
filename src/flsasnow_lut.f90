MODULE FLSASNOW_SURFACE_ALBEDO
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   FLSASNOW_SURFACE_ALBEDO
!
! !Description:
!
!    Purpose - 
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
!    flsa0404_lut         - 
!    tf_table             - 
!    rlui                 -  
!    ilui                 - 
!
!  Modules Used:
!
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
 IMPLICIT NONE
 
 INTEGER nrec,nu0,ncoef,nv,nct

 PARAMETER(nu0=10,nct=10)
 PARAMETER(nrec=nu0*nct)
 PARAMETER(ncoef=3,nv=2) !!!!!NCOEF

 REAL coefs(ncoef,nrec)
 LOGICAL ichoose(nv,2**nv) 

 SAVE   coefs,ichoose

! Load FLSA file :flsasnow.23.coef              

  DATA coefs /    &
 -0.33283722,  0.85506755, -0.15511213, -0.20326212,  0.57169145, -0.11911894, &
 -0.14775497,  0.46084565, -0.10778747, -0.11658969,  0.40095237, -0.10227565, &
 -0.09656601,  0.36326095, -0.09902339, -0.08259209,  0.33728299, -0.09686867, &
 -0.07227615,  0.31827599, -0.09535275, -0.06434213,  0.30373469, -0.09419346, &
 -0.05805182,  0.29225358, -0.09330896, -0.05293427,  0.28295296, -0.09259457, &
 -0.37733823,  0.92910737, -0.17676769, -0.22415611,  0.60747916, -0.13066429, &
 -0.16058065,  0.48431870, -0.11639944, -0.12539163,  0.41843787, -0.10952097, &
 -0.10296571,  0.37720904, -0.10546507, -0.08740161,  0.34891838, -0.10280096, &
 -0.07595240,  0.32826093, -0.10090946, -0.06717275,  0.31249115, -0.09946762, &
 -0.06022945,  0.30007669, -0.09838852, -0.05459083,  0.29002926, -0.09751225, &
 -0.49547490,  1.13081348, -0.24165693, -0.28106722,  0.70653754, -0.16443051, &
 -0.19568491,  0.54930520, -0.14113660, -0.14952905,  0.46675590, -0.13005577, &
 -0.12054551,  0.41570744, -0.12358625, -0.10062990,  0.38095537, -0.11935125, &
 -0.08608720,  0.35571703, -0.11633682, -0.07500382,  0.33659399, -0.11412704, &
 -0.06626599,  0.32153121, -0.11239412, -0.05920034,  0.30939656, -0.11101638, &
 -0.78760874,  1.66593719, -0.45152268, -0.43564811,  0.98771387, -0.27307269, &
 -0.29272264,  0.73457670, -0.21796121, -0.21669988,  0.60410547, -0.19210535, &
 -0.16968402,  0.52469254, -0.17721975, -0.13776930,  0.47129685, -0.16758220, &
 -0.11469258,  0.43291977, -0.16083388, -0.09723803,  0.40403756, -0.15586925, &
 -0.08356382,  0.38143873, -0.15201780, -0.07256798,  0.36333111, -0.14897656, &
 -1.36377406,  2.91096711, -1.10820508, -0.84317183,  1.81418455, -0.67043763, &
 -0.56632257,  1.29930305, -0.49294198, -0.41075051,  1.02323711, -0.40537623, &
 -0.31359854,  0.85506290, -0.35484257, -0.24778435,  0.74284297, -0.32236266, &
 -0.20045920,  0.66297072, -0.29988584, -0.16487339,  0.60336179, -0.28347322, &
 -0.13717815,  0.55717856, -0.27094227, -0.11502126,  0.52040410, -0.26110640, &
 -2.55469394,  6.06780052, -3.19899273, -1.88734186,  4.34718609, -2.20573401, &
 -1.38161111,  3.23858905, -1.64096093, -1.03118277,  2.51482844, -1.29344094, &
 -0.79120463,  2.03504634, -1.07184756, -0.62230116,  1.70452750, -0.92356229, &
 -0.49912101,  1.46717250, -0.81946939, -0.40623292,  1.29028416, -0.74331462, &
 -0.33412659,  1.15421414, -0.68562657, -0.27674007,  1.04675198, -0.64065790, &
 -8.05154037, 21.40889168,-13.89083385, -6.10786629, 15.65277767, -9.76969719, &
 -4.73659468, 12.12463188, -7.52088022, -3.72606564,  9.65389633, -6.01719856, &
 -2.96623087,  7.84173536, -4.93991995, -2.38957477,  6.48599529, -4.14508390, &
 -1.94796276,  5.45739651, -3.54760695, -1.60582364,  4.66581583, -3.09091091, &
 -1.33706844,  4.04707289, -2.73574972, -1.12288523,  3.55607414, -2.45518327, &
 -6.14669895,  8.88885212,  0.00000000, -5.21556187,  7.26126719,  0.00000000, &
 -4.49644756,  6.24444103,  0.00000000, -3.91682172,  5.48696136,  0.00000000, &
 -3.43890834,  4.88551521,  0.00000000, -3.03808236,  4.39136314,  0.00000000, &
 -2.69725037,  3.97618699,  0.00000000, -2.40433192,  3.62201571,  0.00000000, &
 -2.15024352,  3.31600714,  0.00000000, -1.92840159,  3.04956746,  0.00000000, &
-18.16087723, 25.30753708,  0.00000000,-15.53756714, 20.77580261,  0.00000000, &
-13.59711266, 18.03330040,  0.00000000,-12.05440903, 16.01593971,  0.00000000, &
-10.79056549, 14.42468929,  0.00000000, -9.73438931, 13.12256050,  0.00000000, &
 -8.83838367, 12.03168392,  0.00000000, -8.06864357, 11.10186863,  0.00000000, &
 -7.40029144, 10.29819965,  0.00000000, -6.81463099,  9.59611893,  0.00000000, &
-61.11161041, 83.50109863,  0.00000000,-51.97213745, 68.00940704,  0.00000000, &
-45.59751511, 59.02483368,  0.00000000,-40.62960052, 52.52924728,  0.00000000, &
-36.59585571, 47.44844055,  0.00000000,-33.24335480, 43.31328964,  0.00000000, &
-30.41105652, 39.86392593,  0.00000000,-27.97463799, 36.91910553,  0.00000000, &
-25.87371063, 34.39312744,  0.00000000,-24.03285599, 32.18667221,  0.00000000/ 

 CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 REAL FUNCTION flsasnow_lut(toa,u0,cldlntau,cldfrac)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   flsasnow_lut
!
! !Description:
!    Purpose - 
!
! 
! !Input Parameters:
!   toa      - TOA ALBEDO (0-1)
!   u0       - Cosine Solar Zenith Angle (0-1)
!   cldlntau - Natural log of Cloud optical depth ( -4:5)
!   cldfrac  - Cloud Fraction(0-1)
!
! !Output Parameters:
!   Broadband Surface Albedo 
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

 REAL toa,u0,cltau
 REAL cldlntau,cldfrac

 REAL rlogmin,rnode

 INTEGER iv
 INTEGER nva(nv),iva(nv)
 REAL vmm(2,nv),rng(nv),val(nv),rva(nv)
 
 LOGICAL, SAVE :: inital
 
 DATA inital/.true./
 
 PARAMETER(rlogmin=1.0E-10)

 DATA nva/nu0,nct/
 DATA vmm/ 0.1 , 1.0, &
          -4 ,   5  /

 IF ( inital )THEN
  CALL tf_table(nv,2**nv,ichoose)
  inital =.false.
 ENDIF

 cltau= (cldfrac *cldlntau ) + (1.0-cldfrac)* (-4.0)  !! WEIGHT lnCldtau By CLDFRAC
 val(1:nv)=vmm(1,1:nv)
 val(1)=u0
 val(2)= cltau 


 DO iv=1,nv
  IF (val(iv) < vmm(1,iv) ) val(iv) = vmm(1,iv)
  IF (val(iv) > vmm(2,iv) ) val(iv) = vmm(2,iv)

  rng = vmm(2,iv)-vmm(1,iv)
  rnode=  (val(iv)-vmm(1,iv)) /rng(iv)
  rva(iv) = 1+ rnode*(nva(iv)-1)
  iva(iv) = nint(rva(iv))
 ENDDO

 flsasnow_lut = rlui(toa,nva,nv,rva, coefs,ncoef,nrec)
 IF (flsasnow_lut < 0.0 .OR. &
     flsasnow_lut > 1.0 ) flsasnow_lut = -9999.

 RETURN

 END FUNCTION flsasnow_lut
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 SUBROUTINE tf_table(n1,n2,ib)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   tf_table
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
 IMPLICIT NONE

 INTEGER n1,n2,i,ii
 LOGICAL ib(n1,n2)

 DO i=1,n2
  DO ii=1,n1
   ib(ii,i)=btest(i-1,ii-1)
  END DO
 END DO

 RETURN

 END SUBROUTINE tf_table
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 REAL FUNCTION rlui(toa,nva,nv,rva, coefs,ncoef,nrec)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   rlui
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
 IMPLICIT NONE

 REAL toa,swx,top,sfc_toa,x
 INTEGER nv,ncoef,nrec,i,j,ik,k
 INTEGER nva(nv),iva(nv,2**nv)
 REAL wx

 REAL coefs(ncoef,nrec)
 REAL wb(nv),wt(nv),rva(nv)

 INTEGER ibase(nv),itop(nv)

 ibase = floor(rva)
 WHERE( ibase < 1) ibase=1

 itop= ceiling(rva)
 WHERE( itop > nva) itop=nva

 wb=1.0-(rva-ibase)
 wt=1.0-wb

 swx=0
 rlui =0
 ik=0

 NODE : DO j=1,2**nv
  wx=1.0

  DIMENSION: DO i=1,nv

   IF( ichoose(i,j) ) THEN
    iva(i,j) = itop(i)
    wx=wx*wt(i)
   ELSE
    iva(i,j) = ibase(i)
    wx=wx*wb(i)
   ENDIF

  END DO DIMENSION

  IF( wx > 1.0E-10 ) THEN

   ik=ik+1
   k=ilui(nva,nv,iva(1,j))

   sfc_toa=  coefs(1,k) + coefs(2,k)*toa + coefs(3,k)*toa*toa  !!!!!NCOEF
   x = toa + sfc_toa 
   swx       = swx  +     wx
   rlui      = rlui + x * wx

   IF (swx >= .9999)EXIT
  ENDIF

 END DO NODE

 rlui = rlui/swx

 RETURN
 
 END FUNCTION rlui
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 INTEGER FUNCTION ilui(nva,nv,iva)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   ilui
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
 IMPLICIT NONE

 INTEGER nv,i,k
 INTEGER nva(nv),iva(nv)
 INTEGER mva(nv)

 mva(1)=nva(1)
 DO i=2,nv
  mva(i)=mva(i-1)*nva(i)
 END DO

 k= iva(1)
 DO i=2,nv
  k= k+ (iva(i)-1)*mva(i-1)
 END DO

 ilui=k

 RETURN

 END FUNCTION ilui
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
END MODULE FLSAsnow_SURFACE_ALBEDO
