MODULE STREAM_CORRECTION_OCEAN
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   STREAM_CORRECTION_OCEAN
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
!    tf_table           - 
!    ilui               - 
!    oceanstreamcorrect -
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
 IMPLICIT NONE
 PRIVATE
 PUBLIC:: oceanstreamcorrect

 INTEGER ,PARAMETER :: nv = 3  ,mxn =10 

! nv = # of Variables in look up table.
! mxn = Max # of nodes for any single variable
! nva = # of nodes for each variables
! anodes = The values at each node in the table; for all variables
 
 INTEGER nva(nv)
 REAL, SAVE :: anodes(mxn,nv)
 LOGICAL, SAVE :: ichoose(nv,2**nv), streamoceaninit = .false.

 DATA nva  /6,10,4/
! 1-6 WIND
 DATA anodes(1:6,1) /0.0,3.0,6.0,9.0,12.0,16.0/
! 2-10 COSSZA 
 DATA anodes(1:10,2) /0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/
! 3-4 PW
 DATA anodes(1:4,3) / 0.29,1.47,2.94,4.41/

 REAL ,DIMENSION(6*10*4) ::dlnswtoa_sea ,dlnswsfc_sea

! dlnswtoa :ALOG(KATO2S)-ALOG(COART) TOA   -0.0348010    0.0563135

 DATA dlnswtoa_sea / &
  0.51,-0.25,-2.07,-2.82,-3.17,-3.46, 0.93, 2.56, 2.07, 1.65, 1.29, 0.97,&
  0.28, 2.14, 2.46, 2.28, 2.12, 1.94,-0.97, 0.37, 1.22, 1.56, 1.74, 1.69,&
 -1.24,-0.69, 0.07, 0.62, 0.96, 1.05,-2.16,-1.89,-1.65,-0.86,-0.63,-0.21,&
 -2.20,-2.07,-2.11,-1.79,-1.36,-1.04,-2.21,-2.26,-2.24,-2.18,-1.82,-1.71,&
 -2.63,-2.48,-2.68,-2.73,-2.39,-2.44,-3.29,-3.11,-3.43,-3.48,-3.10,-3.03,&
  2.75, 1.46,-0.39,-1.18,-1.56,-1.87, 2.29, 3.95, 3.39, 2.91, 2.48, 2.14,&
  1.19, 3.21, 3.55, 3.36, 3.17, 2.99,-0.26, 1.19, 2.13, 2.48, 2.66, 2.62,&
 -0.68,-0.09, 0.77, 1.33, 1.70, 1.81,-1.68,-1.38,-1.07,-0.27,-0.01, 0.42,&
 -1.80,-1.65,-1.67,-1.32,-0.87,-0.53,-1.88,-1.94,-1.90,-1.80,-1.44,-1.32,&
 -2.35,-2.22,-2.40,-2.43,-2.10,-2.11,-3.05,-2.86,-3.18,-3.23,-2.87,-2.78,&
  4.36, 2.80, 0.93, 0.12,-0.30,-0.61, 3.51, 5.11, 4.48, 3.96, 3.50, 3.13,&
  2.01, 4.11, 4.46, 4.26, 4.05, 3.83, 0.37, 1.88, 2.87, 3.23, 3.40, 3.36,&
 -0.18, 0.44, 1.33, 1.93, 2.30, 2.42,-1.23,-0.92,-0.60, 0.21, 0.50, 0.93,&
 -1.40,-1.26,-1.27,-0.91,-0.45,-0.12,-1.53,-1.62,-1.55,-1.46,-1.10,-0.96,&
 -2.05,-1.93,-2.10,-2.13,-1.80,-1.83,-2.78,-2.60,-2.92,-2.96,-2.62,-2.53,&
  5.07, 3.44, 1.59, 0.77, 0.31, 0.00, 4.05, 5.63, 4.96, 4.43, 3.96, 3.58,&
  2.42, 4.55, 4.90, 4.68, 4.47, 4.25, 0.67, 2.23, 3.22, 3.59, 3.76, 3.72,&
  0.04, 0.69, 1.62, 2.22, 2.59, 2.71,-1.04,-0.73,-0.36, 0.44, 0.75, 1.17,&
 -1.24,-1.10,-1.11,-0.73,-0.27, 0.07,-1.40,-1.50,-1.42,-1.32,-0.97,-0.82,&
 -1.95,-1.85,-2.02,-2.04,-1.73,-1.73,-2.70,-2.53,-2.83,-2.88,-2.57,-2.46/

!dlnswtoa :alog(KATO2S)-alog(COART) SFC   -0.0780773  -0.00235319

 DATA dlnswsfc_sea / &
 -7.81,-6.56,-5.97,-5.66,-5.46,-5.30,-3.90,-4.19,-3.90,-3.69,-3.52,-3.44,&
 -2.33,-2.68,-2.68,-2.65,-2.58,-2.53,-1.58,-1.75,-1.85,-1.89,-1.88,-1.89,&
 -1.17,-1.22,-1.29,-1.35,-1.37,-1.39,-0.89,-0.89,-0.92,-0.96,-0.99,-1.02,&
 -0.68,-0.67,-0.68,-0.70,-0.71,-0.74,-0.52,-0.50,-0.50,-0.51,-0.52,-0.53,&
 -0.33,-0.33,-0.33,-0.34,-0.34,-0.34,-0.24,-0.24,-0.24,-0.24,-0.25,-0.25,&
 -6.92,-5.60,-5.03,-4.73,-4.51,-4.36,-3.59,-3.80,-3.53,-3.29,-3.16,-3.08,&
 -2.09,-2.40,-2.44,-2.37,-2.32,-2.24,-1.37,-1.55,-1.66,-1.67,-1.69,-1.67,&
 -1.02,-1.08,-1.15,-1.21,-1.24,-1.24,-0.83,-0.83,-0.86,-0.91,-0.94,-0.96,&
 -0.71,-0.69,-0.71,-0.73,-0.74,-0.76,-0.62,-0.62,-0.62,-0.62,-0.63,-0.64,&
 -0.57,-0.56,-0.55,-0.55,-0.55,-0.56,-0.48,-0.48,-0.49,-0.49,-0.49,-0.49,&
 -6.10,-4.75,-4.20,-3.89,-3.69,-3.53,-3.07,-3.28,-2.99,-2.74,-2.60,-2.50,&
 -1.72,-2.05,-2.05,-1.97,-1.93,-1.87,-1.10,-1.26,-1.35,-1.39,-1.40,-1.39,&
 -0.80,-0.84,-0.92,-0.98,-1.01,-1.01,-0.63,-0.63,-0.66,-0.71,-0.75,-0.76,&
 -0.53,-0.52,-0.53,-0.54,-0.57,-0.58,-0.46,-0.44,-0.44,-0.45,-0.46,-0.46,&
 -0.40,-0.39,-0.39,-0.39,-0.39,-0.39,-0.40,-0.31,-0.32,-0.32,-0.32,-0.32,&
 -5.89,-4.54,-4.00,-3.68,-3.48,-3.33,-2.97,-3.19,-2.90,-2.69,-2.54,-2.38,&
 -1.64,-1.98,-1.98,-1.93,-1.84,-1.79,-1.05,-1.22,-1.31,-1.35,-1.34,-1.32,&
 -0.75,-0.81,-0.90,-0.94,-0.97,-0.99,-0.61,-0.61,-0.64,-0.69,-0.71,-0.74,&
 -0.51,-0.50,-0.52,-0.53,-0.56,-0.57,-0.45,-0.43,-0.43,-0.44,-0.44,-0.45,&
 -0.40,-0.38,-0.40,-0.40,-0.40,-0.41,-0.38,-0.39,-0.39,-0.39,-0.39,-0.39/

 CONTAINS
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
 
 IF(  streamoceaninit ) RETURN
 
 DO i=1,n2
  DO ii=1,n1
   ib(ii,i)=btest(i-1,ii-1)
  ENDDO
 ENDDO
 
 streamoceaninit = .TRUE.
 
 RETURN
 
 END SUBROUTINE tf_table
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 INTEGER FUNCTION ilui(iva)
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
 
 INTEGER i,k
 INTEGER iva(nv)
 INTEGER mva(nv)

 mva(1)=nva(1)
 DO i=2,nv
  mva(i)=mva(i-1)*nva(i)
 ENDDO

 k= iva(1)
 DO i=2,nv
  k= k+ (iva(i)-1)*mva(i-1)
 ENDDO

 ilui=k
 
 RETURN
 
 END FUNCTION ilui
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 SUBROUTINE oceanstreamcorrect(wind,u0,pw,ctoa,csfc)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   oceanstreamcorrect
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
 REAL,INTENT(IN) ::  wind,u0,pw
 REAL,INTENT(OUT):: ctoa,csfc

 REAL value_table1,value_table2, swx,wx,rmin,rmax,trlui1,trlui2
 INTEGER i,j,ik,k,iv,in
 INTEGER iva(nv,2**nv)

 REAL wb(nv),wt(nv),rva(nv),val(nv)
 INTEGER ibase(nv),itop(nv)

 val(1) =  wind ! Wind Speed (msec-1)
 val(2) =  u0   ! Cos Sol
 val(3) =  pw   ! PrecipWater (cm)

 CALL tf_table(nv,2**nv,ichoose)

 
 VSET : DO iv=1,nv 
   rmin = anodes(1,iv)
   rmax = anodes(nva(iv),iv)

   IF (val(iv) <= rmin )THEN 
     rva(iv) = 1
   ELSEIF (val(iv) >= rmax ) THEN
     rva(iv) = nva(iv)
   ELSE

     FNODES : DO in = 1,nva(iv)-1
      IF( val(iv) >= anodes(in,iv) .AND. val(iv) < anodes(in+1,iv)) & 
          rva(iv)= in+( val(iv)-anodes(in,iv)) /                    &
                    (anodes(in+1,iv)-anodes(in,iv))
     ENDDO FNODES

   ENDIF
 ENDDO VSET


 ibase = floor(rva)
 WHERE( ibase < 1) ibase=1

 itop= ceiling(rva)
 WHERE( itop > nva) itop=nva

 wb=1.0-(rva-ibase)
 wt=1.0-wb

 swx=0 ; trlui1 =0; trlui2 =0 ; ik=0

 NODE : DO j=1,2**nv
  wx=1.0

  DIMENSION: do i=1,nv

   IF( ichoose(i,j) ) THEN
    iva(i,j) = itop(i)
    wx=wx*wt(i)
   ELSE
    iva(i,j) = ibase(i)
    wx=wx*wb(i)
   ENDIF

  ENDDO DIMENSION

  IF( wx > 0 ) THEN
   ik=ik+1
   k = ilui(iva(1,j))

!  value_table = table(k)
   value_table1 = dlnswtoa_sea(k)
   value_table2 = dlnswsfc_sea(k)
   swx    = swx  +  wx
   trlui1 = trlui1 + value_table1 * wx
   trlui2 = trlui2 + value_table2 * wx

!   IF (swx >= 0.9999 )EXIT
  ENDIF

 ENDDO NODE
 
 ctoa = trlui1/swx       
 csfc = trlui2/swx    

 RETURN
 
 END SUBROUTINE oceanstreamcorrect
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
END MODULE STREAM_CORRECTION_OCEAN

!#########################################################################################
!#########################################################################################
!#########################################################################################

MODULE STREAM_CORRECTION_LAND
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   STREAM_CORRECTION_LAND
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
!    tf_table           - 
!    ilui               - 
!    landstreamcorrect -
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
 IMPLICIT NONE

 PRIVATE
 PUBLIC landstreamcorrect

 INTEGER ,PARAMETER :: nv = 4  ,mxn =10 

! nv = # of Variables in look up table.
! mxn = Max # of nodes for any single variable
! nva = # of nodes for each variables
! anodes = The values at each node in the table; for all variables
 
 INTEGER nva(nv)
 REAL,SAVE :: anodes(mxn,nv)
 LOGICAL,SAVE :: ichoose(nv,2**nv), streamlandinit = .FALSE.

 DATA nva  /4,10,4,3/

! 1-4 PW
 DATA anodes(1:4,1)/ 0.29,1.47,2.94,4.41/

! 2- 10 COSSZA 
 DATA anodes(1:10,2) /0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/

! 3-4 SURFACE_ALBEDO
 DATA anodes(1:4,3) /0.1,0.3,0.6,0.8/

! 4 Land Type (SUBSET OF IGBP)
 DATA anodes(1:3,4) /1.0,2.0,3.0/

 REAL ,DIMENSION(4*10*4*3) ::dlnswtoa_lnd ,dlnswsfc_lnd

! dlnswtoa :ALOG(KATO2S)-ALOG(FU4S)TOA   -0.0108867    0.0116496

 DATA dlnswtoa_lnd / &
 -1.05,-1.01,-1.01,-1.01,-0.15,-0.13,-0.15,-0.15, 0.25, 0.26, 0.26, 0.26, 0.46, 0.46, 0.46, 0.46, 0.55, 0.55, 0.56, 0.56,&
  0.59, 0.59, 0.59, 0.59, 0.60, 0.60, 0.60, 0.60, 0.58, 0.59, 0.57, 0.57, 0.55, 0.54, 0.55, 0.55, 0.50, 0.50, 0.51, 0.51,&
 -0.46,-0.48,-0.45,-0.45, 0.19, 0.19, 0.19, 0.19, 0.43, 0.42, 0.43, 0.43, 0.51, 0.50, 0.50, 0.50, 0.51, 0.52, 0.51, 0.51,&
  0.51, 0.50, 0.50, 0.50, 0.48, 0.47, 0.47, 0.47, 0.44, 0.44, 0.43, 0.43, 0.40, 0.40, 0.39, 0.39, 0.36, 0.35, 0.36, 0.36,&
 -0.18,-0.18,-0.16,-0.16, 0.20, 0.19, 0.18, 0.18, 0.28, 0.27, 0.26, 0.26, 0.26, 0.25, 0.25, 0.25, 0.22, 0.23, 0.23, 0.23,&
  0.18, 0.18, 0.18, 0.18, 0.14, 0.14, 0.14, 0.14, 0.10, 0.10, 0.10, 0.10, 0.05, 0.06, 0.06, 0.06, 0.01, 0.02, 0.02, 0.02,&
 -0.08,-0.08,-0.08,-0.08, 0.22, 0.23, 0.23, 0.23, 0.30, 0.29, 0.30, 0.30, 0.29, 0.29, 0.29, 0.29, 0.26, 0.26, 0.26, 0.26,&
  0.22, 0.22, 0.22, 0.22, 0.18, 0.18, 0.18, 0.18, 0.14, 0.14, 0.14, 0.14, 0.10, 0.10, 0.10, 0.10, 0.06, 0.06, 0.06, 0.06,&
 -0.97,-0.94,-0.94,-0.94,-0.04,-0.04,-0.06,-0.06, 0.36, 0.36, 0.37, 0.37, 0.56, 0.56, 0.57, 0.57, 0.68, 0.68, 0.68, 0.68,&
  0.73, 0.74, 0.74, 0.74, 0.75, 0.75, 0.75, 0.75, 0.74, 0.75, 0.74, 0.74, 0.72, 0.73, 0.72, 0.72, 0.68, 0.69, 0.69, 0.69,&
 -0.30,-0.30,-0.30,-0.30, 0.35, 0.35, 0.35, 0.35, 0.59, 0.60, 0.61, 0.61, 0.69, 0.69, 0.70, 0.70, 0.73, 0.73, 0.72, 0.72,&
  0.73, 0.73, 0.73, 0.73, 0.72, 0.71, 0.71, 0.71, 0.70, 0.69, 0.69, 0.69, 0.67, 0.66, 0.66, 0.66, 0.64, 0.64, 0.63, 0.63,&
  0.05, 0.03, 0.05, 0.05, 0.38, 0.39, 0.39, 0.39, 0.48, 0.48, 0.47, 0.47, 0.49, 0.48, 0.48, 0.48, 0.46, 0.45, 0.45, 0.45,&
  0.43, 0.42, 0.42, 0.42, 0.39, 0.39, 0.38, 0.38, 0.36, 0.35, 0.34, 0.34, 0.32, 0.31, 0.30, 0.30, 0.28, 0.27, 0.27, 0.27,&
  0.09, 0.09, 0.09, 0.09, 0.29, 0.29, 0.28, 0.28, 0.31, 0.31, 0.31, 0.31, 0.28, 0.27, 0.27, 0.27, 0.23, 0.22, 0.21, 0.21,&
  0.18, 0.17, 0.17, 0.17, 0.14, 0.12, 0.12, 0.12, 0.09, 0.08, 0.07, 0.07, 0.05, 0.04, 0.03, 0.03, 0.01, 0.00,-0.01,-0.01,&
 -0.71,-0.72,-0.75,-0.75, 0.16, 0.16, 0.14, 0.14, 0.59, 0.60, 0.58, 0.58, 0.81, 0.80, 0.80, 0.80, 0.92, 0.93, 0.92, 0.92,&
  0.99, 0.99, 0.99, 0.99, 1.03, 1.02, 1.02, 1.02, 1.04, 1.03, 1.03, 1.03, 1.03, 1.02, 1.02, 1.02, 1.00, 1.00, 1.00, 1.00,&
  0.14, 0.12, 0.12, 0.12, 0.66, 0.66, 0.64, 0.64, 0.86, 0.86, 0.86, 0.86, 0.94, 0.94, 0.94, 0.94, 0.98, 0.98, 0.99, 0.99,&
  0.98, 0.99, 0.99, 0.99, 0.98, 0.99, 0.99, 0.99, 0.97, 0.98, 0.98, 0.98, 0.95, 0.96, 0.96, 0.96, 0.93, 0.94, 0.94, 0.94,&
  0.47, 0.49, 0.48, 0.48, 0.55, 0.57, 0.57, 0.57, 0.55, 0.57, 0.58, 0.58, 0.52, 0.55, 0.56, 0.56, 0.50, 0.53, 0.54, 0.54,&
  0.47, 0.50, 0.51, 0.51, 0.43, 0.47, 0.48, 0.48, 0.41, 0.44, 0.45, 0.45, 0.38, 0.41, 0.42, 0.42, 0.35, 0.38, 0.40, 0.40,&
  0.43, 0.44, 0.44, 0.44, 0.28, 0.31, 0.32, 0.32, 0.18, 0.22, 0.24, 0.24, 0.10, 0.15, 0.17, 0.17, 0.06, 0.10, 0.12, 0.12,&
  0.00, 0.05, 0.07, 0.07,-0.04, 0.01, 0.03, 0.03,-0.08,-0.03,-0.01,-0.01,-0.11,-0.06,-0.04,-0.04,-0.14,-0.09,-0.07,-0.07/

! dlnswtoa :ALOG(KATO2S)-ALOG(FU4S) SFC   -0.0188503    0.0109692

 DATA dlnswsfc_lnd / &
  1.08, 1.10, 1.10, 1.10, 0.38, 0.37, 0.37, 0.37, 0.15, 0.15, 0.15, 0.15, 0.05, 0.05, 0.04, 0.04,-0.01,-0.02,-0.02,-0.02,&
 -0.05,-0.06,-0.06,-0.06,-0.08,-0.08,-0.08,-0.08,-0.10,-0.10,-0.10,-0.10,-0.11,-0.12,-0.12,-0.12,-0.13,-0.13,-0.13,-0.13,&
  0.91, 0.91, 0.91, 0.91, 0.18, 0.17, 0.17, 0.17,-0.07,-0.08,-0.08,-0.08,-0.19,-0.20,-0.21,-0.21,-0.28,-0.28,-0.29,-0.29,&
 -0.33,-0.34,-0.34,-0.34,-0.36,-0.37,-0.38,-0.38,-0.40,-0.40,-0.41,-0.41,-0.42,-0.43,-0.43,-0.43,-0.44,-0.45,-0.45,-0.45,&
  0.65, 0.63, 0.63, 0.63,-0.12,-0.14,-0.15,-0.15,-0.40,-0.41,-0.43,-0.43,-0.56,-0.57,-0.58,-0.58,-0.66,-0.67,-0.68,-0.68,&
 -0.73,-0.73,-0.74,-0.74,-0.77,-0.78,-0.78,-0.78,-0.81,-0.82,-0.82,-0.82,-0.85,-0.85,-0.86,-0.86,-0.87,-0.88,-0.88,-0.88,&
  0.54, 0.54, 0.52, 0.52,-0.24,-0.25,-0.26,-0.26,-0.52,-0.53,-0.53,-0.53,-0.67,-0.68,-0.69,-0.69,-0.78,-0.79,-0.79,-0.79,&
 -0.84,-0.85,-0.86,-0.86,-0.89,-0.90,-0.91,-0.91,-0.93,-0.94,-0.95,-0.95,-0.97,-0.98,-0.98,-0.98,-1.00,-1.01,-1.01,-1.01,&
  1.06, 1.06, 1.06, 1.06, 0.35, 0.35, 0.35, 0.35, 0.13, 0.13, 0.13, 0.13, 0.03, 0.02, 0.02, 0.02,-0.03,-0.04,-0.04,-0.04,&
 -0.07,-0.07,-0.07,-0.07,-0.10,-0.10,-0.10,-0.10,-0.12,-0.12,-0.12,-0.12,-0.13,-0.13,-0.13,-0.13,-0.14,-0.15,-0.15,-0.15,&
  0.85, 0.83, 0.85, 0.85, 0.10, 0.09, 0.09, 0.09,-0.14,-0.14,-0.15,-0.15,-0.27,-0.27,-0.27,-0.27,-0.34,-0.35,-0.35,-0.35,&
 -0.39,-0.39,-0.40,-0.40,-0.42,-0.43,-0.43,-0.43,-0.45,-0.45,-0.46,-0.46,-0.47,-0.48,-0.48,-0.48,-0.49,-0.49,-0.50,-0.50,&
  0.51, 0.51, 0.49, 0.49,-0.28,-0.29,-0.29,-0.29,-0.56,-0.57,-0.57,-0.57,-0.71,-0.71,-0.72,-0.72,-0.80,-0.81,-0.82,-0.82,&
 -0.87,-0.88,-0.88,-0.88,-0.91,-0.92,-0.93,-0.93,-0.95,-0.96,-0.97,-0.97,-0.98,-1.00,-1.00,-1.00,-1.01,-1.02,-1.03,-1.03,&
  0.27, 0.27, 0.25, 0.25,-0.55,-0.56,-0.56,-0.56,-0.84,-0.85,-0.86,-0.86,-1.01,-1.02,-1.03,-1.03,-1.12,-1.13,-1.14,-1.14,&
 -1.19,-1.20,-1.21,-1.21,-1.24,-1.26,-1.27,-1.27,-1.29,-1.31,-1.32,-1.32,-1.33,-1.35,-1.35,-1.35,-1.36,-1.38,-1.39,-1.39,&
  1.03, 1.03, 1.03, 1.03, 0.30, 0.31, 0.31, 0.31, 0.08, 0.08, 0.09, 0.09,-0.02,-0.02,-0.02,-0.02,-0.08,-0.07,-0.07,-0.07,&
 -0.11,-0.11,-0.11,-0.11,-0.14,-0.13,-0.13,-0.13,-0.15,-0.15,-0.15,-0.15,-0.17,-0.16,-0.16,-0.16,-0.18,-0.17,-0.17,-0.17,&
  0.70, 0.72, 0.72, 0.72,-0.07,-0.05,-0.04,-0.04,-0.31,-0.29,-0.29,-0.29,-0.43,-0.41,-0.40,-0.40,-0.49,-0.47,-0.47,-0.47,&
 -0.53,-0.52,-0.51,-0.51,-0.56,-0.55,-0.54,-0.54,-0.59,-0.57,-0.56,-0.56,-0.60,-0.59,-0.58,-0.58,-0.62,-0.60,-0.59,-0.59,&
  0.17, 0.20, 0.20, 0.20,-0.67,-0.64,-0.63,-0.63,-0.96,-0.92,-0.91,-0.91,-1.10,-1.06,-1.05,-1.05,-1.17,-1.14,-1.12,-1.12,&
 -1.23,-1.19,-1.17,-1.17,-1.27,-1.23,-1.21,-1.21,-1.30,-1.26,-1.25,-1.25,-1.32,-1.28,-1.27,-1.27,-1.34,-1.30,-1.29,-1.29,&
 -0.23,-0.19,-0.16,-0.16,-1.13,-1.08,-1.06,-1.06,-1.44,-1.39,-1.37,-1.37,-1.60,-1.55,-1.53,-1.53,-1.69,-1.63,-1.61,-1.61,&
 -1.75,-1.70,-1.67,-1.67,-1.80,-1.74,-1.72,-1.72,-1.83,-1.78,-1.76,-1.76,-1.86,-1.81,-1.78,-1.78,-1.89,-1.83,-1.81,-1.81/

 CONTAINS
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

 IF(  streamlandinit ) RETURN

 DO i=1,n2
  DO ii=1,n1
   ib(ii,i)=btest(i-1,ii-1)
  ENDDO
 ENDDO

 streamlandinit = .TRUE.

 RETURN

 END SUBROUTINE tf_table
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 INTEGER FUNCTION ilui(iva)
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

 INTEGER i,k
 INTEGER iva(nv)
 INTEGER mva(nv)

 mva(1)=nva(1)
 DO i=2,nv
  mva(i)=mva(i-1)*nva(i)
 ENDDO

 k= iva(1)
 DO i=2,nv
  k= k+ (iva(i)-1)*mva(i-1)
 ENDDO

 ilui=k
 
 RETURN
 
 END FUNCTION ilui
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 SUBROUTINE landstreamcorrect(pw,u0,salb,rlnd,ctoa,csfc)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   landstreamcorrect
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

 REAL,INTENT(IN)::  pw,u0,salb,rlnd
 REAL,INTENT(OUT):: ctoa,csfc

 REAL value_table1,value_table2, swx,wx,rmin,rmax,trlui1,trlui2
 INTEGER i,j,ik,k,iv,in
 INTEGER  iva(nv,2**nv)

 REAL wb(nv),wt(nv),rva(nv),val(nv)
 INTEGER ibase(nv),itop(nv)

 val(1) =  pw    ! PrecipWater (cm)
 val(2) =  u0    ! Cos Sol (0_1)
 val(3) =  salb  ! Surface Albedo (0-1)
 val(4) =  rlnd  ! Land Type ( 1=Crop(12)  ,2 =Desert(16) ,3=snow(19) )

 CALL tf_table(nv,2**nv,ichoose)

 VSET : DO iv=1,nv 
  rmin = anodes(1,iv)
  rmax = anodes(nva(iv),iv)

  IF (val(iv) <= rmin )THEN 
   rva(iv) = 1
  ELSEIF (val(iv) >= rmax ) THEN
   rva(iv) = nva(iv)
  ELSE
   FNODES : DO in = 1,nva(iv)-1
     IF( val(iv) >= anodes(in,iv) .AND. val(iv) < anodes(in+1,iv)) & 
         rva(iv)= in+( val(iv)-anodes(in,iv)) /                    &
                    (anodes(in+1,iv)-anodes(in,iv))
   ENDDO FNODES
  ENDIF
 ENDDO VSET

 ibase = floor(rva)
 WHERE( ibase < 1) ibase=1

 itop= ceiling(rva)
 WHERE( itop > nva) itop=nva

 wb=1.0-(rva-ibase)
 wt=1.0-wb

 swx=0 ; trlui1 =0; trlui2 =0 ; ik=0

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
  ENDDO DIMENSION

  IF ( wx > 0 ) THEN
    ik=ik+1
    k = ilui(iva(1,j))

!    value_table = table(k)
    value_table1  = dlnswtoa_lnd(k)
    value_table2  = dlnswsfc_lnd(k)
    swx = swx  +   wx
    trlui1      = trlui1 + value_table1 * wx
    trlui2      = trlui2 + value_table2 * wx

!    IF (swx >= 0.9999 )EXIT
  ENDIF

 ENDDO NODE

 ctoa = trlui1/swx       
 csfc = trlui2/swx    

 RETURN

 END SUBROUTINE landstreamcorrect

END MODULE STREAM_CORRECTION_LAND

!===============================================================
!===============================================================

MODULE STREAM_CORRECTION
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   STREAM_CORRECTION
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
!    tf_table           - 
!    ilui               - 
!    landstreamcorrect -
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
 USE STREAM_CORRECTION_OCEAN, ONLY: oceanstreamcorrect
 USE STREAM_CORRECTION_LAND, ONLY: landstreamcorrect
 
 IMPLICIT NONE
 
 PRIVATE

 TYPE STRCOR
  SEQUENCE

! INPUTS 
  INTEGER istype   !! (1-24) EXTENDED IGBP
  REAL u0	   !! Cosine Sol (0-1.0)
  REAL wind	   !! WindSpeed (msec-1)
  REAL pw	   !! PW (cm) (0- ~7)
  REAL salb	   !! BB surface Albedo (0-1)
  REAL swtoaup_un  !! Uncorrected TOA PRISTINE SWup
  REAL swsfcdn_un  !! Uncorrected SFC PRISTINE SWdn

! OUTPUTS
  REAL swtoaup_cor !! Corrected TOA PRISTINE SWup
  REAL swsfcdn_cor !! Corrected SFC PRISTINE SWdn
 END TYPE STRCOR

 TYPE (STRCOR) stco

 PUBLIC :: STRCOR ,stco, streamcorrect

 INTEGER ilnd
 REAL ctoa,csfc
 
 CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 SUBROUTINE streamcorrect
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   streamcorrect
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
 SELECT CASE ( stco%istype ) !!! ASSOCIATES EXTENDED IGBP TYPES to LUT INDEX
  CASE ( 1:6, 8:14 , 18 ) ! CROP
    ilnd = 1
  CASE ( 7 ,16, 23:24 )   !DESERTS 
    ilnd=2
  CASE ( 15, 19:22 )      !SNOWs
    ilnd = 3
  CASE ( 17  )            !OCEAN
    ilnd=0
 END SELECT 

 SELECT CASE ( ilnd )
  CASE ( 0 ) 
    CALL oceanstreamcorrect(stco%wind,stco%u0,stco%pw,ctoa,csfc)
  CASE ( 1:3 ) 
    CALL landstreamcorrect(stco%pw,stco%u0,stco%salb,float(ilnd),ctoa,csfc)
 END SELECT 

 stco%swtoaup_cor=0.00  ! Fixed for Beta4 20070501
 stco%swsfcdn_cor=0.00

 IF ( stco%swtoaup_un > 0 ) & 
  stco%swtoaup_cor = EXP (  LOG(stco%swtoaup_un)   - ctoa*0.01  ) 

 IF ( stco%swsfcdn_un > 0 ) & 
  stco%swsfcdn_cor = EXP (  LOG(stco%swsfcdn_un)   - csfc*0.01  ) 

 END SUBROUTINE streamcorrect

END MODULE STREAM_CORRECTION
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 SUBROUTINE Stream_FluxCorrection 
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Stream_FluxCorrection
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
 USE FL_IO_Params, ONLY : u0, SW_TOAUP_UNCOR, SW_SFCDN_UNCOR,  &
                          SW_TOAUP_CORR, SW_SFCDN_CORR   
 USE FUOUTPUT, ONLY : fsfc
 USE SARB_Var, ONLY : MOA_PW, SfcWind 
 USE STREAM_CORRECTION, ONLY : stco, streamcorrect
 USE SYNSPECTRAL, ONLY : sg

 IF ( sg%ptv(17) > 50 ) THEN
  stco%istype = 17  !! Mostly Ocean
 ELSEIF ( sg%ptv(15)+sg%ptv(19)+sg%ptv(20)+sg%ptv(21)+sg%ptv(22) > 50 ) THEN
  stco%istype = 15  !! Mostly Snow
 ELSEIF ( sg%ptv(7)+sg%ptv(16)+sg%ptv(23)+sg%ptv(24)> 50 ) THEN
  stco%istype = 7   !! Mostly Desert
 ELSE
  stco%istype = 1   ! assume Crop Land as default
 ENDIF

 stco%u0         = u0             ! CosSol
 stco%pw         = MOA_PW         ! PW (0-~7)
 stco%salb       = fsfc(3)%swalb  ! BB Surface Albedo ( not needed for istype =17)
 stco%wind       = SfcWind        ! WindSpeed (msec-1) ( Only needed for istype =17)
 stco%swtoaup_un = SW_TOAUP_UNCOR
 stco%swsfcdn_un = SW_SFCDN_UNCOR

 CALL streamcorrect 

 SW_TOAUP_CORR = stco%swtoaup_cor 
 SW_SFCDN_CORR = stco%swsfcdn_cor

 END SUBROUTINE Stream_FluxCorrection
