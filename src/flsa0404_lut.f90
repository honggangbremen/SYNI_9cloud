MODULE FLSA0404_SURFACE_ALBEDO
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   FLSA0404_SURFACE_ALBEDO
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
!    flsa_lut_loader_0404 - 
!    flsa0404_lut         - 
!    tf_table             - 
!    rlui                 -  
!    ilui                 - 
!
!  Modules Used:
!   SYN_IO_Utils      - (SARBlib)  Used by Sbr. flsa_lut_loader_0404
!   SARB_IO_Utils     - (SARBlib)  Used by Sbr. flsa_lut_loader_0404
!   SynSARB_IO_Params - (SARBlib)  Used by Sbr. flsa_lut_loader_0404
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
 USE SYN_IO_Utils ,ONLY:  Fred_OpenFile, Fred_CloseFile
 USE SARB_IO_Utils
 USE SynSARB_IO_Params

 IMPLICIT NONE
 INTEGER nrec,nu0,npw,no3,nta,nspec,nalt,ntyp,ncoef,nv

 PARAMETER(nu0=10,npw=7,no3=3,nta=6,nspec=3,nalt=3,ntyp=4)
 PARAMETER(nrec=nu0*npw*no3*nta*nspec*nalt*ntyp)
 PARAMETER(ncoef=3,nv=7)
 REAL coefs(ncoef,nrec)
 LOGICAL ichoose(nv,2**nv) 

 SAVE  coefs,ichoose

 INTEGER iunit_flsa_lut

 CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 SUBROUTINE flsa_lut_loader_0404
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   flsa_lut_loader_0404
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

 INTEGER ios
 CHARACTER*120 :: Name_Sbr = 'flsa_lut_loader_0404'

 INTEGER :: RECL = ncoef*nrec*4
 INTEGER :: VerID = 1
!
 CALL SARB_GenOpen (LID_FLSA_0404, Flag_File_Read,                  & ! I
                    Flag_File_Direct, Flag_File_Unformat, Name_Sbr, & ! I
                                                iunit_flsa_lut,     & ! O
                                                      VerID, RECL)    ! I (Opt)
!
 READ (iunit_flsa_lut,rec=1,iostat=ios) coefs

 IF ( ios /= 0 ) &
  CALL SARB_RdWrtError (LID_FLSA_0404, ios, Name_Sbr, IRec = 1)

 CALL Fred_CloseFile(iunit_flsa_lut)
 
 CALL tf_table(nv,2**nv,ichoose)

 RETURN

 END SUBROUTINE flsa_lut_loader_0404
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
 REAL FUNCTION flsa0404_lut(toa,u0,pw,o3,ta,spec,psfc,wssa)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   flsa0404_lut
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

 REAL toa,u0,pw,o3,ta,spec,psfc,wssa

 REAL rlogmin,rnode

 INTEGER iv
! INTEGER ifile

 INTEGER nva(nv),iva(nv)
 REAL vmm(2,nv),rng(nv),val(nv),rva(nv)

 PARAMETER(rlogmin=1.0E-10)

 DATA nva/nu0,npw,no3,nta,nspec,nalt,ntyp/
 DATA vmm/ 0.1 , 1.0, &
            -4 ,   2, &
            200, 500, &
            -5,    0, &
            -1,    1, &
           628, 1013, &
           0.02,0.131 / ! Sulfate,Continental , 1um Dust, 2um Dust

 val(1:nv) = vmm(1,1:nv)
 val(1) = u0
 IF(pw >= rlogmin) val(2)= log(pw) 
 val(3)= o3
 IF(ta >= rlogmin) val(4)= log(ta)

 val(5)= spec
 val(6)= ifix(psfc)
 val(7) = wssa

 DO iv=1,nv
  IF (val(iv) < vmm(1,iv) ) val(iv) = vmm(1,iv)
  IF (val(iv) > vmm(2,iv) ) val(iv) = vmm(2,iv)

  rng = vmm(2,iv)-vmm(1,iv)
  rnode   = (val(iv)-vmm(1,iv)) /rng(iv)
  rva(iv) = 1+ rnode*(nva(iv)-1)
  iva(iv) = nint(rva(iv))
 ENDDO

 flsa0404_lut = rlui(toa,nva,nv,rva, coefs,ncoef,nrec)
 
 RETURN
 
 END FUNCTION flsa0404_lut
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
 INTEGER  nva(nv),iva(nv,2**nv)
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

   sfc_toa=  coefs(1,k) + coefs(2,k)*toa + coefs(3,k)*toa*toa
   x = toa + sfc_toa 
   swx  = swx  +     wx
   rlui = rlui + x * wx

   IF (swx >= .9999) EXIT
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
 INTEGER  nva(nv),iva(nv)
 INTEGER  mva(nv)

 mva(1)=nva(1)
 DO i=2,nv
  mva(i)=mva(i-1)*nva(i)
 enddo

 k = iva(1)
 DO i=2,nv
  k = k+(iva(i)-1)*mva(i-1)
 END DO

 ilui=k
 
 RETURN
 
 END FUNCTION ilui
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
END MODULE FLSA0404_SURFACE_ALBEDO
