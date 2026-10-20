 MODULE SAHPERT
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   SAHPERT
!
! !Description:
!
!    Purpose - Applies percentage correction to surface albedo
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
!    apply_sahpert - Correct surface albedo using snow/ice percentage
!
!  Modules Used:
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

 TYPE SAHPERT_TYPE
  SEQUENCE
  REAL*4 salbXSAH     !!! SAH Monthly Mean albedo 
  REAL*4 cry_fracXSAH !!! SAH Monthly Mean SnowIce Fraction
  REAL*4 salb000      !!! Assumed surface albedo for 0 fraction SNOWorICE
  REAL*4 salb100      !!! Assumed surface albedo for 1.0 fraction  SNOWorICE
  REAL*4 cry_frac     !!! SNOWorICE fraction
  !!! OUTPUT
  REAL*4 salbSAHCRY   ! Surface Albedo@ SNOWorICE fraction  :: SAHPERT
  REAL*4 mu0
 END TYPE SAHPERT_TYPE
  
 CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
 SUBROUTINE apply_sahpert(sap)
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   apply_sahpert
!
! !Description:
!    Purpose - Correct surface albedo using snow/ice percentage
!
! 
! !Input Parameters:
!    NONE
!
! !Output Parameters:
!    sap
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
 TYPE (SAHPERT_TYPE) sap
 REAL factor
 REAL s1,s0 , sm 
 REAL fx,fm
 
 s0 = sap%salb000   !!! Lower bound Surface Albedo ( typically ocean)
 s1 = sap%salb100   !!! Upper bound Surface Albedo ( typically Snow or Ice)
 fx = sap%cry_frac  !!! Daily snowice fraction
 fm = sap%cry_fracXSAH !!! Monthly MEAN snowice fraction from SAH map
 sm = sap%salbXSAH     !!! Monthly MEAN SurfaceAlbedo from SAH map
 
 
  IF ( fm < 0 .OR. fm > 1.01 .OR. &
   fx < 0 .OR. fx > 1.01 ) THEN 
   sap%salbSAHCRY = sm
   RETURN  !!! MAKE NO CORRECTION
  ENDIF

  IF ( sm >= s1  .OR.  fm > 0.95) s1 = sm
  IF ( sm <= s0  .OR.  fm < 0.05) s0 = sm

  IF ( fm < 0.001 ) THEN
    s0 = sm
    fm = 0.001
  ENDIF
  IF ( fm > 0.999 ) THEN
    s1 = sm
    fm = 0.999
  ENDIF       
  factor=1.0
    
  IF ( fx <= fm ) THEN
    factor = (fx/fm)  
    sap%salbSAHCRY = s0 + factor * (sm -s0)    
  ELSEIF ( fx > fm ) THEN 
    factor = (fx-fm)/(1.0-fm)
    sap%salbSAHCRY = sm + factor * (s1-sm)
  ENDIF
  
 END SUBROUTINE apply_sahpert
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
END MODULE SAHPERT

!===============================================================================================

subroutine test_sahpert
USE SAHPERT ,only : SAHPERT_TYPE ,apply_sahpert

type (SAHPERT_TYPE) sap

 sap%salbXSAH     = 0.66  !!! SAH Monthly Mean albedo 
 sap%cry_fracXSAH =0.75 !!! SAH Monthly Mean SnowIce Fraction
 sap%salb000	  =0.06   !!! Assumed surface albedo for 0 fraction SNOWorICE
 sap%salb100      =0.67 !!! Assumed surface albedo for 1.0 fraction  SNOWorICE
 sap%cry_frac     =0.50!!! SNOWorICE fraction
 
 do k= 0,100
 sap%cry_frac = k*0.01
 call apply_sahpert(sap)
 
 print'(6f8.3)',sap
 
 enddo
 
 end subroutine test_sahpert
