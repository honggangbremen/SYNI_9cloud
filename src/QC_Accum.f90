MODULE QC_Accum
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    QC_Accum
!
!
! !Description:
!    Routine ID -
!
!    Purpose - To provide a module of routines to accumulate statistics
!              after each FOV has been processed through the SARB algorithms
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
!  Internal Routines:
!    cstats  - Accumulates QC report statistics for groups of output
!              parameters for each record
!    Foot_QC - Accumulates QC report statistics for individual output
!              parameters for each record
!    Get_Gamma - calculates the Glint Angle for CERES and MODIS
!    nan_rem - Redefines out-of-range values, such as 'nans,' to a 
!              specIFied default value
!    Tau_Fov - Calculates the mean FOV optical depth
!  Modules Used:
!    FL_IO_Params     - (SARBlib)  Used by Sbr. Foot_QC
!    Profile_Params   - (SARBlib)  Used by Sbr. Foot_QC
!    SARB_OutVar      - (SARBlib)  Used by Sbr. Foot_QC
!    SARB_QC_Params   - (SARBlib)  Used by Sbr. Foot_QC
!    SARB_Var         - (SARBlib)  Used by Sbr. Foot_QC
!    SARBInput_Params - (SARBlib)  Used by Sbr. Foot_QC
!
! !Revision History:
!  Version 3.4   October 4, 2004 -
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                  1. Added Get_Gamma Sbr.
!                  2. Added Tau_Fov Sbr.
!  Version 3.3   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                  1. ModIFied Sbr. Foot_QC
!                  2. ModIFied Sbr. nan_rem
!  Version 3.2   December 24, 2001 - SCCR #315
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. ModIFied Foot_QC to use CERESlib paramter SOLAR_CONST
!  Version 3.0  March 9, 2001 - SCCR #246  
!               Ed Kizer, SAIC, e.a.kizer@larc.nasa.gov
!               ModIFied Subroutine Foot_QC to use generalized
!                 variables in the SARB Library. This is in effort
!                 to make Foot_QC generic between CER5 and CER7.2.
!  Version 2.3  June 16, 1998 - SCCR #89
!               Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 ModIFied Sbr. Foot_QC
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Shalini Gupta, SAIC, s.gupta@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      USE FL_IO_Params
      USE Profile_Params
      USE SARB_OutVar
      USE SARB_QC_Params
      USE SARB_Var
      USE SARBInput_Params
!
      IMPLICIT NONE
!
!
 CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Foot_QC ( CP, IP, irec,                            & ! I
                                         fqc)                         ! I/O
!
!******************************************************************************
!
! !F90
!
! !Name:
!    Foot_QC
!
! !Description:
!    Routine ID -
!
!    Purpose - To accumulate QC report statistics for individual CRS
!              parameters for each record
!
!
! !Input Parameters:
!    CP          - Array index for the constrained pass
!    IP          - Array index for the initial pass
!    irec        - Input product record number
!
!  Input/Output Parameters:
!    None
!
! !Output Parameters:
!    fqc - QC Report statistics
!
! !Team-Unique Header:
!
!  Internal Routines Called:
!    cstats
!    nan_rem
!
!  Modules Used:
!    FL_IO_Params     - (SARBlib)  Parameters: CF, Default_REAL4_FL, DnDir, 
!                                              LW_Prof, SW_Prof,  
!                                              TotLWrad, TotSky, WNflx_SatEm, 
!                                              WNrad_SatEm, UpDir, u0
!    Profile_Params   - (SARBlib)  Parameters: MaxLev_Cld
!    SARBInput_Params - (SARBlib)  Parameters: ClrIdx, Daytime, OceanIdx, SolCon
!    SARB_OutVar      - (SARBlib)  Parameters: a_aer, a_CldFrac, a_CldTemp, 
!                                              a_OptDepth, a_Precip_H2O, a_pts, 
!                                              a_salb, a_TotLWrad, 
!                                              a_UpTrop_RelHum, a_WNflx_SatEm,
!                                              a_WNrad_SatEm, LW_Delta,
!                                              SW_Delta
!    SARB_QC_Params   - (SARBlib)  Parameters: LW_sfc_dnb, ngt, ng1, ng1x, 
!                                              ng1y, ng2, ng2a, ng2ax ,ng2ay,
!                                              ng2x, ng2y, ng3, ng3a, ng3ax, 
!                                              ng3ay, ng3x, ng3y, ng4x, ng4y, 
!                                              SW_sfc_dna
!                                  Structures: fqc_type
!    SARB_Var         - (SARBlib)  Parameters: AerOptDepth, bbout, DayNite,  
!                                              EffTemp, igbp_sid,
!                                              LW_TOA_flux, LW_TOA_UnfiltRad,
!                                              OptDepth_mn, pts, SW_TOA_flux,  
!                                              uth,
!                                              WN_TOA_FiltRad, WN_TOA_Flux, wv 
!
! !Revision History:
!  Version 3.5   June 4, 2014 - SCCR #1017
!                Thomas Caldwell, SSAI, Thomas.E.Caldwell@nasa.gov
!                 Added variable solar constant provided by TISA group 
!  Version 3.4   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. ModIFied calling sequence to include as input arguments:
!                     a. the default value for four-byte reals 
!                     b. the array index pointing to the values associated 
!                        with the constrained 
!                 2. Incorporated use of parameters from Mod. FL_IO_Params
!                 3. Using parameter SolCon from SARBlib 
!                    Mod. SARBInput_Params instead of parameter
!                    SOLAR_CONST from CERESlib Mod. ceres_constants
!  Version 3.2   December 24, 2001 - SCCR #315
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                1. Replaced hardwired value with CERESlib parameter SOLAR_CONST
!  Version 3.0  March 9, 2001 - SCCR #246  
!                Ed Kizer, SAIC, e.a.kizer@larc.nasa.gov
!                1. Removed the CRS and SSF variable from the Subroutine CALL
!                2. Replaced all references to the CRS and SSF to variables 
!                   declared in the SARB Library.
!  Version 2.3  June 16, 1998 - SCCR #89
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                1. Replaced where statements for determining scene type
!                   with just using the first scene type indicated on the SSF
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER,         INTENT (IN)  :: CP
      INTEGER,         INTENT (IN)  :: IP
      INTEGER,         INTENT (IN)  :: irec
      TYPE (fqc_type), INTENT (OUT) :: fqc
!
      INTEGER         :: i, ib, icase, ie, j ,ic
      INTEGER :: itemp, itemp2
      REAL :: Mod_Glint_Angle, CERES_Glint_Angle, Tau_Mean_Fov

!---------------------------------------------------------------------------

! - SUBSETS
! SCENE
!
      fqc % icv ( 2:ncase) = .FALSE.
      fqc % icv ( 1) = .TRUE.
      fqc % icv ( 3) = .TRUE.
!
!-clr/pc/ovc
      CF ( IP, ClrIdx:MaxLev_Cld) = CF ( IP, ClrIdx: MaxLev_Cld) * 100.0
!
      IF (CF ( IP, ClrIdx) == 100.0) THEN
!     *** Clear sky
!
          fqc % icv ( 2) = .TRUE.
          fqc % icv ( 3) = .FALSE.
!
      ELSE IF (CF ( IP, ClrIdx) >= 5.0 .AND. CF ( IP, ClrIdx) < 100.0) THEN
!     *** Partly cloudy sky
!
           fqc % icv ( 4) = .TRUE.
!
      ELSE IF (CF ( IP, ClrIdx) >= 0.0 .AND.  CF ( IP, ClrIdx) < 5.0) THEN
!     *** Overcast
!
           CALL Tau_Fov (Tau_Mean_Fov)!Output   
!   
       fqc % icv ( 5) = .TRUE.
       IF (Tau_Mean_Fov /= Default_REAL4_FL) THEN
        IF (Tau_Mean_Fov >= 0.0 .AND. Tau_Mean_Fov <= 5.0) &
         fqc % icv ( 28) = .TRUE.  !Overcast Thin Cloud
        IF (Tau_Mean_Fov > 5.0 .AND. Tau_Mean_Fov < 20.0) &
         fqc % icv ( 29) = .TRUE.  !Overcast Moderate Cloud
        IF (Tau_Mean_Fov >= 20.0) fqc % icv ( 30) = .TRUE.  !Overcast Thick Cloud
       ENDIF 
      END IF
!
!
!-land/sea
      IF (igbp_sid == OceanIdx) THEN
!     *** Ocean
!
          fqc % icv ( 8) = .TRUE. 
          IF ( fqc % icv ( 2)) fqc % icv ( 9) = .TRUE.  !CLR OCEAN
          IF ( fqc % icv ( 3)) fqc % icv ( 10) = .TRUE.  !CLD OCEAN
          IF ( fqc % icv ( 4)) fqc % icv ( 11) = .TRUE.  !PTCLD OCEAN          
          IF ( fqc % icv ( 5)) fqc % icv ( 12) = .TRUE.  !OVC OCEAN
!
      ELSE IF (igbp_sid == DesertIdx(1) .OR. igbp_sid == DesertIdx(2)) THEN
!     *** Desert
!
          fqc % icv ( 23) = .TRUE. 
          IF ( fqc % icv ( 2)) fqc % icv ( 24) = .TRUE.  !CLR DESERT
          IF ( fqc % icv ( 3)) fqc % icv ( 25) = .TRUE.  !CLD DESERT
          IF ( fqc % icv ( 4)) fqc % icv ( 26) = .TRUE.  !PTCLD DESERT          
          IF ( fqc % icv ( 5)) fqc % icv ( 27) = .TRUE.  !OVC DESERT
!
      ELSE IF (igbp_sid == SnowIdx(1) .OR. igbp_sid == SnowIdx(2) .OR. &
          igbp_sid == SnowIdx(3)) THEN
!     *** Snow
!
          fqc % icv ( 18) = .TRUE. 
          IF ( fqc % icv ( 2)) fqc % icv ( 19) = .TRUE.  !CLR SNOW
          IF ( fqc % icv ( 3)) fqc % icv ( 20) = .TRUE.  !CLD SNOW
          IF ( fqc % icv ( 4)) fqc % icv ( 21) = .TRUE.  !PTCLD SNOW
          IF ( fqc % icv ( 5)) fqc % icv ( 22) = .TRUE.  !OVC SNOW  
!
      ELSE
!  *** Land
!
        fqc % icv ( 13) = .TRUE. 
        IF ( fqc % icv ( 2)) fqc % icv ( 14) = .TRUE.  !CLR LAND
        IF ( fqc % icv ( 3)) fqc % icv ( 15) = .TRUE.  !CLD LAND
        IF ( fqc % icv ( 4)) fqc % icv ( 16) = .TRUE.  !PTCLD LAND
        IF ( fqc % icv ( 5)) fqc % icv ( 17) = .TRUE.  !OVC LAND
  
      END IF
!
!day/night
      IF (DayNite == Daytime) THEN
!     *** Daytime 
!
           fqc % icv ( 6) = .true.
! 
      ELSE
!     *** Nighttime
!
          fqc % icv ( 7) = .true.
!
      END IF
!
!CERES View Zenith Angle
!
! ONLY for CRS
      IF (QCHead_SSID == 'Instantaneous SARB') THEN
       IF (vzen_toa >= 0 .AND. vzen_toa <= 30) THEN
     
        fqc % icv ( 31) = .TRUE.   ! [0 - 30]
  
       ELSE IF (vzen_toa >= 31 .AND. vzen_toa <= 60) THEN
     
        fqc % icv ( 32) = .TRUE.   ![31 - 60]
  
       ELSE
    
        fqc % icv ( 33) = .TRUE.   ![ > 60]

       ENDIF
!
!Modis View Zenith Angle
!
      ! ONLY for CRS
       IF (imag_vzen >= 0 .AND. imag_vzen <= 30) THEN
     
        fqc % icv ( 34) = .TRUE.   ! [0 - 30]
  
       ELSE IF (imag_vzen >= 31 .AND. imag_vzen <= 60) THEN
     
        fqc % icv ( 35) = .TRUE.   ![31 - 60]
  
       ELSE
    
        fqc % icv ( 36) = .TRUE.   ![ > 60]

       ENDIF
!
!Glint Angles
!
      ! ONLY for CRS
       CALL Get_Gamma (Mod_Glint_Angle, CERES_Glint_Angle)  !O
!
       IF (Mod_Glint_Angle <= 30) THEN 

        fqc % icv ( 37) = .TRUE.  !Modis Glint Angle <= 30
  
       ELSE 
    
        fqc % icv ( 38) = .TRUE.   !Modis Glint Angle > 30
 
       ENDIF
!
!
      ! ONLY for CRS
       IF (CERES_Glint_Angle <= 30) THEN 

        fqc % icv ( 39) = .TRUE.  !CERES Glint Angle <= 30
  
       ELSE 
    
        fqc % icv ( 40) = .TRUE.  !CERES Glint Angle > 30

       ENDIF

      ENDIF  ! ONLY for CRS                     
! ---------------------------------------------------------------
!
! Group 1 - TOA FLUXES
!
      fqc % omit= Default_REAL4_FL
      fqc % g1  = fqc % omit
      fqc % toains = Solar_Constant(DataYear, DataMonth, DataDay) * u0

      IF( fqc % toains > 0 .AND. SW_TOA_flux /= fqc % omit )              &
          fqc % g1 ( 1, 1) = SW_TOA_flux / fqc % toains
      fqc % g1 ( 1, 2) = SW_TOA_flux
      fqc % g1 ( 1, 3) = LW_TOA_flux
      fqc % g1 ( 1, 4) = WN_TOA_Flux
      fqc % g1 ( 1, 5) = LW_TOA_UnfiltRad 
      fqc % g1 ( 1, 6) = WN_TOA_FiltRad
!
      IF ( fqc % toains > 0                                                & 
           .AND. SWProf ( CP, TotSky, UpDir, TOALev_Idx) /= fqc % omit) THEN
           fqc % g1 ( 3, 1) =                                            &
                      SWProf ( CP, TotSky, UpDir, TOALev_Idx) / fqc % toains
      END IF
!
       fqc % g1 ( 3, 2) = SWProf ( CP, TotSky, UpDir, TOALev_Idx)
       fqc % g1 ( 3, 3) = LWProf ( CP, TotSky, UpDir, TOALev_Idx)
       fqc % g1 ( 3, 4) = WNflx_SatEm ( CP, TotSky)
       fqc % g1 ( 3, 5) = TotLWrad ( CP, TotSky)
       fqc % g1 ( 3, 6) = WNrad_SatEm ( CP, TotSky)
!
       IF (fqc % toains > 0                                                &
            .AND. SW_Delta ( TotSky, UpDir, TOALev_Idx) /= fqc % omit) THEN
!       *** Daytime
!
            fqc % g1 ( 2, 1) = (  fqc % g1 ( 3, 2)                          &
                                - SW_Delta ( TotSky, UpDir, TOALev_Idx))    &
                              / fqc % toains
!
       END IF
!
       IF ( SW_Delta ( TotSky, UpDir, TOALev_Idx) /= fqc % omit            &
            .AND. fqc % g1(3,2)/=fqc % omit)                               &
           fqc % g1(2,2) = fqc % g1(3,2)- SW_Delta ( TotSky, UpDir, TOALev_Idx)
!
       IF ( LW_Delta ( TotSky, UpDir, TOALev_Idx) /= fqc % omit            &
            .AND. fqc % g1(3,3) /= fqc % omit)                             &
           fqc % g1(2,3) = fqc % g1(3,3) - LW_Delta ( TotSky, UpDir, TOALev_Idx)
!
       IF( a_WNflx_SatEm/=fqc%omit .AND. fqc%g1(3,4)/=fqc%omit)            &
           fqc % g1 ( 2, 4) = fqc % g1 ( 3, 4) - a_WNflx_SatEm
!
       IF( a_TotLWrad/=fqc%omit .AND. fqc%g1(3,5)/=fqc%omit)               &
           fqc % g1 ( 2, 5) = fqc % g1 ( 3, 5) - a_TotLWrad
!
       IF( a_WNrad_SatEm /=fqc%omit .AND. fqc%g1(3,6)/=fqc%omit)           &
           fqc % g1 ( 2, 6) = fqc % g1 ( 3, 6) - a_WNrad_SatEm
!
       where (fqc % g1(1,1:6)/= fqc % omit                                 &
             .AND. fqc % g1(2,1:6)/= fqc % omit )                          &
           fqc % g1(4,1:6)=  fqc % g1(1,1:6) - fqc % g1(2,1:6)
!
       where (fqc % g1(1,1:6)/= fqc % omit                                 &
               .AND. fqc % g1(3,1:6)/= fqc % omit )                        &
           fqc % g1(5,1:6)=  fqc % g1(1,1:6) - fqc % g1(3,1:6)
!
       call nan_rem (fqc % g1, Default_REAL4_FL, ng1x, ng1y)
 
!
! ---------------------------------------------------------------
! Group 2 - SFC FLUXES
!
       fqc % g2  = fqc % omit
       IF( fqc % toains > 0.0 .AND. SW_sfc_dna /= fqc % omit ) then
        fqc % g2 ( 1, 1) = SW_sfc_dna/ fqc % toains  !!! SFC TRANS
        fqc % g2 ( 1, 2) = SW_sfc_dna !! LI_leighton 
       ENDIF
       fqc % g2 ( 1, 3)= LW_sfc_dnb  !! GUPTA
!
       IF (fqc % toains > 0  .AND.                                           &
            SWProf (CP, TotSky, DnDir, ModLev_Cnt ( CP)) /= fqc % omit) THEN
              fqc % g2 ( 2, 1) =                                             &
                   SWProf (CP, TotSky, DnDir, ModLev_Cnt ( CP)) / fqc % toains
       END IF
!
       fqc % g2 ( 2, 2) = SWProf ( CP, TotSky, DnDir, ModLev_Cnt ( CP))
       fqc % g2 ( 2, 3) = LWProf ( CP, TotSky, DnDir, ModLev_Cnt ( CP))
!
       WHERE (fqc % g2(1,1:3)/= fqc%omit .AND. fqc % g2(2,1:3)/= fqc % omit) &
              fqc % g2 ( 3, 1:3) = fqc % g2 ( 1, 1:3) - fqc % g2 ( 2, 1:3)
!
       CALL nan_rem (fqc % g2, Default_REAL4_FL, ng2x, ng2y)
!
! ---------------------------------------------------------------
! Group 3 - Flux Componets / DIVERGENCE
!
      fqc % g3 = fqc % omit
!++  fqc % g3 ( 1:ModLev_Cnt ( CP), 1) = SWProf ( CP, TotSky, DnDir, 1:ModLev_Cnt ( CP))
!++  fqc % g3 ( 1:ModLev_Cnt ( CP), 2) = SWProf ( CP, TotSky, UpDir, 1:ModLev_Cnt ( CP))
!++  fqc % g3 ( 1:ModLev_Cnt ( CP), 3) = LWProf ( CP, TotSky, DnDir, 1:ModLev_Cnt ( CP))
!++  fqc % g3 ( 1:ModLev_Cnt ( CP), 4) = LWProf ( CP, TotSky, UpDir, 1:ModLev_Cnt ( CP))
       do itemp = 1,4
        IF (itemp < 3) itemp2 = itemp
        IF (itemp >= 3) itemp2 = itemp + 1
        fqc % g3 ( itemp, 1) = SWProf ( CP, TotSky, DnDir, itemp2)
        fqc % g3 ( itemp, 2) = SWProf ( CP, TotSky, UpDir, itemp2)
        fqc % g3 ( itemp, 3) = LWProf ( CP, TotSky, DnDir, itemp2)
        fqc % g3 ( itemp, 4) = LWProf ( CP, TotSky, UpDir, itemp2)
       end do
!
      IF( fqc % toains > 0 )then
       IF( fqc % g3(1,1) /= fqc % omit .AND. fqc % g3(1,2) /= fqc % omit    &
       .AND. fqc % g3(2,1) /= fqc % omit .AND. fqc % g3(2,2) /= fqc % omit) &
         fqc % g3(1,5) = ((fqc % g3(1,1)-fqc % g3(1,2))                     &
                             -(fqc % g3(2,1)-fqc % g3(2,2)))/fqc % toains
!
!
       IF( fqc % g3(2,1) /= fqc % omit .AND. fqc % g3(2,2) /= fqc % omit    &
       .AND. fqc % g3(3,1) /= fqc % omit .AND. fqc % g3(3,2) /= fqc % omit) &
         fqc % g3(2,5) = ((fqc % g3(2,1)-fqc % g3(2,2))                     &
                                -(fqc % g3(3,1)-fqc % g3(3,2))) /fqc % toains
!
      IF( fqc % g3(3,1) /= fqc % omit .AND. fqc % g3(3,2) /= fqc % omit     &
      .AND. fqc % g3(4,1) /= fqc % omit .AND. fqc % g3(4,2) /= fqc % omit ) &
       fqc % g3(3,5) =  ((fqc % g3(3,1)-fqc % g3(3,2))                 &
                                 -(fqc % g3(4,1)-fqc % g3(4,2)))/fqc % toains
!
      IF( fqc % g3(4,1) /= fqc % omit .AND. fqc % g3(4,2) /= fqc % omit)    &
       fqc % g3(4,5) = (fqc % g3(4,1)-fqc % g3(4,2))/fqc % toains
      ENDIF
!
      IF( fqc % g3(1,3) /= fqc % omit .AND. fqc % g3(1,4) /= fqc % omit     &
      .AND. fqc % g3(2,3) /= fqc % omit .AND. fqc % g3(2,4) /= fqc % omit)  &
        fqc % g3(1,6) =    (fqc % g3(1,3)-fqc % g3(1,4))                    &
                         - (fqc % g3(2,3)-fqc % g3(2,4))
!
      IF( fqc % g3(2,3) /= fqc % omit .AND. fqc % g3(2,4) /= fqc % omit     &
      .AND. fqc % g3(3,3) /= fqc % omit .AND. fqc % g3(3,4) /= fqc % omit)  &
        fqc % g3(2,6) =   (fqc % g3(2,3)-fqc % g3(2,4))                     &
                        - (fqc % g3(3,3) - fqc % g3(3,4)) 
!
      IF( fqc % g3(3,3) /= fqc % omit .AND. fqc % g3(3,4) /= fqc % omit     &
      .AND.fqc % g3(4,3) /= fqc % omit .AND. fqc % g3(4,4) /= fqc % omit)   &
        fqc % g3(3,6) =   (fqc % g3(3,3)-fqc % g3(3,4))                     &
                         -(fqc % g3(4,3)-fqc % g3(4,4))
!
      IF (fqc % g3 ( 4, 3) /= fqc % omit .AND. fqc % g3 ( 4, 4) /= fqc % omit) &
       fqc % g3 ( 4, 6) = (fqc % g3 ( 4, 3) - fqc % g3 ( 4, 4)) 
!
      WHERE (fqc % g3(1:4,5) /= fqc % omit .AND. fqc % g3(1:4,6) /= fqc % omit)&
       fqc % g3(1:4,7) = fqc % g3(1:4,5)*fqc % toains +fqc % g3(1:4,6)
!
      CALL nan_rem (fqc % g3, Default_REAL4_FL, ng3x, ng3y)
!
! ---------------------------------------------------------------
! Group 4 - Tuning Adjustments
!
      fqc % g4= fqc % omit
!
!##      fqc % g4 ( 1, 1) = pts ( CP)
!##      fqc % g4 ( 1, 2) = wv ( CP)
!##      fqc % g4 ( 1, 3) = bbout ( CP)
!##      fqc % g4 ( 1, 4) = AerOptDepth ( CP)
!
      fqc % g4 ( 1, 1) = pts ( IP)
      fqc % g4 ( 1, 2) = wv ( IP)
      fqc % g4 ( 1, 3) = bbout ( IP)
      fqc % g4 ( 1, 4) = AerOptDepth ( IP)
!
! *** Reverse order of cloud fraction array to match that of the SSF
!
      CF ( IP, 1:MaxLev_Cld) = CF ( IP, MaxLev_Cld:1:-1)
      fqc % g4 ( 1, 7) = 0
      DO ic = 1,MaxLev_Cld
       IF (CF ( IP, ic) <= 100.0) fqc % g4 ( 1, 7) = fqc % g4 ( 1, 7)  + CF ( IP, ic)
      ENDDO 


!
! *** Reverse order of cloud optical depth array to match that of the SSF
!
      OptDepth_mn ( IP, 1:MaxLev_Cld) = OptDepth_mn ( IP, MaxLev_Cld:1:-1)

	WHERE( OptDepth_mn  <= 0 .OR. OptDepth_mn  > 100) OptDepth_mn =fqc % omit
	
	fqc%g4(1,5) = 0.0
	 
      do ic = 1,MaxLev_Cld
       IF (OptDepth_mn(IP,ic) /= fqc%omit .AND. CF(IP,ic) > 0.0 .AND. CF(IP,ic) <= 100.0) &
       fqc%g4(1,5) = fqc%g4(1,5)+ OptDepth_mn ( IP, ic) * CF ( IP, ic)
      enddo
 
    IF (fqc % g4 ( 1, 5) /= fqc % omit .AND. fqc % g4 ( 1, 7) /= fqc % omit) &
        fqc % g4 ( 1, 5) = fqc % g4 ( 1, 5) / fqc % g4 ( 1, 7)

!
! *** Reverse order of cloud effective temperature to match that of the SSF
!
      EffTemp ( IP, 1:MaxLev_Cld) = EffTemp ( IP, MaxLev_Cld:1:-1)

	fqc % g4 (1,6) = 0
	DO ic = 1,MaxLev_Cld
         IF(EffTemp(IP,ic) /= fqc % omit .AND. CF(IP,ic) > 0.0 .AND. CF (IP,ic) <= 100.0 ) &
          fqc%g4 (1,6) = fqc%g4(1,6) + (EffTemp ( IP, ic) * CF ( IP, ic))
	ENDDO
	
      IF (fqc % g4 ( 1, 6) /= fqc % omit .AND. fqc % g4 ( 1, 7) /= fqc % omit) &
          fqc % g4 ( 1, 6) = fqc % g4 ( 1, 6) / fqc % g4 ( 1, 7)
!
      fqc % g4 ( 2, 1) = a_pts
      fqc % g4 ( 2, 2) = a_Precip_H2O
      fqc % g4 ( 2, 3) = a_salb
      fqc % g4 ( 2, 4) = a_aer
!
      IF (a_CldFrac ( 2) /= fqc % omit) fqc % g4 ( 2, 7) = a_CldFrac ( 2)
!
      IF (a_CldFrac ( 1) /= fqc % omit)                                   &
          fqc % g4 ( 2, 7) = fqc % g4 ( 2, 7) + a_CldFrac ( 1)
!
      IF (a_OptDepth ( 2) /= fqc % omit)                                  &
          fqc % g4 ( 2, 5) = a_OptDepth ( 2)
!
      IF (a_OptDepth ( 1) /= fqc % omit)                                  &
          fqc % g4 ( 2, 5) = fqc % g4 ( 2, 5) + a_OptDepth ( 1)
!
      IF (a_CldTemp ( 2) /= fqc % omit)                                   &
          fqc % g4 ( 2, 6) = a_CldTemp ( 2)
!
      IF (a_CldTemp ( 1) /= fqc % omit)                                   &
          fqc % g4 ( 2, 6) = fqc % g4 ( 2, 6) + a_CldTemp ( 1) 
!
      fqc % g4 ( 1, 8) = uth ( CP)
      fqc % g4 ( 2, 8) = a_UpTrop_RelHum
!
!
      call nan_rem (fqc % g4, Default_REAL4_FL, ng4x, ng4y)
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! *** Group combination and Filtering for Subsets
!
      fqc % gm = fqc % omit
      DO icase = 1, ncase
          IF (fqc % icv ( icase) ) THEN
              fqc % gm ( 1:ng1, icase)                 =reshape(fqc%g1, (/ng1/))
              fqc % gm ( ng1+1:ng1+ng2, icase)         =reshape(fqc%g2, (/ng2/))
              fqc % gm ( ng1+ng2+1:ng1+ng2+ng3, icase) =reshape(fqc%g3, (/ng3/))
              fqc % gm ( ng1+ng2+ng3+1:ngt, icase)     =reshape(fqc%g4, (/ng4/))
          END IF
      END DO
!
      CALL cstats (fqc % gm, Default_REAL4_FL)                           ! I
!
!
      END SUBROUTINE Foot_QC
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE nan_rem ( x,                                      & ! I/O
                              Deflt_4real, ii, jj)                   ! I
!
!******************************************************************************
!
! !F90
!
! !Name:
!    nan_rem
!
! !Description:
!    Routine ID -
!
!    Purpose - To redefine out-of-range values, such as 'nans,' to the 
!              CERES default value
!
!
! !Input Parameters:
!    Deflt_4real - Default value assigned in the event of the occurence 
!                  of an invalid value
!    ii          - Array dimension
!    jj          - Array dimension
!
!  Input/Output Parameters:
!    x - Array to check and correct IF out of range
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 3.3   December 20, 2002 - SCCR #408
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                 1. ModIFied calling sequence to pass include the default 
!                    value for four-byte reals as an input argument
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!******************************************************************************
!
!
      REAL,    INTENT (IN)    :: Deflt_4real
      INTEGER, INTENT (IN)    :: ii
      INTEGER, INTENT (IN)    :: jj
      REAL,    INTENT (INOUT) :: x ( ii, jj)
!
      INTEGER :: i, j
!
!
      DO i = 1, ii
       DO j = 1, jj
        IF (ABS (x ( i, j) ) < 1.0E+30) THEN
        ELSE
          x ( i, j) = Deflt_4real 
        END IF
       END DO
      END DO
!
!
      END SUBROUTINE nan_rem
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
     SUBROUTINE cstats (dat, omit)                                   ! I
!
!
!******************************************************************************
!
! !F90
!
! !Name:
!    cstats
!
! !Description:
!    Routine ID -
!
!    Purpose - To accumulate QC report statistics for groups of CRS
!              parameters for each record
!
!
! !Input Parameters:
!    dat  - Array of statistics
!    omit - Default value
!
! !Output Parameters:
!    NONE
!
! !Team-Unique Header
!
!  Modules Used:
!    SARB_QC_Params - (SARBlib) Parameters: icnt, lmax, lmin, np, rmax, rmin, sa
!
! !Revision History:
!  Version 2.2   April 3, 1998 - SCCR #72
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                Lisa Coleman, SAIC, l.h.coleman@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!******************************************************************************
!
!
     REAL,    INTENT (IN) :: dat ( np)
     REAL,    INTENT (IN) :: omit
!
     INTEGER      :: ip
     REAL (REAL8) :: x1, x2, x3, x4
!
!
      DO ip = 1, np
       IF (dat ( ip) < 1.0E+7) THEN
!
        x1 = dat ( ip)
        x2 = x1 * x1
        x3 = x2 * x1
        x4 = x2 * x2
        icnt ( ip) = icnt ( ip) + 1
        sa ( ip, 1) = sa ( ip, 1) + x1
        sa ( ip, 2) = sa ( ip, 2) + x2
        sa ( ip, 3) = sa ( ip, 3) + x3
        sa ( ip, 4) = sa ( ip, 4) + x4
!
        IF (x1 > rmax ( ip) ) rmax ( ip) = x1
        IF (x1 < rmin ( ip) ) rmin ( ip) = x1
!
       END IF
      END DO
!
!
      END SUBROUTINE cstats
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE  Get_Gamma (Mod_Glint_Angle, CERES_Glint_Angle) !Output 
!
!******************************************************************************
!
! !F90
!
! !Name:
!    Get_Gamma
!
! !Description:
!    Routine ID -
!
!    Purpose - To calculate the Glint Angle for CERES and MODIS
!
!
! !Input Parameters:
!
! !Output Parameters:
!    Mod_Glint_Angle
!    CERES_Glint_Angle
!
! !Team-Unique Header
!
!  Modules Used:
!
! !Revision History:
!  Version 1.0   Oct 4, 2004 
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!******************************************************************************  
!
      REAL, INTENT (OUT) :: Mod_Glint_Angle
      REAL, INTENT (OUT) :: CERES_Glint_Angle
      REAL PI, THETA1, THETAZ, PHI1
!
      PI = 3.1415927
      THETAZ = SolarZen_angle * PI / 180.0
  
      THETA1 = vzen_toa * PI / 180.0
      PHI1 = raz_toa * PI / 180.0
      CERES_Glint_Angle = 180.0 * ACOS(COS(THETA1) * COS(THETAZ) + &
                      SIN(THETA1) * SIN(THETAZ) * COS(PHI1)) / PI
!  
      THETA1 = imag_vzen * PI / 180.0
      PHI1 = imag_raz * PI / 180.0 
      Mod_Glint_Angle = 180.0 * ACOS(COS(THETA1) * COS(THETAZ) + &
                      SIN(THETA1) * SIN(THETAZ) * COS(PHI1)) / PI  
  
!
      END SUBROUTINE Get_Gamma  
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Tau_Fov (Tau_Mean_Fov)
!
!
!******************************************************************************
!
! !F90
!
! !Name:
!    Tau_Fov
!
! !Description:
!    Routine ID -
!
!    Purpose - To calculate the mean FOV optical depth
!
!
! !Input Parameters:
!
! !Output Parameters:
!    Tau_Mean_Fov
!
! !Team-Unique Header
!
!  Modules Used:
!
! !Revision History:
!  Version 1.0   Oct 4, 2004 
!                Scott Zentz, SAIC, s.m.zentz@larc.nasa.gov
!                Initial Version of Code
!
! !END
!
!******************************************************************************  
!
       REAL, INTENT(OUT) :: Tau_Mean_Fov
       INTEGER nexist, i
       REAL scf, slt, taulogmnfov

       scf = 0.0
       slt = 0.0
       nexist = count (CF(InitPass, 1:MaxLev_Cld) > 0 .AND. CF(InitPass, 1:MaxLev_Cld) <= 100.0)
       IF ( nexist > 0) THEN
        DO i = 1, MaxLev_Cld 
         IF ( CF(InitPass, i) > 0 .AND. CF(InitPass, i) <= 100.0) THEN
          scf = CF(InitPass, i) + scf
          slt = CF(InitPass, i) * OptDepth_mn(InitPass, i) + slt
         ENDIF
        END DO 
       ENDIF
!
       IF ( scf >0) THEN
         taulogmnfov = slt/scf
         Tau_Mean_Fov = EXP( taulogmnfov )
       ELSE 
         Tau_Mean_Fov = Default_REAL4_FL
       ENDIF
!
!
       END SUBROUTINE Tau_Fov             
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!   
END MODULE QC_Accum
