MODULE Control_Mod
!
!
!*****************************************************************************
!
! !F90
!
! !Name:
!   Control_Mod
!
! !Description:
!    Routine ID - ?
!
!    Purpose - To provide a module of routines to control subsystem processing.
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
!  Key Local Parameters:
!   Aer_Key                - Hierarchy of aerosol data sources
!   Aer_Rules              - Aerosol selection rules used by main processor
!   AerRangeMax            - Place holder for two rule values (min and max)
!   AerRuleMax             - Maximum number of control rules allowed for aerosols
!   Cloud_Frac             - Holder variable for percentage of cloudy skies
!   Default_AerKey         - Default heirarchy of aerosol data sources
!   Default_SigKey         - Default sigma data source
!   NumAerRules            - Number of aerosol control rules from control file
!   NumAerSources          - Number of aerosol data sources
!   NumSigRules            - Number of sigma control rules from control file
!   NumSigSources          - Number of sigma data sources
!   Rules_Aerosol          - Data type to contain aerosol source selection rules
!   Rules_Sigma            - Data type to contain sigma source selection rules
!   Sig_Key                - sigma data sources
!   Sig_Rules              - Sigma selection rules used by main processor
!   SigRangeMax            - Place holder for two rule values (min and max)
!   SigRuleMax             - Maximum number of control rules allowed for sigmas
!   SnowIce_Frac           - Holder variable for SSF snow/ice flag
!
!  Internal Routines:
!    Aer_Rule_Select       - Sets aerosol source according to control file rules
!    Control_Init          - Retrieves processing rules from control file
!    Sig_Rule_Select       - Sets sigma source according to control file rules
!
!  Modules Used:
!    IO           - (CERESlib) Used by Sbr. Control_Init
!
! !Revision History:
!  Version 4.0   June 10, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Fred Rose, AS&M, f.g.rose@larc.nasa.gov
!                1. Added reading of tuning control section from 
!                   ControlFile for variable sigmas 
!                2. Moved Aer_Rule_Select from SARBAer_Utils to here 
!                2. Moved aerosol rule variables from SARBAer_Var to here 
!                3. Added Sig_Rule_Select and appropriate variables 
!                4. Added Func. ihandle_intrp_flag to decode new TISA flux  
!                   source flag
!  Version 3.5   August 1, 2003 - SCCR #458 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
      USE TSI_FLAG_ED4,only : ihandle_intrp_flag
      USE IO, ONLY : CloseFile, OpenFile
      USE MOA_IO_NESTED, ONLY : LatLong_Reg
      USE grid_nest_routines
!
      IMPLICIT NONE
!
      REAL,       SAVE      :: Cloud_Frac
      REAL,       SAVE      :: SnowIce_Frac
!
      INTEGER,  PARAMETER :: NumAerSources = 6
      INTEGER,  PARAMETER :: AerRangeMax = 2
      INTEGER,  PARAMETER :: AerRuleMax = 100
!
      INTEGER,    SAVE      :: NumAerRules
      INTEGER,    SAVE      :: Aer_Key ( NumAerSources)
      INTEGER,    SAVE      :: Default_AerKey ( NumAerSources)
!
      TYPE Rules_Aerosol

       INTEGER IGBP 
       REAL Lat ( AerRangeMax)
       REAL CldFrac ( AerRangeMax)
       REAL u0 ( AerRangeMax)
       REAL Ice ( AerRangeMax)
       INTEGER Hierarchy ( NumAerSources)

      END TYPE Rules_Aerosol 
!
      TYPE (Rules_Aerosol),  SAVE :: Aer_Rules (AerRuleMax)
!
      INTEGER,  PARAMETER :: NumSigSources = 6
      INTEGER,  PARAMETER :: SigRangeMax = 2
      INTEGER,  PARAMETER :: SigRuleMax = 100
!
      INTEGER,    SAVE      :: NumSigRules
      INTEGER,    SAVE      :: Sig_Key 
      INTEGER,    SAVE      :: Default_SigKey 
!
      TYPE Rules_Sigma

       INTEGER IGBP 
       REAL Lat ( SigRangeMax)
       REAL CldFrac ( SigRangeMax)
       REAL u0 ( SigRangeMax)
       REAL Ice ( SigRangeMax)
       INTEGER Int_Flag
       INTEGER Sig_Table

      END TYPE Rules_Sigma 
!
      TYPE (Rules_Sigma),  SAVE :: Sig_Rules (SigRuleMax)
!
      INTEGER,  PARAMETER :: NumSites = 172
!
      TYPE SiteListType

       CHARACTER(LEN=4) zone 
       INTEGER region
       CHARACTER(LEN=3) code 
       REAL lat 
       REAL long
       CHARACTER(LEN=60) name

      END TYPE SiteListType 
!
      TYPE (SiteListType),  SAVE :: Site_List (NumSites)
!
!
CONTAINS
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Control_Init (Rules_LID)                                  ! I
!
!*****************************************************************************
!
! !F90
!
! !Name:
!    Control_Init 
!
! !Description:
!   Routine ID - ?
!
!   Purpose - To open the control file and read the data source 
!             selection rules
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
!    IO                    - Sbr. OpenFile, CloseFile
!
!  Internal Routines Used:
!    NONE
!
! !Revision History
!  Version 3.5   August 1, 2003 - SCCR #458 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Initial Version of code
!
! !END
!
!*****************************************************************************
!
!
      INTEGER, INTENT (IN) :: Rules_LID
!
      INTEGER :: I, IOS, StringMatch
      INTEGER :: Rule_Unit
      CHARACTER (LEN=30)  :: CLine
!
      INTEGER :: igbp, latmin, latmax, Hier(NumAerSources)
      INTEGER :: intflag, sigtable
      REAL    :: cldmin, cldmax, umin, umax, icemin, icemax
!
      CALL OpenFile (LogicID = Rules_LID, status = IOS,       & ! I
                     handle = Rule_Unit,                      & ! I
                     mode = "R", access ="S",                 & ! I
                     format = "F")                              ! I
!
!
      DO WHILE (IOS == 0)
!
       READ (Rule_Unit, "(a30)", IOSTAT=IOS) CLine 
       IF (IOS == 0) THEN


        StringMatch = 0
        StringMatch = INDEX (CLine, "-->TUNECONTROLL<--")       
        IF ( StringMatch > 0) THEN
         READ (Rule_Unit, "(a30)", IOSTAT=IOS) CLine 
         READ (Rule_Unit, "(i2)", IOSTAT=IOS) Default_SigKey
         READ (Rule_Unit, "(i2)", IOSTAT=IOS) NumSigRules
         READ (Rule_Unit, "(a30)", IOSTAT=IOS) CLine 
         DO I = 1, NumSigRules
          READ (Rule_Unit, 100, IOSTAT=IOS) igbp, latmin, latmax, & 
                                            cldmin, cldmax, umin, & 
                                            umax, icemin, icemax, & 
                                            intflag, sigtable  
          Sig_Rules(I) % IGBP                 = igbp 
          Sig_Rules(I) % Lat(1)               = latmin 
          Sig_Rules(I) % Lat(SigRangeMax)     = latmax 
          Sig_Rules(I) % CldFrac(1)           = cldmin 
          Sig_Rules(I) % CldFrac(SigRangeMax) = cldmax 
          Sig_Rules(I) % u0(1)                = umin 
          Sig_Rules(I) % u0(SigRangeMax)      = umax 
          Sig_Rules(I) % Ice(1)               = icemin 
          Sig_Rules(I) % Ice(SigRangeMax)     = icemax 
          Sig_Rules(I) % Int_Flag             = intflag
          Sig_Rules(I) % Sig_Table            = sigtable
         ENDDO 
        ENDIF

        StringMatch = 0
        StringMatch = INDEX (CLine, "-->AOTCONTROLL<--")       
        IF ( StringMatch > 0) THEN
         READ (Rule_Unit, "(a30)", IOSTAT=IOS) CLine 
         READ (Rule_Unit, 210, IOSTAT=IOS) Default_AerKey
         READ (Rule_Unit, "(i2)", IOSTAT=IOS) NumAerRules
         READ (Rule_Unit, "(a30)", IOSTAT=IOS) CLine 
         DO I = 1, NumAerRules
          READ (Rule_Unit, 200, IOSTAT=IOS) igbp, latmin, latmax, & 
                                            cldmin, cldmax, umin, & 
                                            umax, icemin, icemax, & 
                                            Hier(1:NumAerSources)  
          Aer_Rules(I) % IGBP                 = igbp 
          Aer_Rules(I) % Lat(1)               = latmin 
          Aer_Rules(I) % Lat(AerRangeMax)     = latmax 
          Aer_Rules(I) % CldFrac(1)           = cldmin 
          Aer_Rules(I) % CldFrac(AerRangeMax) = cldmax 
          Aer_Rules(I) % u0(1)                = umin 
          Aer_Rules(I) % u0(AerRangeMax)      = umax 
          Aer_Rules(I) % Ice(1)               = icemin 
          Aer_Rules(I) % Ice(AerRangeMax)     = icemax 
          Aer_Rules(I) % Hierarchy            = Hier
         ENDDO 
         EXIT
        ENDIF
       ENDIF
!
      ENDDO
!
      CALL CloseFile (file_handle = Rule_Unit, status = IOS)
!
!
100   FORMAT (i2,3x,i3,2x,i2,3x,f4.2,1x,f4.2,3x,f5.2,1x,f4.2, &
      3x,f4.2,1x,f4.2,3x,i2,9x,i2) 
!
200   FORMAT (i2,3x,i3,2x,i2,3x,f4.2,1x,f4.2,3x,f5.2,1x,f4.2, &
      3x,f4.2,1x,f4.2,3x,i1,1x,i1,1x,i1,1x,i1,1x,i1,1x,i1) 
210   FORMAT (i1,1x,i1,1x,i1,1x,i1,1x,i1,1x,i1) 
!
      END SUBROUTINE Control_Init
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Aer_Rule_Select (IGBP, Lat, CldFrac, u0, Ice)    ! I
!
!*****************************************************************************80
!
! !F90
!
! !Name:
!   Aer_Rule_Select
!
! !Description:
!   Routine ID -
!
!   Purpose - Checks the footprint data for rule compliance
!
!
! !Input Parameters:
!   CldFrac   - Percentage of cloud covered skies 
!   Ice       - Snow/ice flag from SSF 
!   IGBP      - Scene index from the SSF 
!   Lat       - Latitude
!   u0        - Cosine of solar zenith angle 
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 3.5   August 1, 2003 - SCCR #458 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Initial version of code
!
! !END
!
!*****************************************************************************80
!
!
!
      INTEGER, INTENT (IN) :: IGBP
      REAL,    INTENT (IN) :: Lat 
      REAL,    INTENT (IN) :: CldFrac 
      REAL,    INTENT (IN) :: u0 
      REAL,    INTENT (IN) :: Ice 
!   
      LOGICAL :: Rule_Flag
      INTEGER :: I
!
      Rule_Flag = .FALSE.
      Aer_Key = 0
!
      DO I = 1, NumAerRules
       IF ( (IGBP == Aer_Rules(I)%IGBP .OR. Aer_Rules(I)%IGBP == 0) .AND. &
                                   Lat >= Aer_Rules(I)%Lat(1)       .AND. & 
                         Lat <= Aer_Rules(I)%Lat(AerRangeMax)       .AND. &     
                           CldFrac >= Aer_Rules(I)%CldFrac(1)       .AND. &
                      CldFrac <= Aer_Rules(I)%CldFrac(AerRangeMax)  .AND. &     
                                     u0 >= Aer_Rules(I)%u0(1)       .AND. & 
                            u0 <= Aer_Rules(I)%u0(AerRangeMax)      .AND. &    
                                   Ice >= Aer_Rules(I)%Ice(1)       .AND. & 
                         Ice <= Aer_Rules(I)%Ice(AerRangeMax)) THEN     
!
        Rule_Flag = .TRUE.
!
       ENDIF
!
       IF (Rule_Flag) THEN
        Aer_Key = Aer_Rules(I) % Hierarchy
        EXIT
       ELSE
        Aer_Key = Default_AerKey
       ENDIF
      ENDDO
!
!
      END SUBROUTINE Aer_Rule_Select
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Sig_Rule_Select (IGBP, Lat, CldFrac, u0, Ice, TISA_Flag)    ! I
!
!*****************************************************************************80
!
! !F90
!
! !Name:
!   Sig_Rule_Select
!
! !Description:
!   Routine ID -
!
!   Purpose - Checks the footprint data for rule compliance
!
!
! !Input Parameters:
!   CldFrac   - Percentage of cloud covered skies 
!   Ice       - Snow/ice flag from SSF 
!   IGBP      - Scene index from the SSF 
!   Lat       - Latitude
!   u0        - Cosine of solar zenith angle 
!   TISA_Flag - Flag from TSI indicating source of TSI flux profiles 
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 4.0   June 10, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Initial version of code
!
! !END
!
!*****************************************************************************80
!
!


      INTEGER, INTENT (IN) :: IGBP
      REAL,    INTENT (IN) :: Lat 
      REAL,    INTENT (IN) :: CldFrac 
      REAL,    INTENT (IN) :: u0 
      REAL,    INTENT (IN) :: Ice 
      INTEGER, INTENT (IN) :: TISA_Flag
!   
      LOGICAL :: Rule_Flag
      INTEGER :: I
      INTEGER :: Int_Flag
!
      Rule_Flag = .FALSE.
      Sig_Key = 0
!
!  Decode TISA flux flag to select tolerances
!
      Int_Flag = ihandle_intrp_flag(TISA_Flag)
!
      DO I = 1, NumSigRules
       IF ( (IGBP == Sig_Rules(I)%IGBP .OR. Sig_Rules(I)%IGBP == 0) .AND. &
                                   Lat >= Sig_Rules(I)%Lat(1)       .AND. & 
                         Lat <= Sig_Rules(I)%Lat(SigRangeMax)       .AND. &     
                           CldFrac >= Sig_Rules(I)%CldFrac(1)       .AND. &
                      CldFrac <= Sig_Rules(I)%CldFrac(SigRangeMax)  .AND. &     
                                     u0 >= Sig_Rules(I)%u0(1)       .AND. & 
                            u0 <= Sig_Rules(I)%u0(SigRangeMax)      .AND. &    
                                   Ice >= Sig_Rules(I)%Ice(1)       .AND. & 
                          Ice <= Sig_Rules(I)%Ice(SigRangeMax)      .AND. & 
                            Int_Flag == Sig_Rules(I)%Int_Flag) THEN     
!
        Rule_Flag = .TRUE.
!
       ENDIF
!
       IF (Rule_Flag) THEN
        Sig_Key = Sig_Rules(I) % Sig_Table
        EXIT
       ELSE
        Sig_Key = Default_SigKey
       ENDIF
      ENDDO
!
!
      END SUBROUTINE Sig_Rule_Select
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      SUBROUTINE Site_Compare(Lat, Long, SFlag)
!
!*****************************************************************************80
!
! !F90
!
! !Name:
!   Sig_Rule_Select
!
! !Description:
!   Routine ID -
!
!   Purpose - Checks the footprint data for rule compliance
!
!
! !Input Parameters:
!   CldFrac   - Percentage of cloud covered skies 
!   Ice       - Snow/ice flag from SSF 
!   IGBP      - Scene index from the SSF 
!   Lat       - Latitude
!   u0        - Cosine of solar zenith angle 
!   TISA_Flag - Flag from TSI indicating source of TSI flux profiles 
!
! !Output Parameters:
!   NONE
!
! !Team-Unique Header:
!
! !Revision History:
!  Version 4.0   June 10, 2004 - SCCR #541 
!                Thomas Caldwell, SAIC, t.e.caldwell@larc.nasa.gov
!                Initial version of code
!
! !END
!
!*****************************************************************************80
!
!
!
      INTEGER, INTENT (IN) :: Lat 
      INTEGER, INTENT (IN) :: Long 
      INTEGER, INTENT (OUT) :: SFlag 
!   
      INTEGER :: I, L, RegNum, RegPerZone 
!
      SFlag = 0
      RegNum = 0      
!      
      IF (Lat == 1) THEN
       RegNum = 1
      ELSE
       DO L = 1, Lat - 1
        RegPerZone = number_nested_regions(L)
        RegNum = RegNum + RegPerZone
       ENDDO
       RegNum = RegNum + Long
      ENDIF      
!      
      DO I = 1, NumSites
       IF ( Site_List(I)%region == RegNum) THEN     
        SFlag = 1
       ENDIF
      ENDDO
!
!
      END SUBROUTINE Site_Compare
!
!!==================================================================
!integer function ihandle_intrp_flag(iflag)
!
!!           A 4-digit integer flag represents the data type for the TOA SW and
!  !           LW fluxes is added in the Fluxes_tsi type structure.
!  !           There are 4 parts in the flag, from left to right:
!  !             - clear-sky LW flux
!  !             - clear-sky SW flux
!  !             - total-sky LW flux
!  !             - total-sky SW flux
!  !
!  !           The meaning of the flag:
!  !           =  1  : CERES data
!  !           =  2  : GGEO data (can be default if there is no CERES observation
!  !                                     in the day)
!  !           =  3  : interpolated data
!  !           =  0  : default
!
!
!implicit none
! integer iflag,itslw,itssw,is
! character*4 cs
!
!ihandle_intrp_flag = 0 
!if ( iflag < 0 .or. iflag > 3333 ) return
!
!write(cs,"(i4)") iflag
!
!!read(cs(1:1),"(i1)") icslw
!!read(cs(2:2),"(i1)") icssw
!read(cs(3:3),"(i1)") itslw
!read(cs(4:4),"(i1)") itssw
!
!is = (itslw-1)*3 + itssw + 1
!
!if ( itslw <= 0 .or. itslw > 3 ) is = 10  !! Assume if outof range assume interpolated
!if ( itssw <= 0 .or. itssw > 3 ) is = 10
!
!ihandle_intrp_flag = is 
!
!end function ihandle_intrp_flag
!==================================================================
!
END MODULE Control_Mod
