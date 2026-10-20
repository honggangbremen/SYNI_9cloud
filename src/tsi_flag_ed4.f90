Module TSI_FLAG_ED4
implicit none
! PARSE the flag "TSI%fluxestsi%data_flg" into individual values 

TYPE TSI_Cld_Flx_Source 
sequence
 integer cldprop
 integer lwtot ! 1= Ceres 2=GEO 3=intrp
 integer swtot
 integer lwclr
 integer swclr !1= Ceres 2=GEO 3=intrp 4=CSmap
END TYPE TSI_Cld_Flx_Source

TYPE ( TSI_Cld_Flx_Source ) src

 CONTAINS
!===================================================================================
SUBROUTINE parse_tsi_dataflag(iflag)
 integer*4 , intent(IN) :: Iflag 
 character*5 cs

!  Parse the 5 digit integer flag represents the data Source for the Cloud properties and
! TOA Fluxes from ED4 TSI  There are 5 parts in the flag,
!
! X0000 = Cloud Properties Source (Ten Thousands Digit)
!  X000  = TOT LW Source             ( Thousands Digit)
!   X00   = TOT SW Source             ( Hundreds Digit)
!    X0    = Clr LW Source                ( Tens Digit)
!     X     = Clr SW Source             ( Single Digit)
!
!   The meaning of the FLUX flags (0XXXX):
!   =  1  : CERES data
!   =  2  : GGEO data (can be default if there is no CERES observationin the day)
!   =  3  : interpolated data
!   =  4 : Clear sky map ( for SW CLEAR ONLY)
!   =  0  : default
!
!--- CLOUD PROPERTY SOURCE flag order: X0000
!  The meanings of the numbers are
!  1 = CERES Terra data
!  2 = CERES Aqua data
!
!!  3,4,5,6 are GGEO data
!  3 = Geo 3chan	MET7		  58
!  4 = Geo 12um 	GEO11, MTSAT1	  76,84
!  5 = Geo 13.2um	GEO12		  78
!  6 = Geo 12+13.2 chan   MET9  	  52
!  7 = spare TBD Fm5???
!  8 = spare TBD Fm6???
!  9 =Interpolate
!  0 = default
!-----------------------------------------------------------------------------

if ( iflag < 0 .or. iflag > 99999 ) then
!BAD FLAG ORV assign defaults to all
 src%cldprop =0
 src%lwtot =0
 src%swtot =0
 src%lwclr =0
 src%swclr =0
return
endif

write(cs,"(i5)") iflag

read(cs(1:1),"(i1)") src%cldprop
read(cs(2:2),"(i1)") src%lwtot
read(cs(3:3),"(i1)") src%swtot
read(cs(4:4),"(i1)") src%lwclr
read(cs(5:5),"(i1)") src%swclr

if ( src%lwtot < 0 .or. src%lwtot > 3 ) src%lwtot = 0
if ( src%swtot < 0 .or. src%swtot > 3 ) src%swtot = 0
if ( src%lwtot < 0 .or. src%lwtot > 3 ) src%lwclr = 0
if ( src%swclr < 0 .or. src%swclr > 3 ) src%swclr = 0

end SUBROUTINE parse_tsi_dataflag

!====================================================================================
integer function ihandle_intrp_flag(iflag)
!Further use parsed TSI Flux Src flag for index for tuning sigma selection (2-10) See Below
 implicit none
 integer*4 , intent(IN) :: Iflag 

 call parse_tsi_dataflag(iflag) !!!! PARSE FLAG

 ihandle_intrp_flag = 10

 if ( src%lwtot <=0 .or. src%lwtot >3 .or. &
      src%swtot <=0 .or. src%swtot >3 ) return
      
! ihandle_intrp_flag = (src%lwtot-1)*3 + src%swtot + 1
!ihandle_intrp_flag = (src%swtot-1)*3 + src%lwtot + 1   !20141203
ihandle_intrp_flag = src%cldprop !20141204
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 2 = CERES SW, CERES LW
! 3 = CERES SW, Geo LW
! 4 = CERES SW, Interp LW!
!
! 5 = GEO SW, CERES LW
! 6 = GEO SW, Geo LW
! 7 = GEO SW, Interp LW
!
! 5 = Interp SW, CERES LW
! 6 = Interp SW, Geo LW
! 7 = Interp SW, Interp LW
!
!&ST_SIGF
!  pct_flux_tol(1:5,1) = 1.0 1.0 2.0 5.0 5.0,
!  pct_flux_tol(1:5,2) = 1.0 1.0 2.0 5.0 5.0,
!  pct_flux_tol(1:5,3) = 1.0 3.0 4.0 7.0 7.0,
!  pct_flux_tol(1:5,4) = 1.0 6.0 8.0 9.0 9.0,
!
!  pct_flux_tol(1:5,5) = 3.0 1.0 2.0 5.0 5.0,
!  pct_flux_tol(1:5,6) = 3.0 3.0 4.0 7.0 7.0,
!  pct_flux_tol(1:5,7) = 3.0 6.0 8.0 9.0 9.0,
!
!  pct_flux_tol(1:5,8) = 6.0 1.0 2.0 5.0 5.0,
!  pct_flux_tol(1:5,9) = 6.0 3.0 4.0 7.0 7.0,
!  pct_flux_tol(1:5,10) = 6.0 6.0 8.0 9.0 9.0,

end function ihandle_intrp_flag
!====================================================================================

end module TSI_FLAG_ED4
!================================================================
!================================================================
subroutine test_tsi_flag_ed4
USE TSI_FLAG_ED4

do i4 =0,4
do i3 =0,3

do i2 =0,3
do i1 =0,3

do i0 =1,6

iflag = 2222
iflag = i0*10000 + i1*1000 + i2*100 +i3*10 +i4


! call parse_tsi_dataflag(iflag)


print'(I5.5,I4,5x,5I3)',iflag , ihandle_intrp_flag(iflag ) ,&
src%cldprop, &
src%lwtot, &
src%swtot, &
src%lwclr, &
src%swclr

enddo
enddo
enddo
enddo
enddo

end subroutine test_tsi_flag_ed4

! call test_tsi_flag_ed4 ; end
