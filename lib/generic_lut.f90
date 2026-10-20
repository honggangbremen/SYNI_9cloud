!===========================================================================
MODULE generic_lut
 implicit none
    


 TYPE LT_TYPE
  integer nv
  integer nrec
  integer mxn
  real scale
  
  integer  ,pointer   ,dimension(:) ::  nva, iva, ibase, itop
  real     ,pointer    ,dimension(:) ::  wb, wt, rva, val
  
  real     ,pointer  ::  anodes(: ,:)
  logical  ,pointer  :: ichoose(:, :)
  integer  ,pointer  ::    ivax(:, :)  
  integer*2,pointer  ::  itable(:)

  real               ::  result !! OUTPUT
 END TYPE LT_TYPE
 

 CONTAINS
!-----------------------------------------------------------------------
subroutine lut_guts(lt)
TYPE ( LT_TYPE ) lt
integer :: iv ,in ,ik  , i , j, k
real :: rmin,rmax
real*8 :: wx,swx,stab

VSET : do iv=1,lt%nv 
 rmin = lt%anodes(1,iv)
 rmax = lt%anodes(lt%nva(iv),iv)

  if (lt%val(iv) <= rmin )then 
    lt%rva(iv) = 1
  elseif (lt%val(iv) >= rmax ) then
    lt%rva(iv) = lt%nva(iv)
  else

  FNODES : do in = 1,lt%nva(iv)-1
   if(lt%val(iv) >= lt%anodes(in,iv) .and. &
      lt%val(iv) < lt%anodes(in+1,iv) )  &
      lt%rva(iv)= in+( lt%val(iv)-lt%anodes(in,iv)) / &
	      (lt%anodes(in+1,iv)-lt%anodes(in,iv))
  enddo FNODES

  endif

!print*,iv,rva(iv)
enddo VSET
!---------------------

  lt%ibase = floor(lt%rva)
  where( lt%ibase < 1) lt%ibase=1

  lt%itop= ceiling(lt%rva)
  where( lt%itop > lt%nva) lt%itop=lt%nva

  lt%wb=1.0-(lt%rva-lt%ibase)
  lt%wt=1.0-lt%wb
	
!  print'(A10,10f5.1)','Exact',rva(1:nv)
!  print'(A10,10I5)','BASE',ibase
!  print'(A10,10I5)','TOP',itop
!  print'(A10,10f5.1)','Wb',wb     
!  print'(A10,10f5.1)','Wt',wt

 swx=0
 ik=0
 lt%result=0

  NODE : do j=1,2**lt%nv
  wx=1.0

   DIMENSION: do i=1,lt%nv

    if( lt%ichoose(i,j) ) then
      lt%ivax(i,j) = lt%itop(i)
      wx=wx*lt%wt(i)
    else
      lt%ivax(i,j) = lt%ibase(i)
      wx=wx*lt%wb(i)
    endif

   enddo DIMENSION

   if( wx > 1.0E-10 ) then
   ik=ik+1
!!!   k = ilui(lt%nva,lt%nv,lt%ivax(1,j))
   k = ilui( lt, j )
   swx  =   swx  +     wx

   lt%result =   lt%result +	( lt%itable(k)/lt%scale )    * wx 

!  print'(3I12,3f10.5,8i3)',ik,j,k,   wx,swx, lt%itable(k)/lt%scale ,lt%ivax(1:lt%nv,j)
   if (swx >= .9999)exit
  endif

  enddo NODE

  if ( swx .ne. 1.0000) lt%result =  lt%result /swx !!! Incase of partial weightings

!  print'(f8.4)' ,lt%result

	return	
end subroutine lut_guts	
!-----------------------------------------------------------------------
subroutine lut_init(lt)
  TYPE ( LT_TYPE ) lt
   integer i,ii


 allocate ( lt%nva(   lt%nv ) ) 
 allocate ( lt%iva(   lt%nv ) ) 
 allocate ( lt%ibase( lt%nv ) ) 
 allocate ( lt%itop(  lt%nv ) ) 

 allocate ( lt%wb(    lt%nv ) ) 
 allocate ( lt%wt(    lt%nv ) ) 
 allocate ( lt%rva(   lt%nv ) ) 
 allocate ( lt%val(   lt%nv ) ) 



allocate (  lt%itable(lt%nrec) )

allocate ( lt%ivax(lt%nv, 2**lt%nv) )

allocate( lt%ichoose(lt%nv, 2**lt%nv) )

allocate (  lt%anodes(lt%mxn, lt%nv) )

!--- TF Table initalization -------------------
  	do i=1,2**lt%nv
	do ii=1,lt%nv
	lt%ichoose(ii,i)=btest(i-1,ii-1)
	enddo
!	print'(I4,4x,16L2)',i,lt%ichoose(1:lt%nv,i)
	enddo
	return
	
end subroutine lut_init
!-----------------------------------------------------------------------
	integer function ilui(lt,jrow)
        TYPE ( LT_TYPE ) lt
	integer jrow
	integer i,k
	integer ,dimension(lt%nv) ::  mva
	
	mva(1)=lt%nva(1)
	do i=2,lt%nv
	 mva(i)=mva(i-1)*lt%nva(i)
	enddo

	k= lt%ivax(1,jrow)
	do i=2,lt%nv
	k= k+ ( lt%ivax(i,jrow)-1)*mva(i-1)
	enddo

	ilui=k
	return
	end function ilui

END MODULE generic_lut
!===========================================================================

