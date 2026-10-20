MODULE wssacomp

CONTAINS

real function getwssa(nc,aots,itypfl)
! This Function provides the constituent weighted aerosol
! Weighted SSA for use as a look up index for "FLSA00426"
! the index gives the aproximated absorption properties of
! mix of aerosol constituents.
! The constituents are as used in FULIOU20040426 aerosol types 1-25.

!    1   0.013 1.Maritime (8 sets of RH dependent properties)              
!    2   0.057 2.CONTINENTAL(8 sets of RH dependent properties)            
!    3   0.082 3.URBAN (8 sets of RH dependent properties)                 
!    4   0.057 4.0.5 Micron Mineral_Dust                                   
!    5   0.094 5.1.0 Micron Mineral_Dust                                   
!    6   0.152 6.2.0 Micron Mineral_Dust                                   
!    7   0.226 7.4.0 Micron Mineral_Dust                                   
!    8   0.306 8.8.0 Micron Mineral_Dust                                   
!    9   0.224 9.inso Insoluble                                            
!   10   0.058 10.waso Water Soluble (8 sets of RH dependent properties)   
!   11   0.826 11.soot Soot                                                
!   12   0.001 12.ssam Sea Salt (Accumulation Mode) (8 sets of RH  prop)   
!   13   0.011 13.sscm Sea Salt (Coarse Mode) (8 sets of RH dependent prop)
!   14   0.034 14.minm Mineral Dust (Nucleation Mode)                      
!   15   0.092 15.miam Mineral Dust (Accumulation Mode)                    
!   16   0.281 16.micm Mineral Dust (Coarse Mode)                          
!   17   0.124 17.mitr Mineral Dust (Transported Mode)                     
!   18   0.002 18.suso Sulfate Droplets (8 sets of RH dependent properties)
!   19   0.028 19 0.5 Micron Mineral_Dust   NEW LACIS                               
!   20   0.047 20 1.0 Micron Mineral_Dust   NEW LACIS                                
!   21   0.082 21 2.0 Micron Mineral_Dust   NEW LACIS                                
!   22   0.131 22 4.0 Micron Mineral_Dust   NEW LACIS                                
!   23   0.194 23 8.0 Micron Mineral_Dust   NEW LACIS
!   24   0.020 24 Match [0.1-0.5] Micron Mineral_Dust                             
!   25   0.065 25 Match [0.5-5.0] Micron Mineral_Dust 
implicit none
integer, parameter :: maxaer =25 
real aots(nc),saot,a1
integer itypfl(nc),i,nc,it
real wssatbl(maxaer)
data wssatbl/0.013,0.057,0.082,0.057,0.094,0.152, &
	     0.226,0.306,0.224,0.058,0.826,0.001, &
	     0.011,0.034,0.092,0.281,0.124,0.002, &
	     0.028,0.047,0.082,0.131,0.194, &
	     0.024,0.052/ 
         

saot=0.0
getwssa=0.0
do i=1,nc
a1=aots(i)
it=itypfl(i)

if ( a1 > 0 .and. a1 <10 .and.  it >0 .and. it<=maxaer) then
 getwssa=getwssa+ a1*wssatbl(it)
 saot=saot+a1
endif
enddo

if (saot == 0.0) then
  getwssa = 0.0
else 
  getwssa=getwssa/saot
endif  

end  function getwssa

END MODULE wssacomp
