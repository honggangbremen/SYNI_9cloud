pro remove_all_old

  openw, 1, 'test_rm.csh'

for ilat=3, 180 do begin

   print, 'ilat, ',  ilat
   
one_lat=STRING(ilat, FORMAT='(I3.3)')
printf, 1, './cleanup_7.2.1P2.pl CER7.2.1P2_PCF_Aqua-MODIS_Alpha4-Ed5_000000.202004Z'+one_lat

endfor
close, 1



  stop

  end
