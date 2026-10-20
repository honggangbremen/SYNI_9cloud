

pro run_20130416_script_tsi

;all_nodes=['zf08.cluster.net', 'zf09.cluster.net', 'zf10.cluster.net', 'zf11.cluster.net', 'zf12.cluster.net', $
;           'zf13.cluster.net', 'zf14.cluster.net', 'zf15.cluster.net', 'zf16.cluster.net', $
;           'zf17.cluster.net', 'zf18.cluster.net']

for ilat=11, 180 do begin

   print, 'ilat, ',  ilat
   
one_lat=STRING(ilat, FORMAT='(I3.3)')

;naa = (ilat mod 10)

;node_index = naa


;spawn, './cleanup_7.2.1P2.pl CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z'+one_lat
;
;wait, 1
;
;spawn, './CER7.2.1P2_pcfgen.pl -date 201304Z'+one_lat
;
;wait, 1


;stop

;; change the TSI file dir.

pcf_file='/homedir/ghong/sarb/CER7.2.1P2/rcf/pcf/CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z'+one_lat
replace_lines_starting_with_12, pcf_file, '12|CER_TSIB_Terra-Aqua-MODIS_Edition4A_401405.201304Z001|/CERES/clouds/workArea/hong/temp/sarb/2013/04_09||||1'

;close, 5
;openr, 5, './pcf/CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z'+one_lat
;close, 5

;stop



;; Every 10 jobs, except after the last job, wait 5 hours
;IF ((ilat mod 10) EQ 0) AND (ilat ne 0) THEN BEGIN
;        PRINT, 'Submitted 10 jobs, sleeping 5 hours...'
;        wait, 3600*5
;endif else begin

   openw, 1, 'day20130416_'+one_lat+'.csh'  

printf, 1, '#!/bin/csh'
printf, 1, '#$ -S /bin/csh'
printf, 1, '#$ -l arch=lx-amd64'
printf, 1, '#$ -q scf-rhel.q'
;printf, 1, '#$ -l h=zf14.cluster.net,zf15.cluster.net,zf16.cluster.net,zf17.cluster.net,zf18.cluster.net,zf19.cluster.net'
printf, 1, '#$ -o /CERES/clouds/workArea/hong/temp/sarb/data/output.log'
printf, 1, '#$ -e /CERES/clouds/workArea/hong/temp/sarb/data/error.log'
printf, 1, ' '
printf, 1, ' '
 
printf, 1, 'set workdir=/homedir/ghong/sarb/CER7.2.1P2/rcf/'
printf, 1, 'cd ${workdir}'
;printf, 1, '${workdir}run_CER7.2.1P2.pl CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z'+one_lat+' > /CERES/clouds/workArea/hong/temp/sarb/data/z'+one_lat+'.dat_print.0416'

printf, 1, '${workdir}run_CER7.2.1P2.pl CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z'+one_lat

close, 1

;spawn, 'qsub  day20130416_'+one_lat+'.csh'

print, '${workdir}run_CER7.2.1P2.pl CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z'+one_lat


;wait, 1

;stop

;endelse
    

;; Wait 5 hours after every 10 jobs, except after the last job
;    IF ((ilat MOD 20) EQ 0)  THEN BEGIN
;        print, 'ilat, ', ilat
;        PRINT, 'Submitted 10 jobs, sleeping 5 hours...'
;        WAIT, 3600 * 3
;    END


stop


 endfor



  stop

  end



PRO replace_lines_starting_with_12, filename, new_line
                                ; Read all lines into a dynamic array
  nnn=numlines(filename)
  lines = STRARR(nnn)
  line=' '
  OPENR, lun, filename, /GET_LUN
  for ii=0, nnn-1 do begin
    READF, lun, line
    trimmed = STRTRIM(line, 2)
    IF STRMID(trimmed, 0, 3) EQ '12|' THEN BEGIN
      lines[ii] = new_line
    END ELSE BEGIN
      lines[ii] = line
    ENDELSE
 endfor
  
  FREE_LUN, lun

  ; Overwrite the same file safely
  OPENW, out_lun, filename, /GET_LUN
  FOR i = 0, N_ELEMENTS(lines)-1 DO BEGIN
    PRINTF, out_lun, lines[i]
  ENDFOR
  FREE_LUN, out_lun

  PRINT, 'Finished replacing and saving.'
END
