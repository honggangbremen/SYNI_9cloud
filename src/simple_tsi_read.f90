program simple_tsi_read
  use simple_tsi_mod
  implicit none

  type(TSI_TYPE) :: tsi_record
  integer :: unitnum, ios, tsi_reclen
  character(len=256) :: filename

  ! Set the long file path via variable
  filename = "/CERES_prd/CERES/TSIB/Terra-Aqua-MODIS_Edition4A/2013/04/CER_TSIB_Terra-Aqua-MODIS_Edition4A_401405.201304Z178"

  ! Get correct record length for TSI_TYPE
  call get_tsi_reclen(tsi_reclen)

  print *, 'tsi_reclen = ', tsi_reclen
  
  ! Open the file for reading
  open(unit=10, file=filename, form="unformatted", access="direct", &
       recl=tsi_reclen, status="old", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening file!"
    stop
  end if
  unitnum = 10

  ! Read the first record
  read(unitnum, rec=1, iostat=ios) tsi_record
  if (ios /= 0) then
    print *, "Error reading record!"
    close(unitnum)
    stop
  end if

  ! Print the record
  call TSI_print(tsi_record)

  ! Close the file
  close(unitnum)
end program simple_tsi_read
