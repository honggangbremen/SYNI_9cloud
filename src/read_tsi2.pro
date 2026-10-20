PRO read_tsi2, filename, rec_num
  COMPILE_OPT IDL2

  ;--------------------------
  ; Constants
  reclen = 588L       ; bytes per TSI record
  record_index = rec_num - 1
  offset_bytes = record_index * reclen

  ;--------------------------
  ; Open file in stream mode
  OPENR, lun, filename, /GET_LUN, /STREAM, /BINARY

  ;--------------------------
  ; Move to correct record position
  POINT_LUN, lun, offset_bytes

  ;--------------------------
  ; Read the record as byte array
  buffer = BYTARR(reclen)
  READU, lun, buffer

  ; Done with file
  FREE_LUN, lun

  ;--------------------------
  ; Decode the record manually
  off = 0

  ; Region number (int32)
  region_num = FIX(LONG(buffer[off:off+3], 0, /LITTLE_ENDIAN))
  off += 4

  hour_box = FIX(LONG(buffer[off:off+3], 0, /LITTLE_ENDIAN))
  off += 4

  geo_sat_id = FIX(LONG(buffer[off:off+3], 0, /LITTLE_ENDIAN))
  off += 4

  ; Julian hour
  julhr = FLOAT(buffer[off:off+3], 0, /IEEE, /LITTLE_ENDIAN)
  off += 4

  ; Print basic info
  PRINT, 'Record #: ', rec_num
  PRINT, 'Region #: ', region_num
  PRINT, 'Hour box: ', hour_box
  PRINT, 'Julian hour: ', julhr

  ; (Optional) Continue decoding more fields...

END
