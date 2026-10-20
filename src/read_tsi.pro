; '/CERES_prd/CERES/TSIB/Terra-Aqua-MODIS_Edition4A/2013/04/CER_TSIB_Terra-Aqua-MODIS_Edition4A_401405.201304Z178'
; IDL procedure to read a TSI_TYPE record from a binary file
pro read_tsi, filename, recnum, tsi

  ; Define sizes (from your Fortran module)
  num_surftyp = 20
  cld_layrs = 4
  num_overlap = 11

  ; Compute the total number of elements in a record
  ; Field-by-field, matching Fortran order

  ; Time_and_location_data
  n_time_and_location = 3*1 + 1*1 + 1*1 + 1*1 + num_surftyp + 1*1 + 1*1 ; 3 ints, 7 floats
  ; Actually, from Fortran: region_num, hour_box_num, geo_sat_id (int4), julhr, sfc_alt, cos_sol_zenith, surf_perc_cov(20), view_angle, imager_view_angle (all real4)
  n_time_and_location = 3 + 1 + 1 + 1 + num_surftyp + 1 + 1 ; 3 ints, 23 floats

  ; clr_sky_area_data: 4 floats
  n_clr_sky_area_data = 4

  ; Fluxes_tsi: 1 int, 8 floats
  n_fluxes_tsi = 1 + 8

  ; Cld_cat: 18 floats per layer, 4 layers
  n_cld_cat = 18 * cld_layrs

  ; Overlap_data: 11 floats
  n_overlap_data = num_overlap

  ; Surface_data: 1 int, 4 floats
  n_surface_data = 1 + 4

  ; aerosol_data: 1 float, 7 floats, 5 floats (total 1+7+5=13 floats)
  n_aerosol_data = 1 + 7 + 5 ; but from your code, it's 1 + 7 + 5 = 13, but your type has 12 floats, so use 12
  n_aerosol_data = 12

  ; Total record size (number of elements)
  n_ints = 3 + 1 + 1 + 1       ; region_num, hour_box_num, geo_sat_id, skintemp_flg, data_flg
  n_floats = 23 + 4 + 8 + (18*4) + 11 + 4 + 12 ; see above

  ; But to avoid confusion, let's make a list of how many and in which order
  ; From your Fortran TSI_TYPE order:
  ; Time_and_location_data: int,int,int,real,real,real,real[20],real,real
  ; clr_sky_area_data: real, real, real, real
  ; cld_layer(4): each is 18 reals
  ; fluxestsi: int, 8 reals
  ; overlap: real[11]
  ; surface: int, 4 reals
  ; aerosol: 12 reals

  ; So number and order of types:
  n_ints = 3 + 1 + 1     ; region_num, hour_box_num, geo_sat_id, data_flg, skintemp_flg
  n_floats = 1 + 1 + 1 + num_surftyp + 1 + 1 + 4 + (18*cld_layrs) + 8 + 11 + 4 + 12
  ; These are in order as in the Fortran structure

  openr, unit, filename, error=error, /get_lun, /swap_endian

  ; Fortran typically writes records as contiguous blocks, so we can read them as a flat array
  ; Each int4 is 4 bytes, each real4 is 4 bytes
  rec_size = 588 ;n_ints*4 + n_floats*4

  ; Set file pointer to the record
  point_lun, unit, (recnum-1)*rec_size

  ; Read all data as int4 and real4 in the correct order
  data_int = lonarr(n_ints, /nozero)
  data_float = fltarr(n_floats, /nozero)

  ; Read integers
  readu, unit, data_int

  ; Read floats
  readu, unit, data_float

  free_lun, unit

  ; Parse the data into a structure for convenience
  tsi = { $
    region_num: data_int[0], $
    hour_box_num: data_int[1], $
    geo_sat_id: data_int[2], $
    julhr: data_float[0], $
    sfc_alt: data_float[1], $
    cos_sol_zenith: data_float[2], $
    surf_perc_cov: data_float[3:22], $
    view_angle: data_float[23], $
    imager_view_angle: data_float[24], $

    aero_vis_opdep063: data_float[25], $
    aero_vis_opdep160: data_float[26], $
    ir_radiance: data_float[27], $
    imager_rad4: data_float[28], $

    cld_layer: reform(data_float[29:29+18*cld_layrs-1], 18, cld_layrs), $

    data_flg: data_int[3], $
    total_sw_mn: data_float[29+18*cld_layrs], $
    total_lw_mn: data_float[30+18*cld_layrs], $
    total_alb_mn: data_float[31+18*cld_layrs], $
    total_wn_mn: data_float[32+18*cld_layrs], $
    clear_sw_mn: data_float[33+18*cld_layrs], $
    clear_lw_mn: data_float[34+18*cld_layrs], $
    clear_alb_mn: data_float[35+18*cld_layrs], $
    clear_wn_mn: data_float[36+18*cld_layrs], $

    areafrac_ov: data_float[37+18*cld_layrs:37+18*cld_layrs+num_overlap-1], $

    skintemp_flg: data_int[4], $
    lw_surf_emiss: data_float[37+18*cld_layrs+num_overlap], $
    wn_surf_emiss: data_float[38+18*cld_layrs+num_overlap], $
    sfc_skin_temp: data_float[39+18*cld_layrs+num_overlap], $
    moa_skin_temp: data_float[40+18*cld_layrs+num_overlap], $

    aerosol_data: data_float[41+18*cld_layrs+num_overlap:41+18*cld_layrs+num_overlap+11] $
  }

  print, tsi

end
