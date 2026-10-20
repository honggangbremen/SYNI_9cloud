#!/bin/csh -f
#
# SYNI files
#
  setenv SS7_2 "Aqua-MODIS"
  setenv PS7_2 "Alpha4-Ed5"
  setenv CC7_2 "999999"
#
  setenv SS7_3_4 "Aqua-MODIS"
  setenv PS7_3_4 "Alpha4-Ed5"
  setenv CC7_3_4 "000000"
#
# SAH Terra files
#
  setenv SS5   "Aqua-FM3-MODIS"
  setenv PS5   "Edition5-Alpha4-Sep17"
  setenv CC5   "999999"
#
# SAH Aqua files
#
  setenv SS5_2  "null"
  setenv PS5_2  "null"
  setenv CC5_2  "null"
#
# Ice Age files
#
  setenv SS5_5  "CERES"
  setenv PS5_5  "AFWA-Mesh16th"
  setenv CC5_5  "999999"
#
# MOA files
#
  setenv SS12  "CERES"
  setenv PS12  "GMAO-ERA5-Edition5-Jun25"
  setenv CC12  "999999"
#
# ECS files
#
  setenv SS4_2 "Aqua-MODIS"
  setenv PS4_2 "Beta2-Ed4"
  setenv CC4_2 "402403"
#
# ESNOW and EICE files
#
  setenv SS4_0 "CERES"
  setenv PS4_0_8 "NSIDC-CLASS"
  setenv CC4_0P1_8 "200201" 

  setenv PS4_0_16 "NSIDC-AFWA-Mesh16th"
  setenv CC4_0P1_16 "200200" 
# 
  setenv SAT   "Aqua"
  setenv INST  "FM3"
  setenv IMAG  "MODIS"
#
# MATCH files
#
  setenv SS_MATCH "Terra-Aqua-MODIS"
  setenv PS_MATCH "Edition4"
  setenv CC_MATCH "402402"
#
  setenv EntropyCode "true"
  setenv PristineCode "true"
  setenv TuningCode "true"
#
  setenv MCD_Col "C6"
#
  setenv SW7_2   "1185"
  setenv DATA7_2 "1185" 

# Set Input File Checking to Default
  setenv InputCheck "Enable" 

# Set Input Archives
  setenv InputArchive "/CERES_prd"
  setenv OutputArchive "$CERESHOME/sarb/data"

  setenv PROD "no"
  setenv DATE "201907"

