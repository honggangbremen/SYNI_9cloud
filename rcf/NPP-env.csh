#!/bin/csh -f
#
# SYNI files
#
  setenv SS7_2 "NPP-FM5-VIIRS"
  setenv PS7_2 "Edition4-SSIT"
  setenv CC7_2 "999999"
#
# TSIB files
#
  setenv SS7_3_4 "NPP-VIIRS"
  setenv PS7_3_4 "Edition1A"
  setenv CC7_3_4 "999999"
#
# SAH Terra files
#
  setenv SS5   "NPP-FM5-VIIRS"
  setenv PS5   "Edition1A"
  setenv CC5   "100101"
#
# SAH Aqua files
#
  setenv SS5_2  "Null"
  setenv PS5_2  "Null"
  setenv CC5_2  "Null"
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
  setenv PS12  "GMAO-G541-Ed4"
  setenv CC12  "400400"
#
# ECS files
#
  setenv SS4_2 "NPP-VIIRS"
  setenv PS4_2 "Edition1A"
  setenv CC4_2 "100101"
#
# ESNOW and EICE files
#
  setenv SS4_0 "CERES"
  setenv PS4_0_8 "NSIDC-CLASS"
  setenv CC4_0P1_8 "200200" 

  setenv PS4_0_16 "NSIDC-AFWA-Mesh16th"
  setenv CC4_0P1_16 "200200" 
# 
  setenv SAT   "NPP"
  setenv INST  "FM5"
  setenv IMAG  "VIIRS"
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
  setenv InputArchive "/ASDC_archive"
  setenv OutputArchive "$CERESHOME/sarb/data"

  setenv PROD "no"
  setenv DATE "201304"

