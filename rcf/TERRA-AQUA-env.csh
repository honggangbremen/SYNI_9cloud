#!/bin/csh -f
#
# SYNI files
#
  setenv SS7_2 "Terra-Aqua-MODIS"
  setenv PS7_2 "Edition4-SSIT"
  setenv CC7_2 "999999"
#
# TSIB files
#
  setenv SS7_3_4 "Terra-Aqua-MODIS"
  setenv PS7_3_4 "Edition4A"
  setenv CC7_3_4 "401405"
#
# SAH Terra files
#
  setenv SS5   "Terra-FM1-MODIS"
  setenv PS5   "Edition4A"
  setenv CC5   "405406"
#
# SAH Aqua files
#
  setenv SS5_2  "Aqua-FM3-MODIS"
  setenv PS5_2  "Edition4A"
  setenv CC5_2  "405406"
#
# Ice Age files
#
  setenv SS5_5  "CERES"
  setenv PS5_5  "AFWA-Mesh16"
  setenv CC5_5  "400400"
#
# MOA files
#
  setenv SS12  "CERES"
  setenv PS12  "GMAO-G541-Ed4"
  setenv CC12  "400400"
#
# ECS files
#
  setenv SS4_2 "Terra-MODIS"
  setenv PS4_2 "Beta2-Ed4"
  setenv CC4_2 "400401"
#
# ESNOW and EICE files
#
  setenv SS4_0 "CERES"
  setenv PS4_0_8 "NSIDC-CLASS"
  setenv CC4_0P1_8 "200200" 

  setenv PS4_0_16 "NSIDC-AFWA-Mesh16th"
  setenv CC4_0P1_16 "200200" 
# 
  setenv SAT   "Terra-Aqua"
  setenv INST  "FM1"
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
  setenv DATE "202108"

