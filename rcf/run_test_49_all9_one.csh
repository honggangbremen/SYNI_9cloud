

./cleanup_7.2.1P2.pl CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z001
./CER7.2.1P2_pcfgen.pl -date 201304Z001

cd pcf
cp test.1_33_99 CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z001
cd ..

cp /CERES/sarb/hong/test_data/test_all_9/comb_112233344_CER_TSIB_Terra-Aqua-MODIS_Edition4A_401405.201304Z001 /homedir/ghong/tmp/SARB/tsi/04_09_333/CER_TSIB_Terra-Aqua-MODIS_Edition4A_401405.201304Z001

./run_CER7.2.1P2.pl CER7.2.1P2_PCF_Terra-Aqua-MODIS_Edition4-SSIT_999999.201304Z001 > test.test.112233344 

