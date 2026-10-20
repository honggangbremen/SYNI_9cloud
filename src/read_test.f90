PROGRAM read_test
  USE ed4_syni_structure, ONLY: SYNI4
  INTEGER :: ios
  OPEN(10, FILE='testbin.dat', ACCESS='DIRECT', FORM='UNFORMATTED', RECL=580)
  READ(10, REC=100, IOSTAT=ios) SYNI4
  PRINT *, "Read IOS=", ios, "Hour=", SYNI4%misc%hr_month
  CLOSE(10)
END PROGRAM
