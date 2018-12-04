! Обработка ошибок подпрограммы adb1c
SUBROUTINE UTAFSI(NAM1,NAM2,NAM3,NAM4,NAM5,IERR)

INTEGER NAM1,NAM2,NAM3,NAM4,NAM5,IERR

IF (IERR /= 0) THEN

  IF (IERR <= -1) THEN
    PRINT 2,NAM1,NAM2,NAM3,NAM4,NAM5
    
  ENDIF      

  IF (IERR == 65) THEN
    PRINT 4,NAM1,NAM2,NAM3,NAM4,NAM5
    
  ENDIF    
    
  IF (IERR == 66) THEN
    PRINT 6,NAM1,NAM2,NAM3,NAM4,NAM5

  ENDIF
    
  IF (IERR == 67) THEN
     PRINT 8,NAM1,NAM2,NAM3,NAM4,NAM5
    
  ENDIF

ENDIF      

2 FORMAT(' ',&
        'БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA ',&
        5A1,&
        ':HEФATAЛЬHAЯ OШИБKA'/' ',&
        'N 1 - MATPИЦA BЫPOЖДEHA .')
4 FORMAT(' ',&
        'БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA ',&
        5A1,&
        ':ФATAЛЬHAЯ OШИБKA'/' &
        ','N 1 - ЗHAЧEHИE',1X,'N ИЛИ M MEHЬШE ИЛИ PABHO 0 .')
6 FORMAT(' ',&
        'БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA ',&
        5A1,&
        ':ФATAЛЬHAЯ OШИБKA'/' ',&
        'N 2 - ПPOИЗOШЛO',1X,'ПEPEПOЛHEHИE ЦEHTPAЛЬHOГO ПPOЦECCOPA .')
8 FORMAT(' ',&
        'БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA ',&
        5A1,&
        ':HEФATAЛЬHAЯ OШИБKA'/' ',&
        'N 2 - CИCTEMA HECOBMECTHA .')

END SUBROUTINE UTAFSI
