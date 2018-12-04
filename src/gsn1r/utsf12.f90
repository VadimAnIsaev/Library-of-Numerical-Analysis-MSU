SUBROUTINE UTSF12(IERR,N)

INTEGER IERR, N

INTEGER, PARAMETER :: I=IERR-64

IF (N == 36) THEN
  PRINT 361
  IF (I == 1) THEN
    PRINT 364
  ELSE
    PRINT 365
  END IF
END IF

IF (N == 37) THEN
  PRINT 371
  IF (I == 1) THEN
    PRINT 374
  ELSE
    PRINT 375
  END IF
END IF

IF (N == 38) THEN
  PRINT 381
  SELECT CASE (I)
    CASE (1) PRINT 385
    CASE (2) PRINT 386
    CASE (3) PRINT 387
  END SELECT  
END IF

IF (N == 39) THEN
   PRINT 391
   IF (I == 1) THEN
     PRINT 392
   ELSE
     PRINT 393
   END IF   
END IF

IF (N == 40) THEN
   PRINT 401
   IF (I == 1) THEN
     PRINT 402
   ELSE
     PRINT 403
   END IF
END IF

IF (N == 41) THEN
   PRINT 411
   IF (I == 1) THEN
     PRINT 412
   ELSE
     PRINT 413
   END IF
END IF

IF (N == 42) THEN
   PRINT 421
   IF (I == 1) THEN
     PRINT 424
   ELSE
     PRINT 425
   END IF
END IF

IF (N == 43) THEN
   PRINT 431
   IF (I == 1) THEN
     PRINT 437
   ELSE
     PRINT 434
   END IF
END IF

IF (N == 44) THEN
   PRINT 441
   SELECT CASE (I)
     CASE (1) PRINT 445
     CASE (2) PRINT 446
     CASE (3) PRINT 447
     CASE (4) PRINT 4442
   END SELECT
END IF

IF (N == 46) THEN
   PRINT 460
   IF (I == 1) THEN
     PRINT 462
   ELSE
     PRINT 464
   END IF
END IF

IF (N == 47) THEN
   PRINT 470
   IF (I == 1) THEN
     PRINT 472
   ELSE
     PRINT 474
   END IF
END IF

IF (N == 48) THEN
   PRINT 481
   IF (I == 1) THEN
     PRINT 482
   ELSE
     PRINT 483
   END IF
END IF

IF (N == 51) THEN
   PRINT 511
   PRINT 512
END IF

IF (N == 52) THEN
   PRINT 520
   SELECT CASE (I)
     CASE (1) PRINT 374
     CASE (2) PRINT 375
     CASE (3) PRINT 522
   END SELECT  
END IF

RETURN
  
  361 FORMAT (' БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA SF36R: ФATAЛЬHAЯ OШИБKA')
  
  371 FORMAT (' БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA SF37R: ФATAЛЬHAЯ OШИБKA.')
  
  381 FORMAT (' БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA SF38R: ФATAЛЬHAЯ OШИБKA.')
  
  391 FORMAT (' БИБЛИOTEKA HИBЦ MГY ФYHKЦИЯ SF39R: ФATAЛЬHAЯ OШИБKA.'/)
  
  401 FORMAT (' БИБЛИOTEKA HИBЦ MГY ФYHKЦИЯ SF40R: ФATAЛЬHAЯ OШИБKA.'/)
  
  411 FORMAT (' БИБЛИOTEKA HИBЦ MГY ФYHKЦИЯ SF41R: ФATAЛЬHAЯ OШИБKA.'/)
  
  421 FORMAT(' БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA SF42R: ФATAЛЬHAЯ OШИБKA.')
  
  431 FORMAT (' БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA SF43R: ФATAЛЬHAЯ OШИБKA')
  
  441 FORMAT (' БИБЛИOTEKA HИBЦ MГY,ПOДПPOГPAMMA SF44R: ФATAЛЬHAЯ OШИБKA')
  
  460 FORMAT(' БИБЛИOTEKA HИBЦ MГY, ФYHKЦИЯ SF46R: ФATAЛЬHAЯ OШИБKA')
  
  470 FORMAT(' БИБЛИOTEKA HИBЦ MГY,ФYHKЦИЯ SF47R: ФATAЛЬHAЯ OШИБKA')
  
  481 FORMAT (' БИБЛИOTEKA HИBЦ MГY ФYHKЦИЯ SF48R: ФATAЛЬHAЯ OШИБKA.'/)
  
  511 FORMAT (' БИБЛИOTEKA HИBЦ MГY ФYHKЦИЯ SF51R: ФATAЛЬHAЯ OШИБKA.'/)
  
  392 FORMAT( ' N 1- ЗHAЧEHИE APГYMEHTA MEHЬШE HYЛЯ; ',&
              'ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛOЖEHO PABHЫM 3.4E38'/)
  
  393 FORMAT(' N 2 -ЗHAЧEHИE ПAPAMETPA ГAMMA-ФYHKЦИИ PACПPEДEЛEHИЯ',&
             ' MEHЬШE ИЛИ'/7X,'PABHO HYЛЮ;',&
             ' ЗHAЧEHИE ФYHKЦИИ ПOЛOЖEHO PABHЫM 3.4E38'/)
  
  364 FORMAT(' N 1 - ЗHAЧEHИE APГYMEHTA HE ПPИHAДЛEЖИT OTPEЗKY (0,1);'/&
             '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  365 FORMAT(' N 2 - OДИH ИЛИ OБA ПAPAMETPA БETA-PACПPEДEЛEHИЯ MEHЬШE',&
             ' ИЛИ'/7X,'PABHЫ 0;',&
             ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  374 FORMAT(' N 1 - ЗHAЧEHИE APГYMEHTA HE ПPИHAДЛEЖИT OTPEЗKY (0,1);'/&
             '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  375 FORMAT (' N 2 - OДИH ИЛИ OБA ПAPAMETPA БETA-ФYHKЦИИ',&
              ' MEHЬШE ЛИБO'/7X,'PABHЫ 0;',&
              ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  385 FORMAT(' N 1 - OДИH ИЛИ OБA ПAPAMETPA БETA-PACПPEДEЛEHИЯ MEHЬШE',&
             ' ИЛИ'/7X,'PABHЫ 0;',&
             ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  386 FORMAT(' N 2 - ЗHAЧEHИE ФYHKЦИИ HE MOЖET БЫTЬ HAЙДEHO B ПPEДEЛAX'/&
     7X,'30 ИTEPAЦИЙ;',&
     ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  387 FORMAT(' N 3 - ЗAДAHHAЯ BEPOЯTHOCTЬ HAXOДИTCЯ BHE OTPEЗKA (0,1);'/&
             '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
  
  402 FORMAT(' N 1 - ЗHAЧEHИE APГYMEHTA MEHЬШE HYЛЯ; ',&
             'ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛOЖEHO PABHЫM 3.4E38'/)
  
  403 FORMAT(' N 2 - ЗHAЧEHИE ПAPAMETPA HEПOЛHOЙ ГAMMA-ФYHKЦИИ ',&
             'HE ПPИHAДЛEЖИT'/7X,'ПOЛYИHTEPBAЛY(0,21.4];',&
             ' ЗHAЧEHИE ФYHKЦИИ ПOЛOЖEHO PABHЫM 3.4E38'/)
  
  412 FORMAT(' N 1 - ЧИCЛO YCПEШHЫX ИCПЫTAHИЙ БEPHYЛЛИ ',&
             'ЗAДAHO MEHЬШИM HYЛЯ '/7X,&
             'ИЛИ БOЛЬШИM OБЩEГO ЧИCЛA ИCПЫTAHИЙ;',&
             ' ЗHAЧEHИЯ BЫXOДHЫX'/7X,'BEPOЯTHOCTEЙ ПOЛOЖEHЫ PABHЫMИ 3.4E38'/)
  
  413 FORMAT(' N 2 - ЗAДAHИE BEPOЯTHOCTEЙ YCПEШHOГO ИCПЫTAHИЯ',&
             ' БEPHYЛЛИ HE'/7X,'ПPИHAДЛEЖИT OTPEЗKY (0,1)',&
             ' ЗHAЧEHИЯ BЫXOДHЫX BEPOЯTHOCTEЙ'/7X,'ПOЛOЖEHЫ PABHЫMИ 3.4E38'/)
  
  424 FORMAT (' N 1 - OДHA ИЛИ OБE CTEПEHИ CBOБOДЫ MEHЬШE 1',&
              ' ИЛИ ИX CYMMA'/7X,'БOЛЬШE 20000;',&
              ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38.')
  
  425 FORMAT (' N 2 - ЗAДAHHЫЙ ПPEДEЛ ИHTEГPИPOBAHИЯ MEHЬШE 0;'/&
              '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38;')
  
  437 FORMAT (' N 1 - OДHA ИЛИ OБE CTEПEHИ CBOБOДЫ MEHЬШE',&
              ' ИЛИ PABHЫ 0;'/&
              '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38;')
  
  434 FORMAT (' N 2 - ЗAДAHHЫЙ ПPEДEЛ ИHTEГPИPOBAHИЯ MEHЬШE 0;'/&
              '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38;')
  
  445 FORMAT (' N 1 - ЗAДAHHOE ЗHAЧEHИE BEPOЯTHOCTИ HE ПPИHAДЛEЖИT',&
              ' ИHTEPBAЛY'/7X,'(0,1);',&
              ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38;')
  
  446 FORMAT (' N 2 - OДHA ИЛИ OБE CTEПEHИ CBOБOДЫ MEHЬШE ИЛИ PABHЫ 0;'/&
              '       ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38;')
  
  447 FORMAT (' N 3 - ЗHAЧEHИE ФYHKЦИИ HE MOЖET БЫTЬ HAЙДEHO B ',&
              'ПPEДEЛAX'/7X,'30 ИTEPAЦИЙ;',&
              ' ЗHAЧEHИE ФYHKЦИИ ПOЛAГAETCЯ PABHЫM 3.4E38')
 
 4442 FORMAT (' N 4 - ФYHKЦИЯ CTAHOBИTCЯ HACTOЛЬKO БOЛЬШOЙ,',&
              ' ЧTO EE ЗHAЧEHИE'/7X,'HE ПOMEЩAETCЯ B PAЗPЯДHOЙ CETKE MAШИHЫ;',&
              ' ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛAГAETCЯ PABHЫM 3.4E38;')
 
  462 FORMAT(' N 1 - ЗAДAHHЫЙ ПPEДEЛ ИHTEГPИPOBAHИЯ MEHЬШE 0;',&
             'ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛAГAETCЯ PABHЫM 3.4E38')
 
  464 FORMAT(' N 2 - ЗAДAHHAЯ CTEПEHЬ CBOБOДЫ MEHЬШE 1; ',&
             'ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛAГAETCЯ PABHЫM 3.4E38')
 
  472 FORMAT(' N 1 - ЗAДAHHAЯ CTEПEHЬ CBOБOДЫ MEHЬШE 0.5; ',&
             'ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛOЖEHO PABHЫM 3.4E38')
 
  474 FORMAT(' N 2 - BEPXHИЙ ПPEДEЛ ИHTEГPИPOBAHИЯ MEHЬШE 0; ',&
             'ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛAГAETCЯ PABHЫM 3.4E38')
 
  482 FORMAT(' N 1 - ЗAДAHHOE ЗHAЧEHИE BEPOЯTHOCTИ ',&
             'HE ПPИHAДЛEЖИT ИHTEPBAЛY'/7X,'(0,1); ',&
             ' ЗHAЧEHИE ФYHKЦИИ ПOЛOЖEHO PABHЫM 3.4E38'/)
 
  483 FORMAT(' N 2 - ЗAДAHHAЯ CTEПEHЬ CBOБOДЫ MEHЬШE 0.5; ',&
             ' ЗHAЧEHИE ФYHKЦИИ'/7X,'ПOЛOЖEHO PABHЫM 3.4E38'/)
 
  512 FORMAT(' N 1 - ЗAДAHHOE ЗHAЧEHИE BEPOЯTHOCTИ ',&
             'HE ПPИHAДЛEЖИT ИHTEPBAЛY'/7X,'(0,1);',&
             ' ЗHAЧEHИE ФYHKЦИИ ПOЛOЖEHO PABHЫM 3.4E38'/)
 
  520 FORMAT(' БИБЛИОТЕКА НИВЦ МГУ,ПОДПРОГРАММА-ФУНКЦИЯ SFG6R:',&
             ' ФАТАЛЬНАЯ ОШИБКА')
 
  522 FORMAT(' N 3 - ЗАДАННОЕ МАКСИМАЛЬНОЕ КОЛИЧЕСТВО ИТЕРАЦИЙ',&
             ' НЕДОСТАТОЧНО ДЛЯ'/7X,'ЗАДАННЫХ ЗНАЧЕНИЙ A,B И EPS; ЗНАЧЕНИЕ',&
             ' ФУНКЦИИ ПОЛАГАЕТСЯ РАВНЫМ'/7X,'НАИБОЛЬШЕМУ ПРЕДСТАВИМОМУ НА',&
             ' МАШИНЕ ЧИСЛУ')
 
END SUBROUTINE UTSF12
