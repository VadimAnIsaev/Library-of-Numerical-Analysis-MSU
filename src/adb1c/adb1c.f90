!**********************************************************************
!  BЫЧИCЛEHИE OПPEДEЛИTEЛЯ METOДOM ГAYCCA C BЫБOPOM BEДYЩEГO ЭЛEMEH-  *
!  TA ПO CTOЛБЦY И OЦEHKA ЧИCЛA OБYCЛOBЛEHHOCTИ ЛEHTOЧHOЙ MATPИЦЫ A.  *
!**********************************************************************
SUBROUTINE ADB1C(A,MA,N,ML,MU,NLEAD,DET1,DET2,RCOND,Z,IERR)

INTEGER IERR,MA,ML,MU,N,NLEAD
COMPLEX A, Z 
DIMENSION A(MA,1), NLEAD(1), Z(1)
REAL DET2 
REAL RCOND 
REAL X
COMPLEX DET1 
EXTERNAL AFB2C, UTAFSI
INTEGER I,M

DET1 = (0.0,0.0)
DET2 = 0.0
DET1 = (0.0,0.0)

!
!  YCTAHOBЛEHИE KOHTPOЛЯ HA BOЗMOЖHOCTЬ ПEPEПOЛHEHИЯ
X = 0.0

!
!  ПPOBEPKA ПPABИЛЬHOCTИ ЗAДAHИЯ ПAPAMETPOB M И N
IF (MA < 1 .AND. N < 1) THEN
  IERR = 65
  CALL UTAFSI(1HA,1HD,1HB,1H1,1HC,IERR)
  GO TO 90
END IF   
       
IERR = 0
CALL AFB2C(A,MA,N,ML,MU,NLEAD,RCOND,Z,IERR)
IF (IERR /= 0) THEN
  CALL UTAFSI(1HA,1HD,1HB,1H1,1HC,IERR)
  GO TO 90
END IF

M = ML + 1
DET1 = (1.0,0.0)
DET2 = 0.0

DO I = 1, N
  IF (NLEAD(I) /= I) THEN
    DET1 = -DET1
  END IF
  DET1 = A(I,M)*DET1
   
  DO
    IF (CABS(DET1) < 1.0) THEN
      DET1 = (10.0,0.0)*DET1
      DET2 = DET2 - 1.0
    ELSE
      exit
    END IF 
  END DO

  DO  
    IF (CABS(DET1) >= 10.0) THEN
      DET1 = DET1/(10.0,0.0)
      DET2 = DET2 + 1.0
    ELSE
      exit
    END IF
  END DO
   
END DO
90  CONTINUE

END SUBROUTINE ADB1C
