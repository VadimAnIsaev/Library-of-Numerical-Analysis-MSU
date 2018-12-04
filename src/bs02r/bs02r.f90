!---------------------------------------------------------------------------------
! Вычисление коэффициентов асимметрии и эксцесса заданной выборки. 
!
! Параметры:
!   vector - заданный вещественный выборочный вектор длины N;
!   N - длина вектора vector (тип: целый);
!   SKEW - вещественная переменная, содержащая выборочный коэффициент асимметрии;
!   CURT - вещественная переменная, содержащая выборочный коэффициент эксцесса. 
!---------------------------------------------------------------------------------

SUBROUTINE BS02R(vector,N,SKEW,CURT)
IMPLICIT NONE

INTEGER N
REAL, DIMENSION(N) :: vector
REAL SKEW,CURT

INTEGER J
REAL S,AVE,ADEV,VAR,P,SDEV

!      IF(N.LE.1)PAUSE 'N must be at least 2'
S=0.

DO J=1,N
  S=S+vector(J)
END DO

AVE=S/N
ADEV=0.
VAR=0.
SKEW=0.
CURT=0.

DO J=1,N
  S=vector(J)-AVE
  ADEV=ADEV+ABS(S)
  P=S*S
  VAR=VAR+P
  P=P*S
  SKEW=SKEW+P
  P=P*S
  CURT=CURT+P
END DO

ADEV=ADEV/N
VAR=VAR/(N-1)
SDEV=SQRT(VAR)

IF (VAR /= 0.) THEN
  SKEW=SKEW/(N*SDEV**3)
  CURT=CURT/(N*VAR**2)-3.
END IF

END SUBROUTINE BS02R
