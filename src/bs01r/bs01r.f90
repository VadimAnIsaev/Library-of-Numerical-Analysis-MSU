program test
implicit none
REAL X(4),XM,XD,S,D
integer N 
      DATA X/1.,2.,3.,4./
      N=4
      CALL BS01R(X,N,XM,XD,D,S)
      PRINT 1,XM,XD,D,S
    1 FORMAT(' TBS01R: XM,XD,D,S =',/ 4F16.8)

contains


!-------------------------------------------------------------------------------------------
! Вычисление: 
!   - среднего, 
!   - среднего отклонения, 
!   - среднеквадратического отклонения, 
!   - дисперсии заданной выборки.
!
! Параметры:
!   vector - заданный вещественный выборочный вектор длины N;
!   N      - длина вектора vector (тип: целый);
!   XM     - вещественная переменная, содержащая выборочное среднее;
!   XD     - вещественная переменная, содержащая выборочное среднее отклонение;
!   D      - вещественная переменная, содержащая выборочную дисперсию;
!   S      - вещественная переменная, содержащая выборочное среднеквадратическое отклонение. 
!-------------------------------------------------------------------------------------------

SUBROUTINE BS01R(vector,N,AVE,ADEV,VAR,SDEV)
implicit none

INTEGER N
REAL, DIMENSION(N) :: vector(N)
REAL AVE,ADEV,VAR,SDEV
INTEGER J
REAL S,P

!IF(N <= 1) PAUSE 'N must be at least 2'
S=0.

DO J=1,N
  S=S+vector(J)
END DO

AVE=S/N
ADEV=0.
VAR=0.

DO J=1,N
  S=vector(J)-AVE
  ADEV=ADEV+ABS(S)
  P=S*S
  VAR=VAR+P
END DO

ADEV=ADEV/N
VAR=VAR/(N-1)
SDEV=SQRT(VAR)
      
END SUBROUTINE BS01R

end program test