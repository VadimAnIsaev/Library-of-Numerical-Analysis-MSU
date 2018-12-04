program test
implicit none

REAL, DIMENSION(3) :: R=0.
INTEGER :: ISEED=123457, N=3

CALL GSU1R(ISEED,N,R)

PRINT 1,ISEED
PRINT 2,R

1 FORMAT(1X,'ISEED = '/1X,I25)
2 FORMAT(1X,'R = ',1X,4F18.14)

contains

! Генерация массива псевдослучайных чисел, 
! pавномеpно распределенных в интервале (0, 1). 
!
! Параметры:
!	ISEED - целая переменная, значение которой перед обращением к 
!		подпрограмме может быть любым целым числом в пределах 
!		[1, 2147483646]; 
!		по окончании работы ее значение полагается равным (231) * R (N), 
!		и это значение может быть использовано при последующем вхождении 
!		в подпрограмму;
!	N - аданное количество генерируемых псевдослучайных чисел (тип: целый);
!	R - вещественный массив длины N, содержащий вычисленные псевдослучайные 
!		числа. 

SUBROUTINE GSU1R(ISEED,N,R)
implicit none

INTEGER ISEED,N
REAL, DIMENSION(N) :: R

INTEGER I, D2P32M
REAL(8) Z,D2P31M,D2PN31

D2PN31=4.656612873077393D-10
D2P31M=2147483647.D0
D2P32M=16807

Z=REAL(ISEED, 8)

DO I=1,N
  Z=MOD(D2P32M*Z, D2P31M)
  R(I)=Z*D2PN31
END DO

ISEED=Z

END SUBROUTINE GSU1R

end program test
