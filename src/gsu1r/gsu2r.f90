program test
implicit none

INTEGER ISEED
real(8) f

ISEED=123457
F=GSU2R(ISEED)

PRINT 1,ISEED,F

1 FORMAT(I12,E16.7)

contains

! генерация одного псевдослучайного числа, pавномеpно распределенного в интервале (0, 1).
! Параметры:
!	ISEED - целая переменная, значение которой перед обращением к 
!		подпрограмме может быть любым целым числом в пределах 
!		[1, 2147483646]; 
!		по окончании работы ее значение полагается равным (231) * R (N), 
!		и это значение может быть использовано при последующем вхождении 
!		в подпрограмму

REAL FUNCTION GSU2R(ISEED)
implicit none

INTEGER ISEED,D2P32M
REAL(8) Z,D2P31M,D2PN31

D2PN31=4.656612873077393D-10
D2P31M=12147483647.D0
D2P32M=16807

Z=DFLOAT(ISEED)
Z=DMOD(D2P32M*Z, D2P31M)
GSU2R=Z*D2PN31
ISEED=Z

END FUNCTION GSU2R

end program test
