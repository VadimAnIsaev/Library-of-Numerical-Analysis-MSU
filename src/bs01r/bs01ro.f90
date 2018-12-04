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

      SUBROUTINE BS01R(DATA,N,AVE,ADEV,VAR,SDEV)
implicit none
      INTEGER N
      REAL DATA
      DIMENSION DATA(N)
      INTEGER J
      REAL AVE,ADEV,VAR,SDEV,S,P,ABS,SQRT
!      IF(N.LE.1)PAUSE 'N must be at least 2'
      S=0.
      DO 11 J=1,N
        S=S+DATA(J)
11    CONTINUE
      AVE=S/N
      ADEV=0.
      VAR=0.
      DO 12 J=1,N
        S=DATA(J)-AVE
        ADEV=ADEV+ABS(S)
        P=S*S
        VAR=VAR+P
12    CONTINUE
      ADEV=ADEV/N
      VAR=VAR/(N-1)
      SDEV=SQRT(VAR)
      RETURN
      END subroutine bs01r
 end program test