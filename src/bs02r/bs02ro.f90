      SUBROUTINE BS02R(DATA,N,SKEW,CURT)
      DIMENSION DATA(N)
      INTEGER N,J
      REAL DATA,SKEW,CURT,S,AVE,ADEV,VAR,P,SDEV,ABS,SQRT
C      IF(N.LE.1)PAUSE 'N must be at least 2'
      S=0.
      DO 11 J=1,N
        S=S+DATA(J)
11    CONTINUE
      AVE=S/N
      ADEV=0.
      VAR=0.
      SKEW=0.
      CURT=0.
      DO 12 J=1,N
        S=DATA(J)-AVE
        ADEV=ADEV+ABS(S)
        P=S*S
        VAR=VAR+P
        P=P*S
        SKEW=SKEW+P
        P=P*S
        CURT=CURT+P
12    CONTINUE
      ADEV=ADEV/N
      VAR=VAR/(N-1)
      SDEV=SQRT(VAR)
      IF(VAR.EQ.0.)RETURN
        SKEW=SKEW/(N*SDEV**3)
        CURT=CURT/(N*VAR**2)-3.
      RETURN
      END
