      INTEGER FUNCTION PA11I(M,N)
      INTEGER N,M,R1,R2,R
      IF(M.NE.0.AND.N.NE.0) GO TO 10
      PA11I=IABS(M+N)
      GO TO 40
   10 R2=M
      R1=N
   20 R=R2-R1*(R2/R1)
      IF(R.EQ.0) GO TO 30
      R2=R1
      R1=R
      GO TO 20
   30 PA11I=IABS(R1)
   40 RETURN
      END
