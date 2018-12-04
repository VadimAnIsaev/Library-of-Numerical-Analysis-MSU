! Вычисление общего наибольшего делителя двух целых чисел.
! Используется алгоритм Евклида

INTEGER FUNCTION PA11I(M,N)

implicit none
! M, N - заданные значения целых чисел, 
! для которых ищется общий наибольший делитель
INTEGER N,M
integer R1,R2,R

IF (M /= 0 .AND. N /= 0) THEN
  R2 = M
  R1 = N
  DO
    R = R2-R1*(R2/R1)
    IF (R == 0) THEN
      EXIT
    END IF
    R2 = R1
    R1 = R
  END DO
  PA11I = IABS(R1)
ELSE
  PA11I = IABS(M+N)
END IF   

END FUNCTION PA11I
