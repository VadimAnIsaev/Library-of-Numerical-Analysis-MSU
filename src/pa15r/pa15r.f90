! Решение уравнения Диофанта.
!
! Подпрограмма PA15R вычисляет частное решение уравнения
!
!           a x  +  b y   =   c 
!
! в целых числах, где  a,  b и  c - заданные целые числа, причем значения  a и  b  не равны нулю.
!
! Параметры 
!	A, B, C - заданные значения коэффициентов исходного уравнения, A ≠ 0, B ≠ 0 (тип: целый);
!	Q - целый одномерный массив, длина которого должна быть в пять раз больше, чем количество цифр в максимальном по модулю из чисел  a,  b и  c;
!	X, Y - целые переменные, значения которых на выходе полагаются равными вычисленному частному решению исходного уравнения;
!	IERR - целая переменная, значение которой полагается равной признаку выявленной ошибки при счете; при этом:
!		IERR=65 - когда исходное уравнение не определено;
!		IERR=66 - когда решения исходного уравнения в целых числах не существует. 

SUBROUTINE pa15r(a,b,c,q,x,y,ierr)

! Code converted using TO_F90 by Alan Miller
! Date: 2018-07-07  Time: 13:37:14
! Edited Vadim Isaev

INTEGER, INTENT(IN OUT)                  :: a, b, c
INTEGER, DIMENSION(1), INTENT(OUT)       :: q
INTEGER, INTENT(OUT)                     :: x
INTEGER, INTENT(OUT)                     :: y
INTEGER, INTENT(OUT)                     :: ierr

INTEGER :: n,r,s,d,i,u,v,sa,sb,n1

IF(a /= 0 .AND. b /= 0) THEN
  ierr=0
  n=0
  i=0
  d=IABS(a)
  s=d
  r=IABS(b)

  do
    10 i=i+1
    n=i
    q(i)=s/r
    d=r
    r=s-r*q(i)
    s=d
    IF(r == 0) EXIT
  end do
IF(d == 0) THEN
  IF(c /= 0) THEN
    ierr=66
    GO TO 80
  END IF
  ierr=65
  GO TO 80
END IF

30 IF(c/d*d == c) GO TO 40
ierr=66
GO TO 80

40 a=a/d
b=b/d
c=c/d
IF(n /= 0) GO TO 50
v=0
u=1
GO TO 70
50 v=1
u=0
IF(n /= 1) THEN
  do i=n-1, 0, -1
    s=v
    v=u-v*q(i)
    u=s
  end do
END IF

70 sa=1
  sb=1
  IF(a < 0) sa=-1
  IF(b < 0) sb=-1
  x=c*u*sa
  y=c*v*sb
  c=x/b
  x=x-c*b
  y=y+c*a
ELSE
  ierr=65
END IF

END SUBROUTINE pa15r
