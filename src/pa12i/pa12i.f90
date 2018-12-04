! 
! п(x) ≤ (x/ln x)*(1 + 1.2762/ln x)
!
! PIERRE DUSART
! THE k^(th) PRIME IS GREATER THAN 
! k(lnk+lnlnk−1) FOR k >=2
!
! MATHEMATICS OF COMPUTATION
! Volume 68, Number 225, January 1999, Pages 411–415
! http://rfpro.ru/d/10601.pdf
! http://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-01037-6/S0025-5718-99-01037-6.pdf
!
program test_pa12i
implicit none

integer, dimension(:), allocatable :: IP
integer N, rn

  N=100
! Размер массива 
  if (N <= 200 ) then
    rn = CEILING(1.6 * N / LOG(real(N)) + 1)
  else
    rn = CEILING(N/(LOG(real(N)) - 2) + 1)
  end if
  allocate(IP(rn))

  CALL PA12I(N,IP,rn)
  PRINT 1,IP
1 FORMAT(10I8)

CONTAINS

!-----------------------------------------------------------------------
! вычисляет последовательность простых чисел 
! (включая единицу), не превосходящих заданного натурального N, 
! используя алглоритм, имеющий название "решето Эратосфена". 
! Полученная последовательность помещается в массиве IP
! Параметры:
! - N - заданное значение натурального числа, 
!       для которого ищутся простые числа, не превосходящие его 
!       (тип: целый); 
!   IP - целый одномерный массив длины [1.6N/lnN + 1], если N ≤ 200, 
!        или длины[N/(lnN - 2) + 1], если N > 200, в котором помещается 
!        вычисленная последовательность простых чисел. 
!   rn - размер массива
!-----------------------------------------------------------------------

SUBROUTINE PA12I(N,IP,rn)
implicit none
      
INTEGER N,rn
INTEGER, DIMENSION(rn) :: IP
integer I,J,K
REAL S

IP(1)=1
IP(2)=2
IP(3)=3
J=3

IF (N > 3) THEN
  DO K=3,N,2
      I=2
      S=SQRT(REAL(K))
    do
      i = i + 1      
      if (IP(I) > S) Then  
        IP(J)=K
        J=J+1
        exit
      else
        IF(K/IP(I)*IP(I) /= K) then
          cycle
        else
          exit
        end if
      end if
    end do
  END DO
END IF

END SUBROUTINE PA12I

end program test_pa12i
