program test_bit_size 

integer :: i = 1 
integer :: size 
character(25) :: c

size = bit_size(i) 

print *, size 

write (c, '(I25.1)') i

print *, TRIM(c)
print *, LEN_TRIM(c)

end program test_bit_size
