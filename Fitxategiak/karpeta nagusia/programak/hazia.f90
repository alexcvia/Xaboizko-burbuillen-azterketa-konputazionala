program hazia 
use tipos
implicit none
integer,allocatable,dimension(:):: seed
integer                         :: n

open(unit=42,action="write",status="replace",file="datuHazia.dat") 

call random_seed()
call random_seed(size=n)
allocate(seed(n))
call random_seed(get=seed)

write(unit=42,fmt=*) seed

close(unit=42)
endprogram hazia
