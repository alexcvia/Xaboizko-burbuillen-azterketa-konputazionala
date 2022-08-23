program probatu_funtzioak

use tipos
use parametroak
use funtzioak
implicit none

!interface  !--->funtzioak funtzio matematiko bihurtu
!  function h_kernel(r,h)
!  real(kind=dp),intent(in)::r,h
!  real(kind=dp)::h_kernel
!  endfunction h_kernel
!
!  function o_kernel(r,h)
!  real(kind=dp),intent(in)::r,h
!  real(kind=dp)::o_kernel
!  endfunction o_kernel
!endinterface

real(kind=dp),dimension(:,:),allocatable::ptuMultzoa
!real(kind=dp),dimension(:),allocatable  ::x_gorde
integer::nptu,i,j,k
real(kind=dp)::R,x,y1,y2,y3,y4,z,modulu

!----parametroak----
R=1.0_dp
nptu=1000

!---prestaketa---
allocate(ptuMultzoa(nptu,2))
open(unit=42,status="replace",action="write",file="datufuntzio.dat")

!---programa----


do i=1,nptu

  x= 0.0_dp + (R -0.0_dp)*(i-1.0_dp)/(nptu-1.0_dp)
  
  y1= o_kernel(x,R_auzo)
  y2= h_kernel(x,R_auzo)
  y3= do_kernel(x,R_auzo)
  y4= dh_kernel(x,R_auzo)

  write(unit=42,fmt=*), x,y1
  write(unit=42,fmt=*), x,y2
  write(unit=42,fmt=*), x,y3
  write(unit=42,fmt=*), x,y4
  
!  if (modulu<R) then
!    write(unit=42,fmt=*), x,0.0,z
!    j=j+1
!  else
!    write(unit=42,fmt=*), "nope"
!  endif
enddo



!----itxi----
deallocate(ptumultzoa)
close(unit=42)


endprogram probatu_funtzioak
