program probatu_auzokoak
use tipos
use parametroak
use auzokoak

real(kind=dp)                           ::x,y,z,kte
integer                                 ::i,j,k,n,kontadore,i_partikula,n_auzo
real(kind=dp),dimension(:,:),allocatable::f

open(unit=42,action="write",status="replace",file="datuauzokoak.dat")
n=100

!   z=kte planoa eraiki
  allocate(f(n,n,3))
  kte=5.0_dp
  kontadore=1
  do i=1,n
    x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
    do j=1,n
      y=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
      f(kontadore,1:3)=(/x,y,kte/)
      write(unit=42,fmt=*) x,y,kte
      kontadore=kontadore+1
    enddo
  enddo

!   bigaren zatian idatzi i_ren auzokoak
  i_partikula=2500
  call i_ren_auzokoak(f,i_partikula,auzokoen_indizeak)
  write(unit=42,fmt=*)
  write(unit=42,fmt=*)
  n_auzo=size(auzokoak)
  do i=1,n_auzo
    write(unit=42,fmt=*) f(auzokoen_indizeak(i),1:3)
  enddo
!-------
close(unit=42)
deallocate(f)

endprogram probatu_auzokoak
