program probatu_auzokoak
use tipos
use parametroak
use auzokoak

real(kind=dp)                           ::x,y,z,kte
integer                                 ::i,j,k,n,kontadore,i_partikula,n_auzo
real(kind=dp),dimension(:,:),allocatable::f
integer,dimension(:,:),allocatable      ::f_sare
integer,dimension(:),allocatable        ::auzokoen_indizeak
integer,dimension(1,3)                    ::psi


open(unit=42,action="write",status="replace",file="datuauzokoak.dat")
n=100

!   z=kte planoa eraiki
  allocate(f(n*n,3))
  kte=5.0_dp
  kontadore=1
  do i=1,n
    x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
    do j=1,n
      y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
      f(kontadore,1:3)=(/x,y,kte/)
      write(unit=42,fmt=*) f(kontadore,1:3)
      kontadore=kontadore+1
    enddo
  enddo

do i=1,n
    write(unit=42,fmt=*) "aaaaaaaaaaaaaaaaah"
enddo

!   bigaren zatian idatzi i_ren auzokoak
  allocate(f_sare(n*n,3))
  i_partikula=2500
  call zati_sarea_sortu(f,f_sare)
  call i_ren_auzokoak(f,f_sare,i_partikula,auzokoen_indizeak,psi)
do i=1,n*n
  write(unit=42,fmt=*) f_sare(i,1:3)
enddo
  write(unit=42,fmt=*) psi
  write(unit=42,fmt=*)
  n_auzo=size(auzokoen_indizeak,1)
  do i=1,n_auzo
    write(unit=42,fmt=*) f(auzokoen_indizeak(i),1:3)
  enddo
!-------
close(unit=42)
deallocate(f)
deallocate(f_sare)
deallocate(auzokoen_indizeak)

endprogram probatu_auzokoak
