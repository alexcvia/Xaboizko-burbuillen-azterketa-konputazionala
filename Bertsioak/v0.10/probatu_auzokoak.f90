program probatu_auzokoak
use tipos
use parametroak
use auzokoak

real(kind=dp)                           ::x,y,z,kte
integer                                 ::i,j,k,n,kontadore,i_partikula,n_auzo
real(kind=dp),dimension(:,:),allocatable::f
integer,dimension(:,:),allocatable      ::f_sare,f_sare_bilduma
integer,dimension(:),allocatable        ::auzokoen_indizeak

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

!  artikuluko olatu funtzioa eraiki
!  allocate(f(n*n,3))
!  kontadore=1
!  do i=1,n
!    x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
!    do j=1,n
!      y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
!      z=0.1*(3*sin(x)+2*cos(y)+4*sin(2*x+y))
!      f(kontadore,1:3)=(/x,y,z/)
!      write(unit=42,fmt=*) f(kontadore,1:3)
!      kontadore=kontadore+1
!    enddo
!  enddo

!do i=1,n
!    write(unit=42,fmt=*) "aaaaaaaaaaaaaaaaah"
!enddo

!   bigaren zatian idatzi i_ren auzokoak
  i_partikula=2500
  call zati_sarea_sortu(f,f_sare,f_sare_bilduma)
  call i_ren_auzokoak(f,f_sare,i_partikula,auzokoen_indizeak)
!do i=1,n*n
!  write(unit=42,fmt=*) f_sare(i,1:3)
!enddo
  write(unit=42,fmt=*) 
  write(unit=42,fmt=*)
  n_auzo=size(auzokoen_indizeak,1)
  do i=1,n_auzo
!    write(unit=42,fmt=*) auzokoen_indizeak(i)
    write(unit=42,fmt=*) f(auzokoen_indizeak(i),1:3)
  enddo

!write(unit=42,fmt=*) 
!write(unit=42,fmt=*)
!do i=1,size(f_sare_bilduma,1)
!  write(unit=42,fmt=*) f_sare_bilduma(i,1:3)
!enddo
!write(unit=42,fmt=*) 
!write(unit=42,fmt=*)
!do i=1,n*n
!  write(unit=42,fmt=*) f_sare(i,1:3)
!enddo
!-------
close(unit=42)
deallocate(f)
deallocate(f_sare)
deallocate(f_sare_bilduma)
deallocate(auzokoen_indizeak)

endprogram probatu_auzokoak
