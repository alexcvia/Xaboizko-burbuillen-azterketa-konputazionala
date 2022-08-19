program probatu_auzokoak
use tipos
use parametroak
use auzokoak

real(kind=dp)                           ::x,y,z,kte
integer                                 ::i,j,k,l,n,kontadore,i_partikula
integer                                 ::n_auzo,x_auzo,y_auzo,z_auzo,errore
real(kind=dp),dimension(:,:),allocatable::f
integer,dimension(:,:,:,:),allocatable  ::f_sare
integer,dimension(:),allocatable        ::auzokoen_indizeak

open(unit=42,action="write",status="replace",file="datuauzokoak.dat")
n=100

!-----z=kte planoa eraiki------
  allocate(f(n*n,3))
  kte=5.0_dp
  kontadore=1
  do i=1,n
    x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
    do j=1,n
      y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
      f(kontadore,1:3)=(/x,y,kte/)
!      write(unit=42,fmt=*) f(kontadore,1:3)
      kontadore=kontadore+1
    enddo
  enddo

!-----artikuluko olatu funtzioa eraiki-----
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

!----------

!do i=1,n
!    write(unit=42,fmt=*) "aaaaaaaaaaaaaaaaah"
!enddo

!   bigaren zatian idatzi i_ren auzokoak
  i_partikula=2500
  call zati_sarea_sortu(f,f_sare)
!  call i_ren_auzokoak(f,f_sare,i_partikula,auzokoen_indizeak)
!do i=1,n*n
!  write(unit=42,fmt=*) f_sare(i,1:3)
!enddo
  write(unit=42,fmt=*) 
  write(unit=42,fmt=*)
  x_auzo=size(f_sare,1)
  y_auzo=size(f_sare,2)
  z_auzo=size(f_sare,3)
  n_auzo=size(f_sare,4)
  do i=1,x_auzo
  do j=1,y_auzo
  do k=1,z_auzo
  do l=1,n_auzo
    if (l==1) then
!      write(unit=42,fmt=*)
!      write(unit=42,fmt=*)
       write(unit=42,fmt=*)i,j,k,l
    endif
    if (f_sare(i,j,k,l)==0) then
      cycle
    endif
!    write(unit=42,fmt=*) auzokoen_indizeak(i)
    write(unit=42,fmt=*) f(f_sare(i,j,k,l),1:3)
!      write(unit=42,fmt=*,iostat=errore) f_sare(i,j,k,l)
!    if (errore<0) then
!      exit
!    endif
  enddo
  enddo
  enddo
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
deallocate(auzokoen_indizeak)

endprogram probatu_auzokoak
