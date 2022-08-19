program probatu_auzokoak
use tipos
use parametroak
use auzokoak

real(kind=dp)                           ::x,y,z,kte
integer                                 ::i,j,k,n,kontadore_nagusi,i_partikula,n_auzo_atera
real(kind=dp),dimension(:,:),allocatable::f
integer,dimension(:,:),allocatable      ::f_sare
integer,dimension(:),allocatable        ::auzokoen_indizeak
!integer,dimension(1,3)                    ::psi

!  real(kind=dp),intent(in),dimension(:,:)      :: f  !partikula guztien zerrenda, "p" "partikula" ordez ezberdintzeko bi subrutinak
!  integer,intent(in),dimension(:,:)            :: f_sare ! zati_sarea_sortu()-ren outputa izena aldatuta
!  integer,intent(in)                           :: i_partikula
!  integer,intent(out),dimension(:),allocatable :: auzokoen_i_partikulak

  real(kind=dp)                                :: i_x,i_y,i_z,j_x,j_y,j_z,distantzia_ij
  integer                                      :: npartikula,kontadore,n_auzo
  integer,dimension(3)                         :: i_ren_sare_koord
  integer,dimension(27,3)                      :: i_ren_27_auzo_zatiak
  integer,dimension(:),allocatable             :: momentuz_auzokoen_indizeak,momentuz_auzokoen_indizeak2

open(unit=42,action="write",status="replace",file="datuauzokoak.dat")
n=100

!   z=kte planoa eraiki
  allocate(f(n*n,3))
  kte=5.0_dp
  kontadore_nagusi=1
  do i=1,n
    x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
    do j=1,n
      y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
      f(kontadore_nagusi,1:3)=(/x,y,kte/)
      write(unit=42,fmt=*) f(kontadore_nagusi,1:3)
      kontadore_nagusi=kontadore_nagusi+1
    enddo
  enddo

do i=1,n
    write(unit=42,fmt=*) "aaaaaaaaaaaaaaaaah"
enddo

!   bigaren zatian idatzi i_ren auzokoak
  allocate(f_sare(n*n,3))
  i_partikula=2500
  call zati_sarea_sortu(f,f_sare)
!  call i_ren_auzokoak(f,f_sare,i_partikula,auzokoen_i_partikulak,psi)

write(unit=42,fmt=*) "1"
!---------------------------------------------------------------------------


  !debugeatzeko
!  integer,intent(out),dimension(:,:)           :: psi
!  psi(1,1:3)=f_sare(1,1:3)



  ! ea ze sare zatian i partikula dagoen bilatu

  i_ren_sare_koord(1)=ceiling( f(i_partikula,1)/R_auzo )
  i_ren_sare_koord(2)=ceiling( f(i_partikula,2)/R_auzo )
  i_ren_sare_koord(3)=ceiling( f(i_partikula,3)/R_auzo )

write(unit=42,fmt=*) "2"
  ! 27 zatitxoetan dauden partikulak lortu
  i_ren_27_auzo_zatiak(1 ,1:3)=i_ren_sare_koord(1:3)
  i_ren_27_auzo_zatiak(2 ,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(3 ,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(4 ,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(5 ,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(6 ,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(7 ,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(8 ,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(9 ,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)  /)

  i_ren_27_auzo_zatiak(10,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(11,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(12,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(13,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(14,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(15,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(16,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(17,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(18,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)-1/)

  i_ren_27_auzo_zatiak(19,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(20,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(21,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(22,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(23,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(24,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(25,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(26,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(27,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)+1/)
write(unit=42,fmt=*) "3"

  npartikula=size(f,1)
  allocate(momentuz_auzokoen_indizeak(npartikula))

write(unit=42,fmt=*) "3.1"
!  call zati_sarea_sortu(partikula_guztiak,partikulak_sare_indizeekin)
  kontadore=1
  do i=1,npartikula
  do j=1,27
!  do k=1,3
    if (f_sare(i,1)==i_ren_27_auzo_zatiak(j,1).and.&
        f_sare(i,2)==i_ren_27_auzo_zatiak(j,2).and.&
        f_sare(i,3)==i_ren_27_auzo_zatiak(j,3)     &
    ) then
      momentuz_auzokoen_indizeak(kontadore)=i
      kontadore=kontadore+1
write(unit=42,fmt=*) "3.2=",kontadore
    endif
!  enddo
  enddo
  enddo

write(unit=42,fmt=*) "4"
  ! 27 zatitxoetan bilatu benetan r<R_auzo diren puntuak (gogoratu i=gure partikula eta j=auzokoak)
  i_x=f(i_partikula,1)
  i_y=f(i_partikula,2)
  i_z=f(i_partikula,3)
  allocate(momentuz_auzokoen_indizeak2(kontadore))
  n_auzo=1
  do i=1,kontadore
    j_x=f(momentuz_auzokoen_indizeak(i),1)
    j_y=f(momentuz_auzokoen_indizeak(i),2)
    j_z=f(momentuz_auzokoen_indizeak(i),3)
    distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> kartesiarretan nahi dugu distantzia, ez sistema lokalean
    if (distantzia_ij<R_auzo) then
      momentuz_auzokoen_indizeak2(n_auzo)=momentuz_auzokoen_indizeak(i)
      n_auzo=n_auzo+1
    endif
  enddo

write(unit=42,fmt=*) "5"
  ! gorde auzokoen i_partikulak
  allocate(auzokoen_indizeak(n_auzo))
  do i=1,n_auzo
    auzokoen_indizeak(i)=momentuz_auzokoen_indizeak2(i)
  enddo

write(unit=42,fmt=*) "6"
!---------------------------------------------------------------------------



do i=1,n*n
  write(unit=42,fmt=*) f_sare(i,1:3)
enddo
!  write(unit=42,fmt=*)
!  write(unit=42,fmt=*)
!  n_auzo_atera=size(auzokoen_indizeak,1)
!  do i=1,n_auzo_atera
!    write(unit=42,fmt=*) f(auzokoen_indizeak(i),1:3)
!  enddo










!-------
close(unit=42)
deallocate(f)
deallocate(f_sare)
deallocate(auzokoen_indizeak)
deallocate(momentuz_auzokoen_indizeak)
deallocate(momentuz_auzokoen_indizeak2)

endprogram probatu_auzokoak
