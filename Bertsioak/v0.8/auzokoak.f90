module auzokoak
use tipos
use parametroak

public :: i_ren_auzokoak           
public :: zati_sarea_sortu !-> denbora pauso bakoitzeko behin soilik deitu behar

contains
subroutine i_ren_auzokoak(p_guztiak,p_sare_indizeekin,indizea,auzokoen_indizeak,psi)

  real(kind=dp),intent(in),dimension(:,:)      :: p_guztiak  !partikula guztien zerrenda, "p" "partikula" ordez ezberdintzeko bi subrutinak
  integer,intent(in),dimension(:,:)            :: p_sare_indizeekin ! zati_sarea_sortu()-ren outputa izena aldatuta
  integer,intent(in)                           :: indizea
  integer,intent(out),dimension(:),allocatable :: auzokoen_indizeak

  real(kind=dp)                                :: i_x,i_y,i_z,j_x,j_y,j_z,distantzia_ij
  integer                                      :: i,j,k,npartikula,kontadore,n_auzo
  integer,dimension(3)                         :: i_ren_sare_koord
  integer,dimension(27,3)                      :: i_ren_27_auzo_zatiak
  integer,dimension(:),allocatable             :: momentuz_auzokoen_indizeak,momentuz_auzokoen_indizeak2


  !debugeatzeko
  integer,intent(out),dimension(:,:)           :: psi
  psi(1,1:3)=p_sare_indizeekin(1,1:3)



  ! ea ze sare zatian i partikula dagoen bilatu

  i_ren_sare_koord(1)=ceiling( p_guztiak(indizea,1)/R_auzo )
  i_ren_sare_koord(2)=ceiling( p_guztiak(indizea,2)/R_auzo )
  i_ren_sare_koord(3)=ceiling( p_guztiak(indizea,3)/R_auzo )

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
  
  npartikula=size(p_guztiak,1)
  allocate(momentuz_auzokoen_indizeak(npartikula))
!  call zati_sarea_sortu(partikula_guztiak,partikulak_sare_indizeekin)
  kontadore=1
  do i=1,npartikula
  do j=1,27
  do k=1,3
    if (p_sare_indizeekin(i,k)==i_ren_27_auzo_zatiak(j,k)) then
      momentuz_auzokoen_indizeak(kontadore)=i
      kontadore=kontadore+1
    endif
  enddo
  enddo
  enddo

  ! 27 zatitxoetan bilatu benetan r<R_auzo diren puntuak (gogoratu i=gure partikula eta j=auzokoak)
  i_x=p_guztiak(indizea,1)
  i_y=p_guztiak(indizea,2)
  i_z=p_guztiak(indizea,3)
  allocate(momentuz_auzokoen_indizeak2(kontadore))
  n_auzo=1
  do i=1,kontadore
    j_x=p_guztiak(momentuz_auzokoen_indizeak(i),1)
    j_y=p_guztiak(momentuz_auzokoen_indizeak(i),2)
    j_z=p_guztiak(momentuz_auzokoen_indizeak(i),3)
    distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> kartesiarretan nahi dugu distantzia, ez sistema lokalean
    if (distantzia_ij<R_auzo) then
      momentuz_auzokoen_indizeak2(n_auzo)=momentuz_auzokoen_indizeak(i)
      n_auzo=n_auzo+1
    endif
  enddo

  ! gorde auzokoen indizeak
  allocate(auzokoen_indizeak(n_auzo))
  do i=1,n_auzo
    auzokoen_indizeak(i)=momentuz_auzokoen_indizeak2(i)
  enddo

endsubroutine i_ren_auzokoak
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine zati_sarea_sortu(partikula_guztiak,partikulak_sare_indizeekin)

  real(kind=dp),intent(in),dimension(:,:)       :: partikula_guztiak
  integer,intent(out),dimension(:,:)            :: partikulak_sare_indizeekin

!  real(kind=dp),dimension(:,:,:,:)              :: zati_sarea
!  real(kind=dp)                                 :: xluzera,yluzera,zluzera
  integer                                       :: i,j,k,nx,ny,nz,npartikula
  
!  xluzera=abs(muga_xf-muga_x0)
!  yluzera=abs(muga_yf-muga_y0)
!  zluzera=abs(muga_zf-muga_z0)
!  nx=ceiling(xluzera/R_auzo)
!  ny=ceiling(yluzera/R_auzo)
!  nz=ceiling(zluzera/R_auzo)
  
!  allocate(zati_sarea(nx,ny,nz,3))

!  do i=1,nx
!  do j=1,ny
!  do k=1,nz
!    zati_sarea(i,j,k,1:3)= (/i*R_auzo,j*R_auzo,k*R_auzo/) ! zati sarearen kubo bakoitzeko erpinak gorde
!  enddo
!  enddo
!  enddo

  npartikula=size(partikula_guztiak,1)
!  allocate(partikulak_sare_indizeekin(npartikula,3))

  do i=1,npartikula   ! --> partikula guztiei sarearen zer zatitan dauden gorde
  do j=1,3  

    partikulak_sare_indizeekin(i,j)=ceiling( partikula_guztiak(i,j)/R_auzo )  ! (muga_i0 kendu behar sarera desplazatzeko <-EZ)
    if (partikulak_sare_indizeekin(i,j)==0) then
      partikulak_sare_indizeekin(i,j)=1
    endif

  enddo
  enddo

endsubroutine zati_sarea_sortu
endmodule auzokoak
