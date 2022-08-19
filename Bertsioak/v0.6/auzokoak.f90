module auzokoak
use tipos
use parametroak

public :: i_ren_auzokoak           
private :: zati_sarea_sortu !-> simulazio hasieran soilik deitu behar, gainazal simulazio mugen menpe

contains
subroutine i_ren_auzokoak(partikula_guztiak,indizea,auzokoen_indizeak)

  real(kind=dp),intent(in),dimension(:,:)     :: partikula_guztiak
  integer,intent(in)                          :: indizea
!  real(kind=dp),dimension(:,:,:,:),intent(in) :: zati_sarea
  integer,intent(out),dimension(:),allocatable:: auzokoen_indizeak

!  real(kind=dp)                               :: px,py,pz
  real(kind=dp)                               :: i_x,i_y,i_z,j_x,j_y,j_z,distantzia_ij
  integer                                     :: i,j,k,nx,ny,nz,npartikula,kontadore1,n_auzo
  integer,dimension(3)                        :: i_ren_sare_koord
  integer,dimension(27,3)                     :: i_ren_27_auzo_zatiak
  integer,dimension(:),allocatable            :: momentuz_auzokoen_indizeak,momentuz_auzokoen_indizeak2


  ! ea ze sare zatian i partikula dagoen bilatu

!  nx=size(zati_sarea,1)
!  ny=size(zati_sarea,2)
!  nz=size(zati_sarea,3)
!  px=partikula_guztiak(indizea,1)
!  py=partikula_guztiak(indizea,2)
!  pz=partikula_guztiak(indizea,3)
!  do i=1,nx
!  do j=1,ny
!  do k=1,nz
!    if (px>zati_sarea(i,j,k,1).and.px<zati_sarea(i,j,k,1)) then
!    if (py>zati_sarea(i,j,k,2).and.py<zati_sarea(i,j,k,2)) then
!    if (pz>zati_sarea(i,j,k,3).and.pz<zati_sarea(i,j,k,3)) then
!      sare_koord(1:3) = (/i,j,k/)
!      exit
!    endif
!    endif
!    endif
!  enddo
!  enddo
!  enddo
  i_ren_sare_koord(1)=ceiling( (partikula_guztiak(indizea,1)-muga_x0)/R_auzo )
  i_ren_sare_koord(2)=ceiling( (partikula_guztiak(indizea,2)-muga_y0)/R_auzo )
  i_ren_sare_koord(3)=ceiling( (partikula_guztiak(indizea,3)-muga_z0)/R_auzo )



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
  
  npartikula=size(partikula_guztiak,1)
  allocate(momentuz_auzokoen_indizeak(npartikula))
  call zati_sarea_sortu(partikula_guztiak,partikulak_sare_indizeekin)
  kontadore=1
  do i=1,npartikula
  do j=1,27
    if (partikulak_sare_indizeekin(i,1:3)==i_ren_27_auzo_zatiak(j,1:3)) then
      momentuz_auzokoen_indizeak(kontadore)=i
      kontadore=kontadore+1
    endif
  enddo
  enddo

  ! 27 zatitxoetan bilatu benetan r<R_auzo diren puntuak (gogoratu i=gure partikula eta j=auzokoak)
  i_x=partikula_guztiak(indizea,1)
  i_y=partikula_guztiak(indizea,2)
  i_z=partikula_guztiak(indizea,3)
  allocate(momentuz_auzokoen_indizeak2(kontadore))
  n_auzo=1
  do i=1,kontadore
    j_x=partikula_guztiak(momentuz_auzokoen_indizeak(i),1)
    j_y=partikula_guztiak(momentuz_auzokoen_indizeak(i),2)
    j_z=partikula_guztiak(momentuz_auzokoen_indizeak(i),3)
    distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> ez da behar -muga_i0 egitea kartesiarretan daudelako guztiak
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
  integer,intent(out),dimension(:,:),allocatable:: partikulak_sare_indizeekin

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
  allocate(partikulak_sare_indizeekin(npartikula,3))

  do i=1,npartikula   ! --> partikula guztiei sarearen zer zatitan dauden gorde

    partikulak_sare_indizeekin(i,1)=ceiling( (partikula_guztiak(i,1)-muga_x0)/R_auzo )  ! muga_i0 kendu behar sarera desplazatzeko
    partikulak_sare_indizeekin(i,2)=ceiling( (partikula_guztiak(i,2)-muga_y0)/R_auzo )
    partikulak_sare_indizeekin(i,3)=ceiling( (partikula_guztiak(i,3)-muga_z0)/R_auzo )

  enddo

endsubroutine zati_sarea_sortu
endmodule auzokoak
