module auzokoak
use tipos
use parametroak
use funtzioak

public :: i_ren_auzokoak           
public :: zati_sarea_sortu !-> simulazio hasieran soilik deitu behar, gainazal simulazio mugen menpe

contains
subroutine i_ren_auzokoak(partikula_guztiak,indizea,zati_sarea,auzokoen_indizeak)

  real(kind=dp),intent(in),dimension(:,:)     :: partikula_guztiak
  integer,intent(in)                          :: indizea
  real(kind=dp),dimension(:,:,:,:),intent(in) :: zati_sarea
  integer,intent(out),dimension(:),allocatable:: auzokoen_indizeak

  real(kind=dp)                               :: px,py,pz
  integer                                     :: i,j,k,nx,ny,nz
  integer,dimension(3)                        :: sare_koord
  
  
  ! ea ze sare zatian i partikula dagoen bilatu
  nx=size(zati_sarea,1)
  ny=size(zati_sarea,2)
  nz=size(zati_sarea,3)
  px=partikula_guztiak(indizea,1)
  py=partikula_guztiak(indizea,2)
  pz=partikula_guztiak(indizea,3)
  do i=1,nx
  do j=1,ny
  do k=1,nz
    if (px>zati_sarea(i,j,k,1).and.px<zati_sarea(i,j,k,1)) then
    if (py>zati_sarea(i,j,k,2).and.py<zati_sarea(i,j,k,2)) then
    if (pz>zati_sarea(i,j,k,3).and.pz<zati_sarea(i,j,k,3)) then
      sare_koord(1:3) = (/i,j,k/)
      exit
    endif
    endif
    endif
  enddo
  enddo
  enddo

  ! 9 zatitxoetan bilatu benetan r<R_auzo diren puntuak
  do i=1,nx
  do j=1,ny
  do k=1,nz
  
  enddo
  enddo
  enddo
  ! gorde auzokoen indizeak
  allocate(auzokoen_indizeak(n_auzo))
 
  do i=1,n_auzo
    auzokoen_indizeak(i)=
  enddo

endsubroutine i_ren_auzokoak
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine zati_sarea_sortu(zati_sarea)

  real(kind=dp),intent(in),dimension(:,:)         :: partikula_guztiak
  integer,intent(out),dimension(:,:,:),allocatable:: zati_sarea

  real(kind=dp),dimension(:,:,:,:)                :: zati_sarea
  real(kind=dp)                                   :: xluzera,yluzera,zluzera
  integer                                         :: i,j,k,nx,ny,nz
  
  xluzera=abs(muga_xf-muga_x0)
  yluzera=abs(muga_yf-muga_y0)
  zluzera=abs(muga_zf-muga_z0)
  nx=ceiling(xluzera/R_auzo)
  ny=ceiling(yluzera/R_auzo)
  nz=ceiling(zluzera/R_auzo)
  
  allocate(zati_sarea(nx,ny,nz,3))

  do i=1,nx
  do j=1,ny
  do k=1,nz
    zati_sarea(i,j,k,1:3)= (/i*R_auzo,j*R_auzo,k*R_auzo/) ! zati sarearen kubo bakoitzeko erpinak gorde
  enddo
  enddo
  enddo

endsubroutine zati_sarea_sortu
endmodule auzokoak
