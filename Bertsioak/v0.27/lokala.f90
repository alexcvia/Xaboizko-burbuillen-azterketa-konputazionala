module lokala
use tipos
use parametroak
use auzokoak
use funtzioak
use mcf_diagonalizacion

public :: i_ren_sistema_lokala 
!public :: 
!interface tridiagonalizatu
!  module procedure tred2
!end interface


contains
subroutine i_ren_sistema_lokala(p_guztiak,indizea,iren_auzo,iren_oinarria) 

  real(kind=dp),intent(in),dimension(:,:)      :: p_guztiak  ! partikula guztien zerrenda
  integer,intent(in)                           :: indizea    ! i partikularen indizea
  integer,intent(in),dimension(:)              :: iren_auzo ! i_ren_auzokoak subrutinaren output-a 
  real(kind=dp),intent(out),dimension(3,3)     :: iren_oinarria ! (e1_i,e2_i,n_i) i-ren oinarri bektoreak
  
  real(kind=dp),dimension(3)                   :: ibektore ! i partikularen posizio bektorea
  real(kind=dp),dimension(3)                   :: kernel_zentru ! i partikularen kernel zentru posizio bektorea
  real(kind=dp)                                :: kernel_masa ! kernel pisuen batura, hots, W_ij balio guztien batura

  integer                                      :: i,j,k  
  integer                                      :: j_kopurua  
  real(kind=dp)                                :: r,h
  real(kind=dp)                                :: i_x,i_y,i_z
  real(kind=dp)                                :: j_x,j_y,j_z

  real(kind=dp),dimension(3,3)                 :: kobariantza_matrizea
  real(kind=dp),dimension(3,3)                 :: delta_matrizea
  real(kind=dp),dimension(3)                   :: delta_jzentru
  real(kind=dp)                                :: zentru_x,zentru_y,zentru_z
  
  real(kind=dp),dimension(3)                 :: diagonal
  real(kind=dp),dimension(3)                 :: estradiagonal 

  h=R_auzo                      ! kernel-aren leuntze luzera (erradio eraginkorra)
  j_kopurua=size(iren_auzo,1)
  i_x=p_guztiak(indizea,1)      ! i partikularen x koord
  i_y=p_guztiak(indizea,2)      ! i partikularen y koord
  i_z=p_guztiak(indizea,3)      ! i partikularen z koord
  ibektore(1:3)=(/i_x,i_y,i_z/)

  kernel_zentru(:)=0.0_dp
  kernel_masa=0.0_dp
  

!-----------------------------------------------------
! kernel-masa eta kernel-zentrua kalkulatu (do berean lortu eta zatitu bukaeran)
  
  do i=1,j_kopurua
    j_x=p_guztiak(iren_auzo(i),1)                    ! j partikularen x koord
    j_y=p_guztiak(iren_auzo(i),2)                    ! j partikularen y koord
    j_z=p_guztiak(iren_auzo(i),3)                    ! j partikularen z koord
    r=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) ! j eta i arteko distantzia

    kernel_zentru=kernel_zentru+fi_kernel(r,h)*ibektore
    kernel_masa=kernel_masa+fi_kernel(r,h)
  enddo
  kernel_zentru=kernel_zentru/kernel_masa            ! normalizatu


!-----------------------------------------------------
! kobariantza matrizea kalkulatu

  kobariantza_matrizea(:,:)=0.0_dp
  zentru_x=kernel_zentru(1)
  zentru_y=kernel_zentru(2)
  zentru_z=kernel_zentru(3)
  
  do i=1,j_kopurua
    j_x=p_guztiak(iren_auzo(i),1)                                   ! j partikularen x koord
    j_y=p_guztiak(iren_auzo(i),2)                                   ! j partikularen y koord
    j_z=p_guztiak(iren_auzo(i),3)                                   ! j partikularen z koord
!    r=sqrt( (j_x-zentru_x)**2+(j_y-zentru_y)**2+(j_z-zentru_z)**2 ) ! j eta kernel_zentru arteko distantzia
    r=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 )                ! j eta i arteko distantzia
    delta_jzentru=(/(j_x-zentru_x),(j_y-zentru_y),(j_z-zentru_z)/) 
    do j=1,3
      do k=1,3
        delta_matrizea(j,k)=delta_jzentru(j)*delta_jzentru(k)       ! delta bektorearekin matrizea sortu -> delta*delta^T
      enddo
    enddo  
    kobariantza_matrizea=kobariantza_matrizea+fi_kernel(r,h)*delta_matrizea
  enddo
  kobariantza_matrizea=kobariantza_matrizea/kernel_masa             ! normalizatu


!-----------------------------------------------------
! kobariantza matrizearen autobektoreak lortu

  call tred2_dp(kobariantza_matrizea,diagonal,estradiagonal)  ! kobariantza matrizea hiru-diagonal bihurtu (bertan gorde)
  call tqli_dp(diagonal,estradiagonal,kobariantza_matrizea)   ! autobalioak ("diagonal" barruan bueltatu) eta autobektoreak (kobariantza matrize zutabeetan)
  call eigsrt_dp(diagonal,kobariantza_matrizea)               ! Autobalio eta autobektoreak txikienetik handienera ordenatu

  iren_oinarria(1:3,1)=kobariantza_matrizea(1:3,3)
  iren_oinarria(1:3,2)=kobariantza_matrizea(1:3,2)         ! Guk handitik txikienera nahi dugu, beraz buelta eman zutabeei
  iren_oinarria(1:3,3)=kobariantza_matrizea(1:3,1)         !    --> normalizatuta dago


!-----------------------------------------------------
! konprobatu ea normala ondo orientatuta dagoela gure simulazioaren baldintzetarako
  
  if (OrientazioPlanoGlobala==0.0_dp) then !-------: 0 bada, ez dugu simetria planorik erabili behar :--------

        if (sum(OrientazioGlobala*iren_oinarria(1:3,3))<0) then   ! normala ez badago "gora" begira, buelta eman oinarriari
          iren_oinarria(1:3,1)=-kobariantza_matrizea(1:3,3)
          iren_oinarria(1:3,2)=-kobariantza_matrizea(1:3,2)    
          iren_oinarria(1:3,3)=-kobariantza_matrizea(1:3,1)    
        endif                                                ! bestela ez egin ezer



  else !--------: 0 ez bada, simetria planoa erabiliko dugu normalak konprobatzeko

    if (sum(OrientazioGlobala*(ibektore-OrientazioGlobala*OrientazioPlanoGlobala))>0) then ! i_partikula planoaren "gainean" al dagoen ikusi
        if (sum(OrientazioGlobala*iren_oinarria(1:3,3))<0) then   ! normala ez badago "gora" begira, buelta eman oinarriari
          iren_oinarria(1:3,1)=-kobariantza_matrizea(1:3,3)
          iren_oinarria(1:3,2)=-kobariantza_matrizea(1:3,2)    
          iren_oinarria(1:3,3)=-kobariantza_matrizea(1:3,1)    
        endif                                                ! bestela ez egin ezer
    else
        if (sum(OrientazioGlobala*iren_oinarria(1:3,3))>0) then   ! normala ez badago "bera" begira, buelta eman oinarriari
          iren_oinarria(1:3,1)=-kobariantza_matrizea(1:3,3)
          iren_oinarria(1:3,2)=-kobariantza_matrizea(1:3,2)    
          iren_oinarria(1:3,3)=-kobariantza_matrizea(1:3,1)    
        endif                                                ! bestela ez egin ezer
    endif
  endif 

endsubroutine i_ren_sistema_lokala 
endmodule lokala
