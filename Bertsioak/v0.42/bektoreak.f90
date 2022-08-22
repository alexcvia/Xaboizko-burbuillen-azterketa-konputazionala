module bektoreak
use tipos
use parametroak
use funtzioak
use mcf_slineales

public :: lortu_ij_tentsore_metrikoa 
public :: lortu_Rij_gainazal
public :: lortu_Uij_gainazal
public :: lortu_gainazal_gradientea
public :: proba1,proba2

contains
subroutine lortu_ij_tentsore_metrikoa(p_guztiak,indizea,iren_auzo,iren_oinarria,g,g_1,det_g,det_g_1) 

  real(kind=dp),intent(in),dimension(:,:)   :: p_guztiak        ! partikula guztien zerrenda
  integer,intent(in)                        :: indizea          ! i partikularen indizea
  integer,intent(in),dimension(:)           :: iren_auzo        ! i_ren_auzokoak subrutinaren output-a 
  real(kind=dp),intent(in),dimension(:,:)   :: iren_oinarria    ! i_ren_sistema_lokala-ren output-a

  real(kind=dp),intent(out),dimension(2,2)  :: g           ! i-ren tentsore metrikoa
  real(kind=dp),intent(out),dimension(2,2)  :: g_1         ! g-ren alderantzizkoa
  real(kind=dp),intent(out)                 :: det_g       ! g-ren determinantea
  real(kind=dp),intent(out)                 :: det_g_1     ! g_1-ren determinantea

  real(kind=dp),dimension(:),allocatable    :: E3    ! auzokoen z posizioen bilduma i-ren oinarrian
  real(kind=dp),dimension(:,:),allocatable  :: W     ! pisuen matrizea
  real(kind=dp),dimension(:,:),allocatable  :: B     ! altuera hurbilketa funtzioen bilduma
  real(kind=dp),dimension(:,:),allocatable  :: B_T   ! B-ren iraulia

  real(kind=dp),dimension(6,6)              :: askatu   ! askatu behar dugun sistemalinealaren A matrizea
  real(kind=dp),dimension(6,1)              :: soluzioa ! sistemaren b bektorea berridatziko duena gaussj soluzioa gordetzeko
  real(kind=dp),dimension(6)                :: beta     ! gainazal hurbilketa funtzioaren parametroak

  integer                                   :: i,j,k    
  integer                                   :: j_kopurua  
  real(kind=dp)                             :: r,h
  real(kind=dp)                             :: i_x,i_y,i_z
  real(kind=dp)                             :: j_x,j_y,j_z
  real(kind=dp),dimension(3)                :: i1,i2,ni     ! i-ren autobektoreak  
  real(kind=dp),dimension(3)                :: ibektore     ! i partikularen posizio bektorea (kartesiarretan)
  real(kind=dp),dimension(3)                :: jbektore     ! j partikularen posizio bektorea (kartesiarretan)
  real(kind=dp),dimension(3)                :: Rij          ! i->j bektorea (kartesiarretan)
  real(kind=dp),dimension(3)                :: Rij_i        ! j partikularen posizio bektorea (i-ren oinarrian)

       
  h=R_auzo                      ! kernel-aren leuntze luzera (erradio eraginkorra)
  j_kopurua=size(iren_auzo,1)
  i_x=p_guztiak(indizea,1)      ! i partikularen x koord
  i_y=p_guztiak(indizea,2)      
  i_z=p_guztiak(indizea,3)      
  ibektore(1:3)=(/i_x,i_y,i_z/)
  i1(1:3)=iren_oinarria(1:3,1)  ! i-ren 1.autobektorea (gogoratu output-ean zutabea dela)
  i2(1:3)=iren_oinarria(1:3,2)  ! i-ren 2.autobektorea
  ni(1:3)=iren_oinarria(1:3,3)  ! i-ren 3.autobektorea
  
  allocate(E3(j_kopurua))
  allocate(W(j_kopurua,j_kopurua))
  allocate(B(j_kopurua,6))
  allocate(B_T(6,j_kopurua))
  W(:,:)=0.0_dp

  do i=1,j_kopurua
    j_x=p_guztiak(iren_auzo(i),1)        ! j partikularen x koord
    j_y=p_guztiak(iren_auzo(i),2)        ! j partikularen y koord
    j_z=p_guztiak(iren_auzo(i),3)        ! j partikularen z koord
    jbektore(1:3)=(/j_x,j_y,j_z/)

    Rij=ibektore-jbektore                
    r=norm2(Rij)                         ! j eta i arteko distantzia
    Rij_i(1)=sum(Rij*i1)/norm2(i1)       ! Rij bektorearen 1 osagaia i sistema lokalean
    Rij_i(2)=sum(Rij*i2)/norm2(i2)
    Rij_i(3)=sum(Rij*ni)/norm2(ni)

    E3(i)     =Rij_i(3)
    W(i,i)    =psi_kernel(r,h,j_kopurua)
    B(i,1:6)  =(/1.0_dp,Rij_i(1),Rij_i(2),Rij_i(1)**2,Rij_i(1)*Rij_i(2),Rij_i(2)**2/)
    B_T(1:6,i)=B(i,1:6)
  enddo

  askatu=matmul(matmul(B_T,W),B)
  soluzioa(:,1)=matmul(matmul(B_T,W),E3)
  call gaussj_dp(askatu,soluzioa) ! "soluzioa" bektorea inout da, b bektorea sartu eta Ax=b-rensoluzioa atera
  beta=soluzioa(:,1)

  g(1,1:2)=(/beta(2)**2 + 1.0_dp,beta(2)*beta(3)/)
  g(2,1:2)=(/beta(2)*beta(3),beta(3)**2 + 1.0_dp/)

  det_g=g(1,1)*g(2,2)-g(1,2)*g(2,1)

  g_1(1,1)= g(2,2)/det_g
  g_1(1,2)=-g(1,2)/det_g
  g_1(2,1)=-g(2,1)/det_g
  g_1(2,2)= g(1,1)/det_g

  det_g_1=g_1(1,1)*g_1(2,2)-g_1(1,2)*g_1(2,1)

!====debug====
if (det_g<0.0_dp) then
 write(unit=109,fmt=*) "(7 errorea) arazoa bektoreak moduluan (88 lerroan):"  
 write(unit=109,fmt=*) "--> g-ren determinantea negatiboa ateratzen da"
 stop  
endif
!====-----====

  deallocate(E3)
  deallocate(W)
  deallocate(B)
  deallocate(B_T)
endsubroutine lortu_ij_tentsore_metrikoa 
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_Rij_gainazal(ri,rj,ni,Rij_gainazal)
  
  real(kind=dp),intent(in),dimension(3)   :: ri            ! i-ren posizio bektorea 
  real(kind=dp),intent(in),dimension(3)   :: rj            ! j-ren posizio bektorea
  real(kind=dp),intent(in),dimension(3)   :: ni            ! i-ren sistemaren bektore normala
  real(kind=dp),intent(out),dimension(3)  :: Rij_gainazal 

  real(kind=dp),dimension(3)              :: Rij          
  
  Rij=ri-rj

  Rij_gainazal=Rij-sum(Rij*ni)*ni

endsubroutine lortu_Rij_gainazal
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_Uij_gainazal(ri,rj,ui,uj,iren_oinarria,jren_oinarria,Uij_gainazal)

  real(kind=dp),intent(in),dimension(3)   :: ri            ! i-ren posizio bektorea 
  real(kind=dp),intent(in),dimension(3)   :: rj            ! j-ren posizio bektorea
  real(kind=dp),intent(in),dimension(3)   :: ui            ! i-ren abiadura bektorea 
  real(kind=dp),intent(in),dimension(3)   :: uj            ! j-ren abiadura bektorea
  real(kind=dp),intent(in),dimension(3,3) :: iren_oinarria ! i_ren_sistema_lokala-ren output-a
  real(kind=dp),intent(in),dimension(3,3) :: jren_oinarria ! berdina, baina j partikularako
  real(kind=dp),intent(out),dimension(3)  :: Uij_gainazal 

  real(kind=dp),dimension(3)              :: i1,i2,ni      ! i-ren autobektoreak  
  real(kind=dp),dimension(3)              :: j1,j2,nj      ! j-renak

  real(kind=dp),dimension(3)              :: Rij          
  real(kind=dp),dimension(3)              :: i1_berri,i2_berri  ! oinarri tenporala
  real(kind=dp),dimension(3)              :: j1_berri,j2_berri
  real(kind=dp)                           :: ai,bi              ! abiaduraren koefizienteak oinarri tenporalean
  real(kind=dp)                           :: aj,bj
 
  i1(1:3)=iren_oinarria(1:3,1) ! i-ren 1.autobektorea (gogoratu output-ean zutabea dela)
  i2(1:3)=iren_oinarria(1:3,2) ! i-ren 2.autobektorea
  ni(1:3)=iren_oinarria(1:3,3) ! i-ren 3.autobektorea
  j1(1:3)=jren_oinarria(1:3,1) ! j-ren 1.autobektorea
  j2(1:3)=jren_oinarria(1:3,2) ! j-ren 2.autobektorea          
  nj(1:3)=jren_oinarria(1:3,3) ! j-ren 3.autobektorea

  
  Rij=ri-rj

  i1_berri=Rij/norm2(Rij)      ! norm2 bektore baten modulua lortzen duen funtzio intrintsekoa da 
  j1_berri=i1_berri

  i2_berri=bektorial(ni,i1_berri)
  j2_berri=bektorial(nj,j1_berri)

  ai=sum(ui*i1_berri)/norm2(i1_berri)
  bi=sum(ui*i2_berri)/norm2(i2_berri)
  aj=sum(uj*j1_berri)/norm2(j1_berri)
  bj=sum(uj*j2_berri)/norm2(j2_berri)

  Uij_gainazal=(aj-ai)*i1_berri+(bj-bi)*i2_berri

!====debug====
if ((Uij_gainazal(1)/=Uij_gainazal(1)).or.(Uij_gainazal(2)/=Uij_gainazal(2)).or.(Uij_gainazal(3)/=Uij_gainazal(3))) then
 write(unit=109,fmt=*) "(6 errorea) arazoa bektoreak moduluan (170 lerroan):"  
 write(unit=109,fmt=*) "--> lortutako bektorearen osagaiak ez dira zenbakiak (NaN dira)"
 stop  
endif
!====-----====
  
endsubroutine lortu_Uij_gainazal
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_proiekzio_lokala(v,iren_oinarria,v_lokal)

  real(kind=dp),intent(in),dimension(3)   :: v             ! kartesiarretan dagoen bektorea
  real(kind=dp),intent(in),dimension(3,3) :: iren_oinarria ! i_ren_sistema_lokala-ren output-a
  real(kind=dp),intent(out),dimension(2)  :: v_lokal       ! sistema lokaleko planora proiektatutako bektorea

  real(kind=dp),dimension(3)              :: i1,i2         ! i-ren autobektoreak (ez dugu normala behar) 
 
  i1(1:3)=iren_oinarria(1:3,1) ! i-ren 1.autobektorea (gogoratu output-ean zutabea dela)
  i2(1:3)=iren_oinarria(1:3,2) ! i-ren 2.autobektorea

  v_lokal(1)=sum(v*i1)/norm2(i1)   ! v bektorearen 1 osagaia i sistema lokalean
  v_lokal(2)=sum(v*i2)/norm2(i2)

endsubroutine lortu_proiekzio_lokala
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_gainazal_gradientea(g_1,Rij_gainazal,iren_oinarria,gainazal_gradientea)

  real(kind=dp),intent(in),dimension(2,2) :: g_1           ! g-ren alderantzizkoa 
  real(kind=dp),intent(in),dimension(3)   :: Rij_gainazal 
  real(kind=dp),intent(in),dimension(3,3) :: iren_oinarria ! i_ren_sistema_lokala-ren output-a
  real(kind=dp),intent(out),dimension(3)  :: gainazal_gradientea
  
  real(kind=dp),dimension(3)              :: i1,i2         ! i-ren autobektoreak (ez dugu normala behar) 
  real(kind=dp),dimension(3)              :: Rij_norm      ! Rij normalizatua,hots, Rij/r 
  real(kind=dp)                           :: h,r           ! leuntze luzera, Rij-ren modulua
  real(kind=dp)                           :: r1,r2         ! Rij-ren osagai tangentzialak i-ren oiarrian
  real(kind=dp)                           :: k             ! kalkuluak ordenatzeko
  real(kind=dp)                           :: a,b           ! gradientearen osagaiak i-ren oiarrian
 
  i1(1:3)=iren_oinarria(1:3,1) ! i-ren 1.autobektorea (gogoratu output-ean zutabea dela)
  i2(1:3)=iren_oinarria(1:3,2) ! i-ren 2.autobektorea

  h=R_auzo                     ! kernel-aren leuntze luzera (erradio eraginkorra)
  r=norm2(Rij_gainazal)
  Rij_norm=Rij_gainazal/r

  r1=sum(Rij_norm*i1)/norm2(i1)
  r2=sum(Rij_norm*i2)/norm2(i2)
  
  k=do_kernel(r,h)
  a=g_1(1,1)*r1+g_1(1,2)*r2
  b=g_1(2,1)*r1+g_1(2,2)*r2

  gainazal_gradientea=(a*k)*i1+(b*k)*i2

write(unit=45,fmt="(a,f20.14)") "------>k"  , k
write(unit=45,fmt="(a,f20.14)") "------>g11", g_1(1,1) 
write(unit=45,fmt="(a,f20.14)") "------>g12", g_1(1,2) 
write(unit=45,fmt="(a,f20.14)") "------>g21", g_1(2,1)
write(unit=45,fmt="(a,f20.14)") "------>g22", g_1(2,2)
write(unit=45,fmt="(a,f20.14)") "------>a"  , a
write(unit=45,fmt="(a,f20.14)") "------>b"  , b
write(unit=45,fmt="(a,3f20.14)") "------>gainazal_gradientea", gainazal_gradientea(1:3)

endsubroutine lortu_gainazal_gradientea
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_dibergentzia_j(Vj,hj,Uij_gainazal,gainazal_gradientea,dibergentzia_j)

  real(kind=dp),intent(in)               :: Vj
  real(kind=dp),intent(in)               :: hj
  real(kind=dp),intent(in),dimension(3)  :: Uij_gainazal 
  real(kind=dp),intent(in),dimension(3)  :: gainazal_gradientea
  real(kind=dp),intent(out),dimension(3) :: dibergentzia_j

  dibergentzia_j=(Vj/hj)*sum(Uij_gainazal*gainazal_gradientea)

endsubroutine lortu_dibergentzia
endmodule bektoreak
