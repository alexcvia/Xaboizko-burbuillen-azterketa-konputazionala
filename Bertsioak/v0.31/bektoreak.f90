module bektoreak
use tipos
use parametroak
use funtzioak

public :: ij_tentsore_metrikoa 
public :: lortu_Rij_gainazal
public :: lortu_Uij_gainazal

contains
subroutine ij_tentsore_metrikoa(iren_oinarria,jren_oinarria,g,g_1,det_g,det_g_1)

  real(kind=dp),intent(in),dimension(3,3)   :: iren_oinarria ! i_ren_sistema_lokala-ren output-a
  real(kind=dp),intent(in),dimension(3,3)   :: jren_oinarria ! berdina, baina j partikularako
  real(kind=dp),intent(out),dimension(2,2)  :: g             ! i-ren tentsore metrikoa
  real(kind=dp),intent(out),dimension(2,2)  :: g_1           ! g-ren alderantzizkoa
  real(kind=dp),intent(out)                 :: det_g         ! g-ren determinantea
  real(kind=dp),intent(out)                 :: det_g_1       ! g_1-ren determinantea

  real(kind=dp),dimension(3)                :: i1,i2         
  real(kind=dp),dimension(3)                :: j1,j2         

  i1(1:3)=iren_oinarria(1:3,1) ! i-ren 1.autobektorea (gogoratu output-ean zutabea dela)
  i2(1:3)=iren_oinarria(1:3,2) ! i-ren 2.autobektorea
  j1(1:3)=jren_oinarria(1:3,1) ! j-ren 1.autobektorea
  j2(1:3)=jren_oinarria(1:3,2) ! j-ren 2.autobektorea

  g(1,1:2)=(/sum(i1*j1),sum(i1*j2)/)
  g(2,1:2)=(/sum(j1*i1),sum(j2*i2)/)

  det_g=g(1,1)*g(2,2)-g(1,2)*g(2,1)

  g_1(1,1)= g(2,2)/det_g
  g_1(1,2)=-g(1,2)/det_g
  g_1(2,1)=-g(2,1)/det_g
  g_1(2,2)= g(1,1)/det_g

  det_g_1=g_1(1,1)*g_1(2,2)-g_1(1,2)*g_1(2,1)

endsubroutine ij_tentsore_metrikoa 
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_Rij_gainazal(ri,rj,ni,Rij_gainazal)
  
  real(kind=dp),intent(in),dimension(3)  :: ri           ! i-ren posizio bektorea 
  real(kind=dp),intent(in),dimension(3)  :: rj           ! j-ren posizio bektorea
  real(kind=dp),intent(in),dimension(3)  :: ni           ! i-ren bektore normala
  real(kind=dp),intent(out),dimension(3) :: Rij_gainazal 

  real(kind=dp),dimension(3)             :: Rij          
  
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
  
endsubroutine lortu_Uij_gainazal
endmodule bektoreak
