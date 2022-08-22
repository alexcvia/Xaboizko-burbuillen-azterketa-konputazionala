module bektoreak
use tipos
use parametroak
use funtzioak

public :: ij_tentsore_metrikoa 

!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------

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

endmodule bektoreak
