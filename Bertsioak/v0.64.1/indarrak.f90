module indarrak
use tipos
use parametroak
use funtzioak
implicit none

public ::  lortu_kanpo_indarra_i
public ::  lortu_bortizitate_indarra_j
public ::  lortu_presio_indarra_j
public ::  lortu_Marangoni_indarra_j
public ::  lortu_kapilare_indarra_j
public ::  lortu_biskositate_indarra_j

contains
subroutine lortu_kanpo_indarra_i(mi,f_ext,kanpo_indarra_i)

  real(kind=dp),intent(in)                       :: mi
  real(kind=dp),intent(in),dimension(3),optional :: f_ext
  real(kind=dp),intent(out),dimension(3)         :: kanpo_indarra_i

  kanpo_indarra_i=mi*(/grabitate,0.0_dp,0.0_dp/)
  if (present(f_ext)) then
    kanpo_indarra_i=kanpo_indarra_i+f_ext
  endif

endsubroutine lortu_kanpo_indarra_i
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_bortizitate_indarra_j(bortizitate_j,Rij_gainazal,nj,bortizitate_indarra_j)

  real(kind=dp),intent(in)               :: bortizitate_j
  real(kind=dp),intent(in),dimension(3)  :: Rij_gainazal
  real(kind=dp),intent(in),dimension(3)  :: nj
  real(kind=dp),intent(out),dimension(3) :: bortizitate_indarra_j

  bortizitate_indarra_j=-bektorial(Rij_gainazal,bortizitate_j*nj)

endsubroutine lortu_bortizitate_indarra_j
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_presio_indarra_j(hi,hj,Vi,Vj,pi,pj,gainazal_gradientea,presio_indarra_j)

  real(kind=dp),intent(in)               :: hi,hj
  real(kind=dp),intent(in)               :: Vi,Vj
  real(kind=dp),intent(in)               :: pi,pj
  real(kind=dp),intent(in),dimension(3)  :: gainazal_gradientea
  real(kind=dp),intent(out),dimension(3) :: presio_indarra_j

  presio_indarra_j=2.0_dp*Vi*hi*Vj*( (pi/(hi**2)) + (pj/(hj**2)) )*gainazal_gradientea

endsubroutine lortu_presio_indarra_j
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_Marangoni_indarra_j(hi,hj,Vi,Vj,tentsio_i,tentsio_j,gainazal_gradientea,Marangoni_indarra_j)

  real(kind=dp),intent(in)               :: hi,hj
  real(kind=dp),intent(in)               :: Vi,Vj
  real(kind=dp),intent(in)               :: tentsio_i,tentsio_j
  real(kind=dp),intent(in),dimension(3)  :: gainazal_gradientea
  real(kind=dp),intent(out),dimension(3) :: Marangoni_indarra_j

  Marangoni_indarra_j=(Vi/hi)*(Vj/hj)*(tentsio_j-tentsio_i)*gainazal_gradientea
  Marangoni_indarra_j=alpha_m*Marangoni_indarra_j

endsubroutine lortu_Marangoni_indarra_j
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_kapilare_indarra_j(hi,hj,Vi,Vj,tentsio_i,ni,ri,rj,gainazal_gradientea,Rij_gainazal,kapilare_indarra_j)

  real(kind=dp),intent(in)               :: hi,hj
  real(kind=dp),intent(in)               :: Vi,Vj
  real(kind=dp),intent(in)               :: tentsio_i
  real(kind=dp),intent(in),dimension(3)  :: ni
  real(kind=dp),intent(in),dimension(3)  :: ri,rj
  real(kind=dp),intent(in),dimension(3)  :: gainazal_gradientea
  real(kind=dp),intent(in),dimension(3)  :: Rij_gainazal
  real(kind=dp),intent(out),dimension(3) :: kapilare_indarra_j

  real(kind=dp),dimension(3)             :: Rij

  Rij=ri-rj !honetarako Rij behar dut, ez betiko Rij_gainazal osagai normala nahi dudalako lortu

  kapilare_indarra_j=((Vi*tentsio_i)/hi)*ni*(Vj/hj)*sum(-Rij*ni)*2.0_dp*(norm2(gainazal_gradientea)/norm2(Rij_gainazal))
  kapilare_indarra_j=alpha_l*kapilare_indarra_j

endsubroutine lortu_kapilare_indarra_j
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_biskositate_indarra_j(hj,Vi,Vj,ni,ui,uj,gainazal_gradientea,Rij_gainazal,biskositate_indarra_j)

  real(kind=dp),intent(in)               :: hj
  real(kind=dp),intent(in)               :: Vi,Vj
  real(kind=dp),intent(in),dimension(3)  :: ni
  real(kind=dp),intent(in),dimension(3)  :: ui,uj
  real(kind=dp),intent(in),dimension(3)  :: gainazal_gradientea
  real(kind=dp),intent(in),dimension(3)  :: Rij_gainazal
  real(kind=dp),intent(out),dimension(3) :: biskositate_indarra_j

  real(kind=dp),dimension(3)             :: Uij

  Uij=uj-ui !honetarako Uij behar dut, ez betiko Uij_gainazal osagai normala nahi dudalako erabili

  biskositate_indarra_j=Vi*mu*(Vj/hj)*( Uij-sum(Uij*ni)*ni )*2.0_dp*(norm2(gainazal_gradientea)/norm2(Rij_gainazal))
  biskositate_indarra_j=alpha_b*biskositate_indarra_j

endsubroutine lortu_biskositate_indarra_j


endmodule indarrak
