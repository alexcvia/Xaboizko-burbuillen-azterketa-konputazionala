module prestaketa
use tipos
use parametroak
use funtzioak

public ::  lortu_gainazal_tentsioa_i
public ::  lortu_dibergentzia_j
public ::  lortu_kurbadura_lokala_j
public ::  lortu_presioa_i

contains
subroutine lortu_gainazal_tentsioa_i(xaboi_kontzentrazioa_i,gainazal_tentsioa_i)

  real(kind=dp),intent(in)               :: xaboi_kontzentrazioa_i
  real(kind=dp),intent(out)              :: gainazal_tentsioa_i

    gainazal_tentsioa_i=gamma_0-gamma_a*xaboi_kontzentrazioa_i

endsubroutine lortu_gainazal_tentsioa_i
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_dibergentzia_j(Vj,hj,Uij_gainazal,gainazal_gradientea,dibergentzia_j)

  real(kind=dp),intent(in)               :: Vj
  real(kind=dp),intent(in)               :: hj
  real(kind=dp),intent(in),dimension(3)  :: Uij_gainazal 
  real(kind=dp),intent(in),dimension(3)  :: gainazal_gradientea
  real(kind=dp),intent(out)              :: dibergentzia_j

    dibergentzia_j=(Vj/hj)*sum(Uij_gainazal*gainazal_gradientea)

endsubroutine lortu_dibergentzia
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_kurbadura_lokala_j(Vj,hj,hi,gainazal_gradientea,Rij,kurbadura_lokala_j)

  real(kind=dp),intent(in)               :: Vj
  real(kind=dp),intent(in)               :: hj
  real(kind=dp),intent(in),dimension(3)  :: Uij_gainazal 
  real(kind=dp),intent(in),dimension(3)  :: Rij
  real(kind=dp),intent(out)              :: kurbadura_lokala_j

    kurbadura_lokala_j=(Vj/hj)*(hj-hi)*2.0_dp*(norm2(Uij_gainazal)/norm2(Rij))

endsubroutine lortu_dibergentzia
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_presioa_i(hi,gainazal_tentsioa_i,kurbadura_lokala_i,dibergentzia_i,presioa_i)

  real(kind=dp),intent(in)               :: hi
  real(kind=dp),intent(in)               :: gainazal_tentsioa_i 
  real(kind=dp),intent(in)               :: kurbadura_lokala_i
  real(kind=dp),intent(in)               :: dibergentzia_i
  real(kind=dp),intent(out)              :: presioa_i

    presioa_i=           alpha_h*((hi/altuera_0)-1.0_dp)
    presioa_i=presioa_i+ alpha_k*gainazal_tentsioa_i*kurbadura_lokala_i
    presioa_i=presioa_i+ alpha_d*dibergentzia_i

endsubroutine lortu_dibergentzia
endmodule prestaketa
