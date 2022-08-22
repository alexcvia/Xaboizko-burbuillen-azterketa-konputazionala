module parametroak
use tipos

!leuntze luzera
  real(kind=dp),parameter::  R_auzo        = 0.42_dp

!denbora pausua
  real(kind=dp),parameter::  delta_t       = 0.2_dp*R_auzo

!kalibrazio parametroak
  real(kind=dp),parameter::  alpha_h       = 1.0_dp                  ! altuera konpresibilitatea
  real(kind=dp),parameter::  alpha_k       = 1.0_dp                  ! kurbaduraren eragina
  real(kind=dp),parameter::  alpha_d       = 1.0_dp                  ! dibergentziaren eragina
  real(kind=dp),parameter::  alpha_c       = 2.3e-9_dp               ! kontzentrazio difusio koefizientea

!mate
  real(kind=dp),parameter::  pi            = acos(-1.0_dp)

!gainazal tentsioa
  real(kind=dp),parameter::  tenperatura   = 298.15_dp               ! (kelvin)
  real(kind=dp),parameter::  R_termo       = 8.3144598_dp            ! (joule/K*mol)
  real(kind=dp),parameter::  gamma_a       = R_termo*tenperatura     ! (J/mol)
  real(kind=dp),parameter::  gamma_0       = 0.07275_dp              ! (Newton/metro)

!biskositatea
  real(kind=dp),parameter::  mu            = 8.9e-4_dp               ! (Pa*s)

!azelerazio grabitatorioa
  real(kind=dp),parameter::  grabitate     = -9.81_dp                ! (m/s**2)

!kernel parametroak
  real(kind=dp),parameter::  kte_h_kernel  = 63.0_dp/(478.0_dp*pi*R_auzo**2)
  real(kind=dp),parameter::  kte_o_kernel  = 10.0_dp/(pi*R_auzo**5)

!========================================= Hasierako Baldintzak ===========================================================
!atseden xaboi kontzentrazioa
  real(kind=dp),parameter::  gammahandi_0  = 6.67e-8_dp              ! (mol/m**2)

!atseden altuera
  real(kind=dp),parameter::  altuera_0     = 4e-7_dp                 ! (m)

!geometria


!orinentazioa normalaren kalkulorako
  real(kind=dp),parameter,dimension(3)::  OrientazioGlobala = (/0.0_dp,0.0_dp,1.0_dp/)  ! simulazioaren orientazio normala
  real(kind=dp),parameter::  OrientazioPlanoGlobala         = 0.0_dp                    ! simulazioaren simetria planoaren "altuera"
                                                                                      ! ---> gainazal irekietarako (normalena) 0 jarri



!ez da behar:
  !real(kind=dp),parameter::  muga_x0       = 1.0    ! simulazioaren mugalde baldintzak
  !real(kind=dp),parameter::  muga_xf       = 10.0   ! prisma honen barruan egongo dira
  !real(kind=dp),parameter::  muga_y0       = 1.0    !
  !real(kind=dp),parameter::  muga_yf       = 10.0   ! honen helburua auzokoen bilaketaren zati sarearen mugak
  !real(kind=dp),parameter::  muga_z0       = 1.0    ! jartzea da, honen kanpoan ez da gainazal azterketarik egingo,
  !real(kind=dp),parameter::  muga_zf       = 10.0   ! soilik fluido normalen simulazioa

endmodule parametroak
