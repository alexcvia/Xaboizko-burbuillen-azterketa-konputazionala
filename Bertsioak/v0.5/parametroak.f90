module parametroak
use tipos

real(kind=dp),parameter::  pi            = acos(-1.0_dp)

real(kind=dp),parameter::  tenperatura   = 293.15_dp               ! (kelvin)

real(kind=dp),parameter::  R_termo       = 8.31446261815324_dp     ! (joule/K*mol)
real(kind=dp),parameter::  gamma_a       = R_termo*tenperatura     ! (J/mol)
real(kind=dp),parameter::  gamma_0       = 0.07286_dp              ! (Newton/metro)

real(kind=dp),parameter::  R_auzo        = 1.0

real(kind=dp),parameter::  alpha_h       = 1.0
real(kind=dp),parameter::  alpha_k       = 1.0
real(kind=dp),parameter::  alpha_d       = 1.0

real(kind=dp),parameter::  alpha_c       = 1.0

real(kind=dp),parameter::  mu            = 1.0

real(kind=dp),parameter::  kte_h_kernel  = 63.0_dp/(478.0_dp*pi*R_auzo**2)
real(kind=dp),parameter::  kte_o_kernel  = 15.0_dp/(pi*R_auzo**6)


real(kind=dp),parameter::  muga_x0       = 1.0    ! simulazioaren mugalde baldintzak
real(kind=dp),parameter::  muga_xf       = 10.0   ! prisma honen barruan egongo dira
real(kind=dp),parameter::  muga_y0       = 1.0    !
real(kind=dp),parameter::  muga_yf       = 10.0   ! honen helburua auzokoen bilaketaren zati sarearen mugak
real(kind=dp),parameter::  muga_y0       = 1.0    ! jartzea da, honen kanpoan ez da gainazal azterketarik egingo,
real(kind=dp),parameter::  muga_yf       = 10.0   ! soilik fluido normalen simulazioa

endmodule parametroak
