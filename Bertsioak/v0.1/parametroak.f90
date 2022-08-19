module parametroak
use tipos

real,parameter::  pi            = acos(-1.0_dp)

real,parameter::  tenperatura   = 293.15_dp               ! (kelvin)

real,parameter::  R_termo       = 8.31446261815324_dp     ! (joule/K*mol)
real,parameter::  gamma_a       = R_termo*tenperatura     ! (J/mol)
real,parameter::  gamma_0       = 0.07286_dp              ! (Newton/metro)

real,parameter::  R_auzo        = 1.0

real,parameter::  alpha_h       = 1.0
real,parameter::  alpha_k       = 1.0
real,parameter::  alpha_d       = 1.0

real,parameter::  alpha_c       = 1.0

real,parameter::  mu            = 1.0

real,parameter::  kte_h_kernel  = 63.0_dp/(478.0_dp*pi*R_auzo**2)
real,parameter::  kte_o_kernel  = 15.0_dp/(pi*R_auzo**6)



endmodule parametroak
