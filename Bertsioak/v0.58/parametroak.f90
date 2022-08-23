module parametroak
use tipos
implicit none

!leuntze luzera
  real(kind=dp),parameter::  R_auzo        = 0.0042_dp

!denbora pausua
  !real(kind=dp),parameter::  delta_t       = 4.0_dp*R_auzo         ! 60fps
  !real(kind=dp),parameter::  delta_t       = 4.0_dp*R_auzo*2.0_dp  ! 30fps
  real(kind=dp),parameter::  delta_t       = 0.1_dp                ! 10fps
  integer,parameter      ::  t_total       = 370

!kalibrazio parametroak
  real(kind=dp),parameter::  alpha_h       = 1000.0_dp             ! altuera konpresibilitatea
  real(kind=dp),parameter::  alpha_k       = 1000.0_dp             ! kurbaduraren eta gainazal tentsio eragina
  real(kind=dp),parameter::  alpha_d       = 1000.0_dp             ! dibergentziaren eragina
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
  !real(kind=dp),parameter::  grabitate     = -9.81_dp                ! (m/s**2)
  real(kind=dp),parameter::  grabitate     = -9.81_dp*0.00000001_dp      ! (m/s**2)

!kernel parametroak
  real(kind=dp),parameter::  kte_h_kernel  = 63.0_dp/(478.0_dp*pi*R_auzo**2)
  real(kind=dp),parameter::  kte_o_kernel  = 10.0_dp/(pi*R_auzo**5)

!========================================= Hasierako Baldintzak ===========================================================
 !atseden xaboi kontzentrazioa:
  real(kind=dp),parameter::  kontzentrazioa_0 = 6.67e-8_dp           ! (mol/m**2)
 !atseden altuera:
  real(kind=dp),parameter::  lodiera_0        = 4.0e-7_dp            ! (m)
 !atseden dentsitatea (urarena):
  real(kind=dp),parameter::  dentsitate_0     = 997.0_dp             ! (kg/m**3)
 !atseden bortizitatea:
  real(kind=dp),parameter::  bortizitate_0    = 0.001_dp             ! (kg/s**2) a ojo
 
!geometria hasieran
  integer,parameter      ::  npartikula_0  = 2500                    ! ( ) ! zenbaki karratua izatea nahi dugu niformeki marrazteko
  real(kind=dp),parameter::  x0            = 1.0_dp                  ! (m) ! puntu hodeia ze koordenatuetan hasten den
  real(kind=dp),parameter::  y0            = 1.0_dp                  ! (m) ! puntu hodeia ze koordenatuetan hasten den
  real(kind=dp),parameter::  z0            = 1.0_dp                  ! (m) ! puntu hodeia ze koordenatuetan hasten den
  real(kind=dp),parameter::  luzera_x0     = 0.05_dp                 ! (m)
  real(kind=dp),parameter::  luzera_y0     = 0.05_dp                 ! (m)
  real(kind=dp),parameter::  azalera_0     = luzera_x0*luzera_y0     ! (m**2)
  real(kind=dp),parameter::  bolumena_0    = azalera_0*lodiera_0     ! (m**3)

  real(kind=dp),parameter::  muga_x0       = x0-R_auzo    
  real(kind=dp),parameter::  muga_xf       = x0+luzera_x0+R_auzo
  real(kind=dp),parameter::  muga_y0       = y0-R_auzo
  real(kind=dp),parameter::  muga_yf       = y0+luzera_y0+R_auzo  
  !real(kind=dp),parameter::  muga_z0       = z0-R_auzo
  !real(kind=dp),parameter::  muga_zf       = z0+luzera_z0+R_auzo  
  
!geometriaren mendeko hasierako balioak
  real(kind=dp),parameter::  masa_0        = dentsitate_0*bolumena_0 ! (kg) masa osoa
  !real(kind=dp),parameter::  mi_0          = masa_0/npartikula_0     ! (kg) masa partikulako
  real(kind=dp),parameter::  mi_0          = 1.0_dp                  ! (kg) masa partikulako
  real(kind=dp),parameter::  Vi_0          = bolumena_0/npartikula_0 ! (kg) bolumen partikulako
  real(kind=dp),parameter::  hi_0          = lodiera_0/2.0_dp        ! (m)  altuera-erdia hasieran
    
!memoria parametroak
  integer,parameter      ::  mMin_iKoord     = 50                    ! zati sarea sortzean memoria minimoa koordenatu indizeetarako
  integer,parameter      ::  mMin_iPartikula = 100                   ! zati sarea sortzean memoria minimoa partikula indizeetarako
  integer,parameter      ::  mMin_jauzoko    = 150                   ! auzokoen indize memoria esleipen maximoaren balio minimoa Rij eta ggrad-erako

!kanpo indar extrak
  real(kind=dp),dimension(3),parameter:: extra_indarra_1    = (/0.0001_dp,0.0_dp,0.0_dp/)  ! kanpo indar 1

!orinentazioa normalaren kalkulorako
  real(kind=dp),parameter,dimension(3)::  OrientazioGlobala = (/0.0_dp,0.0_dp,1.0_dp/)  ! simulazioaren orientazio normala
  real(kind=dp),parameter::  OrientazioPlanoGlobala         = 0.0_dp                    ! simulazioaren simetria planoaren "altuera"
                                                                                      ! ---> gainazal irekietarako (normalena) 0 jarri


endmodule parametroak
