module funtzioak
use tipos
use parametroak

public:: h_kernel !---> h altuera numerikoko kernel funtzioa
public:: o_kernel !---> erabiliko dugun kernel funtzio orokorra



!-------------------------------------------------
contains
function h_kernel(r_in,h)
real(kind=dp),intent(in):: r_in,h
real(kind=dp):: h_kernel
real(kind=dp):: kte,r

r=abs(r_in)
kte = h/3.0_dp

  if (r >= 0.0_dp .and. r < kte) then

    h_kernel = (3.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel  -  6.0_dp*(2.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel  + 15.0_dp*(1.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel*kte_h_kernel

  elseif (r >= kte .and. r < 2.0_dp*kte) then

    h_kernel = (3.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel  -  6.0_dp*(2.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel*kte_h_kernel

  elseif (r >= 2.0_dp*kte .and. r < h) then

    h_kernel = (3.0_dp-(3.0_dp*r)/h)**5
    h_kernel = h_kernel*kte_h_kernel

  else
    h_kernel = 0.0_dp
  endif

endfunction h_kernel

!---------------------
!===========================================================================
!---------------------

function o_kernel(r,h)
real(kind=dp),intent(in):: r,h
real(kind=dp):: o_kernel
real(kind=dp):: kte

  if (r>=0.0_dp .and. r<=h) then

    o_kernel = kte_o_kernel*(h-r)**3

  else
    o_kernel = 0.0_dp
  endif

endfunction o_kernel
endmodule funtzioak
