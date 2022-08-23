module muga
use tipos
use parametroak
use funtzioak
implicit none

public ::  lortu_islapen_karratu

contains
subroutine lortu_islapen_karratu(pos,ui)

  real(kind=dp),intent(inout),dimension(3)         :: pos
  real(kind=dp),intent(inout),dimension(3)         :: ui

  if ( (pos(1)<0.0_dp).or.(pos(1)>luzera_x0) ) then
    pos   = pos - ui*delta_t
    ui(1) = -ui(1)
    pos   = pos + ui*delta_t
  endif
  if ( (pos(2)<0.0_dp).or.(pos(2)>luzera_y0) ) then ! bi if jarri ditut elseif ordez porseakaso justo eskinatik pasatzen bada partikula bat
    pos   = pos - ui*delta_t
    ui(2) = -ui(2)
    pos   = pos + ui*delta_t
  endif

endsubroutine lortu_islapen_karratu

endmodule muga
