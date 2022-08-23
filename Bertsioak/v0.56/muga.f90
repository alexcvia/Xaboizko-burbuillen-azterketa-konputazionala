module muga
use tipos
use parametroak
use funtzioak
implicit none

public ::  lortu_islapen_karratu
public ::  lortu_muga_indize_karratu

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
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_muga_indize_karratu(partikulak,n,muga_indizeak)

  real(kind=dp),intent(in),dimension(:,:)         :: partikulak
  integer,      intent(in)                        :: n              ! mugako partikula kopurua
  integer,intent(out),dimension(:)                :: muga_indizeak  ! kanpoan alokatuko dugu memoria n*4 izateko

  integer                                         :: i,kontadore
  
  kontadore=1
  muga_indizeak(:)=0
  do i=1,size(partikulak,1)
!====debug====
if (kontadore>(n*4)) then
 write(unit=109,fmt=*) "(10 errorea) arazoa muga moduluan (42 lerroan):"  
 write(unit=109,fmt=*) "--> definitutako muga partikula kopurua baino indize gehio gorde nahi dira"
 stop  
endif
!=============
    if     (partikulak(i,1)==0.0_dp) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    elseif (partikulak(i,2)==0.0_dp) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    elseif (partikulak(i,1)==luzera_x0) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    elseif (partikulak(i,2)==luzera_y0) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    endif
  enddo

endsubroutine lortu_muga_indize_karratu
endmodule muga
