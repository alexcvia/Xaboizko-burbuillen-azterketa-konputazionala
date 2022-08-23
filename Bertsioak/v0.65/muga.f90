module muga
use tipos
use parametroak
use funtzioak
implicit none

public ::  lortu_islapen_karratu
public ::  lortu_muga_indize_karratu
public ::  lortu_ez_itsatsi_karratu
private::  lortu_mugakanpo_auzokoak_i_karratu ! ez dugu erabiliko

contains
subroutine lortu_islapen_karratu(pos,ui)

  real(kind=dp),intent(inout),dimension(3)         :: pos
  real(kind=dp),intent(inout),dimension(3)         :: ui

  if ( (pos(1)<x0).or.(pos(1)>(luzera_x0+x0)) ) then
    pos   = pos - ui*delta_t
    ui(1) = -ui(1)
    pos   = pos + ui*delta_t
  endif
  if ( (pos(2)<y0).or.(pos(2)>(luzera_y0+y0)) ) then ! bi if jarri ditut elseif ordez porseakaso justo eskinatik pasatzen bada partikula bat
    pos   = pos - ui*delta_t
    ui(2) = -ui(2)
    pos   = pos + ui*delta_t
  endif

endsubroutine lortu_islapen_karratu
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_ez_itsatsi_karratu(pos,ui)

  real(kind=dp),intent(inout),dimension(3)         :: pos
  real(kind=dp),intent(inout),dimension(3)         :: ui

  if ( (pos(1)<x0).or.(pos(1)>(luzera_x0+x0)) ) then
    pos   = pos - ui*delta_t
    ui(1) = 0.0_dp
    pos   = pos + ui*delta_t
  endif
  if ( (pos(2)<y0).or.(pos(2)>(luzera_y0+y0)) ) then ! bi if jarri ditut elseif ordez porseakaso justo eskinatik pasatzen bada partikula bat
    pos   = pos - ui*delta_t
    ui(2) = 0.0_dp
    pos   = pos + ui*delta_t
  endif

endsubroutine lortu_ez_itsatsi_karratu
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------

subroutine lortu_muga_indize_karratu(partikulak,muga_indizeak)

  real(kind=dp),intent(in),dimension(:,:)         :: partikulak
  integer,intent(out),dimension(:)                :: muga_indizeak  ! kanpoan alokatuko dugu memoria n*4 izateko

  integer                                         :: i,kontadore
  
  kontadore=1
  muga_indizeak(:)=0
  do i=1,size(partikulak,1)
!====debug====
if (kontadore>(size(partikulak,1))) then
 write(unit=109,fmt=*) "(10 errorea) arazoa muga moduluan (42 lerroan):"  
 write(unit=109,fmt=*) "--> definitutako muga partikula kopurua baino indize gehio gorde nahi dira"
 stop  
endif
!=============
    if     (partikulak(i,1)<=x0) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    elseif (partikulak(i,2)<=y0) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    elseif (partikulak(i,1)>=(luzera_x0+x0)) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    elseif (partikulak(i,2)>=(luzera_y0+y0)) then
      muga_indizeak(kontadore)=i
      kontadore=kontadore+1
    endif
  enddo

endsubroutine lortu_muga_indize_karratu
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine lortu_mugakanpo_auzokoak_i_karratu(partikulak,indizea,auzokoak,mugakoak,mugakanpo_indizeak,mugakanpo_partikulak)

  real(kind=dp),intent(in),dimension(:,:)         :: partikulak
  integer,intent(in)                              :: indizea               ! i_ren indizea
  integer,intent(in),dimension(:)                 :: auzokoak              ! i_ren auzokoen zerrenda
  integer,intent(in),dimension(:)                 :: mugakoak              ! muga indizeen zerrenda
  integer,intent(out),dimension(:,:)              :: mugakanpo_indizeak    ! kanpoan copiatu dugun auzokoen zerrenda
  real(kind=dp),intent(out),dimension(:,:)        :: mugakanpo_partikulak  ! kanpoan copiatu dugun auzokoen zerrenda
  
  integer                                         :: i,j,k
  integer                                         :: ze_muga               ! ze mugan markatzeko iztegia:  1:x=0, 2:y=0, 3:x=luzera_x0, 4:y=luzera_y0 
  logical                                         :: ea_mugan              ! auzokoren bat muga bada true egiteko
  real(kind=dp)                                   :: di                    ! i_ren eta mugaren arteko distantzia bider bi
  real(kind=dp)                                   :: dauzo                 ! muga eta auzoko berrien artean egon behar den distantzia

  ea_mugan= .false.
  ze_muga = 0               ! 0 bada, ez dugu muga partikularik
  mugakanpo_indizeak(:,:)=0 ! zeroz beteko dugu beste programetan hutsuneak ignoratu ahal izateko
                            ! --horrela ikusteko i_ren bat kanpo auzokoak dituen
                            ! --ikusi ea mugakanpo_indizeak(i,1)==0 den

  do i=1,size(mugakoak,1) 
  do j=1,size(auzokoak,1)
    if (mugakoak(i)==auzokoak(j)) then   ! i_ren auzoko bat mugako partikula bat bada

      ea_mugan=.true.
       
      if     (partikulak(auzokoak(j),1)==x0) then
        ze_muga=1
      elseif (partikulak(i,2)==y0) then
        ze_muga=2
      elseif (partikulak(i,1)==(luzera_x0+x0)) then
        ze_muga=3
      elseif (partikulak(i,2)==(luzera_y0+y0)) then
        ze_muga=4
      endif

      exit
    endif
  enddo
    if (ea_mugan) then  !bukatu nahi dugu konprobatzen bat aurkitu bezain pronto
      exit
    endif
  enddo

  if (ea_mugan) then    !soilik sartu mugan bagaude
    k=1                 !kontadorea mugako partikulen indizerako
    do j=1,size(auzokoak,1)
      if (ze_muga==1) then
          di   =2.0_dp*partikulak(indizea,1)
          dauzo=partikulak(auzokoak(j),1)-di
          if (dauzo>di) then
            mugakanpo_indizeak(indizea,k)=auzokoak(j)  ! ez liatzeko muga kanpoko partikulen indizeak i_ren funtzioan jarriko ditugu
            mugakanpo_partikulak(auzokoak(j),1) = -dauzo
            mugakanpo_partikulak(auzokoak(j),2) = partikulak(auzokoak(j),2)
            mugakanpo_partikulak(auzokoak(j),3) = partikulak(auzokoak(j),3)
            k=k+1
          endif
      elseif (ze_muga==2) then
          di   =2.0_dp*partikulak(indizea,2)
          dauzo=partikulak(auzokoak(j),2)-di
          if (dauzo>di) then
            mugakanpo_indizeak(indizea,k)=auzokoak(j)  ! ez liatzeko muga kanpoko partikulen indizeak i_ren funtzioan jarriko ditugu
            mugakanpo_partikulak(auzokoak(j),2) = -dauzo
            mugakanpo_partikulak(auzokoak(j),1) = partikulak(auzokoak(j),1)
            mugakanpo_partikulak(auzokoak(j),3) = partikulak(auzokoak(j),3)
            k=k+1
          endif
      elseif (ze_muga==3) then
          di   =2.0_dp*abs(luzera_x0-partikulak(indizea,1))
          dauzo=luzera_x0-di-partikulak(auzokoak(j),1)
          if (dauzo>di) then
            mugakanpo_indizeak(indizea,k)=auzokoak(j)  ! ez liatzeko muga kanpoko partikulen indizeak i_ren funtzioan jarriko ditugu
            mugakanpo_partikulak(auzokoak(j),1) = luzera_x0+dauzo
            mugakanpo_partikulak(auzokoak(j),2) = partikulak(auzokoak(j),2)
            mugakanpo_partikulak(auzokoak(j),3) = partikulak(auzokoak(j),3)
            k=k+1
          endif
      elseif (ze_muga==4) then
          di   =2.0_dp*abs(luzera_y0-partikulak(indizea,2))
          dauzo=luzera_y0-di-partikulak(auzokoak(j),2)
          if (dauzo>di) then
            mugakanpo_indizeak(indizea,k)=auzokoak(j)  ! ez liatzeko muga kanpoko partikulen indizeak i_ren funtzioan jarriko ditugu
            mugakanpo_partikulak(auzokoak(j),2) = luzera_y0+dauzo
            mugakanpo_partikulak(auzokoak(j),1) = partikulak(auzokoak(j),1)
            mugakanpo_partikulak(auzokoak(j),3) = partikulak(auzokoak(j),3)
            k=k+1
          endif
      endif
    enddo
  endif

endsubroutine lortu_mugakanpo_auzokoak_i_karratu


endmodule muga
