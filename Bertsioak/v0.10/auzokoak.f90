module auzokoak
use tipos
use parametroak

public :: i_ren_auzokoak           
public :: zati_sarea_sortu !-> simulazio denbora pausu bakoitzean behin soilik deitu behar

contains
subroutine i_ren_auzokoak(p_guztiak,p_sare_indizeekin,indizea,auzokoen_indizeak)

  real(kind=dp),intent(in),dimension(:,:)      :: p_guztiak  !partikula guztien zerrenda, "p" "partikula" ordez ezberdintzeko bi subrutinak
  integer,intent(in),dimension(:,:)            :: p_sare_indizeekin ! zati_sarea_sortu()-ren outputa izena aldatuta
  integer,intent(in)                           :: indizea
  integer,intent(out),dimension(:),allocatable :: auzokoen_indizeak

  real(kind=dp)                                :: i_x,i_y,i_z,j_x,j_y,j_z,distantzia_ij
  integer                                      :: i,j,k,npartikula,kontadore,n_auzo
  integer,dimension(3)                         :: i_ren_sare_koord
  integer,dimension(27,3)                      :: i_ren_27_auzo_zatiak
  integer,dimension(:),allocatable             :: momentuz_auzokoen_indizeak,momentuz_auzokoen_indizeak2


  ! ea ze sare zatian i partikula dagoen bilatu

  i_ren_sare_koord(1)=ceiling( p_guztiak(indizea,1)/R_auzo )
  i_ren_sare_koord(2)=ceiling( p_guztiak(indizea,2)/R_auzo )
  i_ren_sare_koord(3)=ceiling( p_guztiak(indizea,3)/R_auzo )


  ! 27 zatitxoetan dauden partikulak lortu
  i_ren_27_auzo_zatiak(1 ,1:3)=i_ren_sare_koord(1:3)
  i_ren_27_auzo_zatiak(2 ,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(3 ,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(4 ,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(5 ,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(6 ,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(7 ,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(8 ,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)  /)
  i_ren_27_auzo_zatiak(9 ,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)  /)

  i_ren_27_auzo_zatiak(10,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(11,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(12,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(13,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(14,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(15,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(16,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(17,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)-1/)
  i_ren_27_auzo_zatiak(18,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)-1/)

  i_ren_27_auzo_zatiak(19,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(20,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(21,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)  ,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(22,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(23,1:3)=(/i_ren_sare_koord(1)  ,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(24,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(25,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(26,1:3)=(/i_ren_sare_koord(1)-1,i_ren_sare_koord(2)+1,i_ren_sare_koord(3)+1/)
  i_ren_27_auzo_zatiak(27,1:3)=(/i_ren_sare_koord(1)+1,i_ren_sare_koord(2)-1,i_ren_sare_koord(3)+1/)
  
  npartikula=size(p_guztiak,1)
  allocate(momentuz_auzokoen_indizeak(npartikula))
!  call zati_sarea_sortu(partikula_guztiak,partikulak_sare_indizeekin)
  kontadore=1
  do i=1,npartikula
  do j=1,27
    if (p_sare_indizeekin(i,1)==i_ren_27_auzo_zatiak(j,1).and.&
        p_sare_indizeekin(i,2)==i_ren_27_auzo_zatiak(j,2).and.&
        p_sare_indizeekin(i,3)==i_ren_27_auzo_zatiak(j,3)     &
    ) then
      momentuz_auzokoen_indizeak(kontadore)=i
      kontadore=kontadore+1
    endif
  enddo
  enddo

  ! 27 zatitxoetan bilatu benetan r<R_auzo diren puntuak (gogoratu i=gure partikula eta j=auzokoak)
  i_x=p_guztiak(indizea,1)
  i_y=p_guztiak(indizea,2)
  i_z=p_guztiak(indizea,3)
  allocate(momentuz_auzokoen_indizeak2(kontadore))
  n_auzo=1
  do i=1,kontadore
    j_x=p_guztiak(momentuz_auzokoen_indizeak(i),1)
    j_y=p_guztiak(momentuz_auzokoen_indizeak(i),2)
    j_z=p_guztiak(momentuz_auzokoen_indizeak(i),3)
    distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> distantzia kartesiarretan konprobatu nahi dugu, ez sistema lokalean
    if (distantzia_ij<R_auzo) then
      momentuz_auzokoen_indizeak2(n_auzo)=momentuz_auzokoen_indizeak(i)
      n_auzo=n_auzo+1
    endif
  enddo

  ! gorde auzokoen indizeak
  allocate(auzokoen_indizeak(n_auzo-1)) ! ken 1 egiten dut ze n_auzo +1 extra bat dauka +1 array-an idatzei ondoren egiten baita
  do i=1,n_auzo
    auzokoen_indizeak(i)=momentuz_auzokoen_indizeak2(i)
  enddo

endsubroutine i_ren_auzokoak
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine zati_sarea_sortu(partikula_guztiak,partikulak_sare_indizeekin,sare_zati_bilduma)

  real(kind=dp),intent(in),dimension(:,:)       :: partikula_guztiak
  integer,intent(out),dimension(:,:),allocatable:: partikulak_sare_indizeekin,sare_zati_bilduma
  integer,dimension(:,:),allocatable            :: momentuz_sare_zati_bilduma
  integer                                       :: i,j,k,npartikula,zatiKopurua
  logical                                       :: eaDago
  

  npartikula=size(partikula_guztiak,1)
  allocate(partikulak_sare_indizeekin(npartikula,3))

  do i=1,npartikula   ! --> partikula guztiei sarearen zer zatitan dauden gorde
  do j=1,3  
    partikulak_sare_indizeekin(i,j)=ceiling( partikula_guztiak(i,j)/R_auzo ) 
    if (partikulak_sare_indizeekin(i,j)==0) then   ! ---> KONTUZ hau balio du zenbaki positiboak direlako, bestela -0.zerbait guztiak 1 bezala jartzen ari zara
      partikulak_sare_indizeekin(i,j)=1
    endif
  enddo
  enddo

!---

  allocate(momentuz_sare_zati_bilduma(npartikula,3))
  zatiKopurua=1
  eaDago=.false.
  momentuz_sare_zati_bilduma(1,1:3)=partikulak_sare_indizeekin(1,1:3) !lehenengoa gordeko dugu ya zatiKoputua 0-tik ez hasteko

  do i=2,npartikula
    do j=1,zatiKopurua !do-ren tamaina handitzen joango da gordetako zati ezberdin kopurua handitzean
      if (partikulak_sare_indizeekin(i,1)==momentuz_sare_zati_bilduma(j,1).and.&
          partikulak_sare_indizeekin(i,2)==momentuz_sare_zati_bilduma(j,2).and.&
          partikulak_sare_indizeekin(i,3)==momentuz_sare_zati_bilduma(j,3)     &
      ) then
        eaDago=.true.
        exit
      endif
    enddo

    if (eaDago==.false.) then
      zatiKopurua=zatiKopurua+1
      write(unit=42,fmt=*) zatiKopurua
      momentuz_sare_zati_bilduma(zatiKopurua,1:3)=partikulak_sare_indizeekin(i,1:3)
    endif 
    eaDago=.false.
  enddo
      
  allocate(sare_zati_bilduma(zatiKopurua,3))
  do i=1,zatiKopurua
    sare_zati_bilduma(i,1:3)= momentuz_sare_zati_bilduma(i,1:3)
  enddo

!---

  allocate(momentuz_zati_sare_hiztegia(zatiKopurua,npartikula))
  allocate(hiztegiIndizeKontadore(npartikula))
  hiztegiIndizeKontadore(:)=1

  do i=1,npartikula
    do j=1,zatiKopurua !do-ren tamaina handitzen joango da gordetako zati ezberdin kopurua handitzean
      if (partikulak_sare_indizeekin(i,1)==sare_zati_bilduma(j,1).and.&
          partikulak_sare_indizeekin(i,2)==sare_zati_bilduma(j,2).and.&
          partikulak_sare_indizeekin(i,3)==sare_zati_bilduma(j,3)     &
      ) then
        momentuz_zati_sare_hiztegia(j,hiztegiIndizeKontadore(j))=i
        hiztegiIndizeKontadore(j)=hiztegiIndizeKontadore(j)+1
        exit
      endif
    enddo 
  enddo

  indizeMax=max(hiztegiIndizeKontadore(:))
  allocate(zati_sare_hiztegia(zatiKopurua,indizeMax))
  do i=1,zatiKopurua
    do j=1,indizeMax
      zati_sare_hiztegia(i,j)=momentuz_zati_sare_hiztegia(i,j)
    enddo
  enddo


endsubroutine zati_sarea_sortu
endmodule auzokoak
