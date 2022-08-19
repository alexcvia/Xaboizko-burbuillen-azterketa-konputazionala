module auzokoak
use tipos
use parametroak

public :: i_ren_auzokoak           
public :: zati_sarea_sortu !-> simulazio denbora pausu bakoitzean behin soilik deitu behar

contains
subroutine i_ren_auzokoak(p_guztiak,zs_hiztegia,indizea,auzokoen_indizeak)

  real(kind=dp),intent(in),dimension(:,:)      :: p_guztiak  !partikula guztien zerrenda, "p" "partikula" ordez ezberdintzeko bi subrutinak
  integer,intent(in),dimension(:,:,:,:)        :: zs_hiztegia ! zati_sarea_sortu()-ren outputa izena aldatuta
  integer,intent(in)                           :: indizea
  integer,intent(out),dimension(:),allocatable :: auzokoen_indizeak

  real(kind=dp)                                :: i_x,i_y,i_z,j_x,j_y,j_z,distantzia_ij
  integer                                      :: i,j,k,npartikula,kontadore,n_auzo,xerrore,yerrore,zerrore
  integer,dimension(3)                         :: i_ren_sare_koord
  integer,dimension(27,3)                      :: i_ren_27_auzo_zatiak
  integer,dimension(:),allocatable             :: momentuz_auzokoen_indizeak,momentuz_auzokoen_indizeak2


  ! ea ze sare zatian i partikula dagoen bilatu

  i_ren_sare_koord(1:3)=ceiling( p_guztiak(indizea,1:3)/R_auzo )


  ! 27 zatitxoen indizeak lortu
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
  
!  npartikula=size(p_guztiak,1)
!  allocate(momentuz_auzokoen_indizeak(npartikula))
!  kontadore=1
!  do i=1,npartikula
!  do j=1,27
!    if (p_sare_indizeekin(i,1)==i_ren_27_auzo_zatiak(j,1).and.&
!        p_sare_indizeekin(i,2)==i_ren_27_auzo_zatiak(j,2).and.&
!        p_sare_indizeekin(i,3)==i_ren_27_auzo_zatiak(j,3)     &
!    ) then
!      momentuz_auzokoen_indizeak(kontadore)=i
!      kontadore=kontadore+1
!    endif
!  enddo
!  enddo

  ! 27 zatitxoetan bilatu benetan r<R_auzo diren puntuak (gogoratu i=gure partikula eta j=auzokoak)
  i_x=p_guztiak(indizea,1)  ! i partikularen koord
  i_y=p_guztiak(indizea,2)
  i_z=p_guztiak(indizea,3)
  npartikula=size(p_guztiak,1)
  npartikulaZatiko=size(zs_hiztegia,4)
  allocate(momentuz_auzokoen_indizeak(npartikula))
  n_auzo=1
!write(unit=42,fmt=*)
!write(unit=42,fmt=*)
  do i=1,27
    do j=1,npartikulaZatiko 
!write(unit=42,fmt=*) "...",zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),"..."
!      read(unit=j_x,iostat=xerrore) p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),1)
!      read(unit=j_y,iostat=yerrore) p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),2)
!      read(unit=j_z,iostat=zerrore) p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),3)
!      if (xerrore>0.and.yerrore>0.and.zerrore>0) then  ! begiratu ea hutsik dagoen erabiltzen sahiatu baino lehen
      if (zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j)/=0) then        !soilik hutsik ez badago egin kalkuloak
        j_x=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),1)  ! j partikularen x koord
        j_y=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),2)  ! j partikularen y koord
        j_z=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),3)  ! j partikularen z koord
!write(unit=42,fmt=*) j_x,j_y,j_z
        distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> distantzia kartesiarretan konprobatu nahi dugu, ez sistema lokalean
        if (distantzia_ij<R_auzo) then
          momentuz_auzokoen_indizeak(n_auzo)=zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j)
          n_auzo=n_auzo+1
        endif
      else
        cycle
      endif
    enddo
  enddo

!  i_x=p_guztiak(indizea,1)
!  i_y=p_guztiak(indizea,2)
!  i_z=p_guztiak(indizea,3)
!  allocate(momentuz_auzokoen_indizeak2(kontadore))
!  n_auzo=1
!  do i=1,kontadore
!    j_x=p_guztiak(momentuz_auzokoen_indizeak(i),1)
!    j_y=p_guztiak(momentuz_auzokoen_indizeak(i),2)
!    j_z=p_guztiak(momentuz_auzokoen_indizeak(i),3)
!    distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> distantzia kartesiarretan konprobatu nahi dugu, ez sistema lokalean
!    if (distantzia_ij<R_auzo) then
!      momentuz_auzokoen_indizeak2(n_auzo)=momentuz_auzokoen_indizeak(i)
!      n_auzo=n_auzo+1
!    endif
!  enddo

  ! gorde auzokoen indizeak
  allocate(auzokoen_indizeak(n_auzo-1)) ! ken 1 egiten dut ze n_auzo +1 extra bat dauka +1 array-an idatzei ondoren egiten baita
  do i=1,n_auzo-1
    auzokoen_indizeak(i)=momentuz_auzokoen_indizeak(i)
  enddo

endsubroutine i_ren_auzokoak
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
subroutine zati_sarea_sortu(partikula_guztiak,zati_sare_hiztegia)

  real(kind=dp),intent(in),dimension(:,:)           :: partikula_guztiak
  integer,dimension(:,:),allocatable                :: partikulak_sare_indizeekin
  integer                                           :: i,j,k,l,npartikula

  integer,dimension(:,:),allocatable                :: momentuz_sare_zati_bilduma, sare_zati_bilduma
  integer                                           :: zatiKopurua
  logical                                           :: eaDago

  integer,dimension(:,:,:,:),allocatable            :: momentuz_zati_sare_hiztegia
  integer,intent(out),dimension(:,:,:,:),allocatable:: zati_sare_hiztegia
  integer,dimension(:,:,:),allocatable              :: hiztegiIndizeKontadore
  integer                                           :: erroZatiKop,erroPartikulaKop !array tamaina hain handia ez izateko mugak
  integer                                           :: xhiztegi, yhiztegi, zhiztegi
  integer                                           :: indizeMax, xsareKoordMax, ysareKoordMax, zsareKoordMax
  integer                                           ::            xsareKoordMin, ysareKoordMin, zsareKoordMin
!  integer                                           ::            xsareKoordDif, ysareKoordDif, zsareKoordDif
  

  npartikula=size(partikula_guztiak,1)
  allocate(partikulak_sare_indizeekin(npartikula,3))

  do i=1,npartikula   ! --> partikula guztiei sarearen zer zatitan dauden gorde
  do j=1,3  
    partikulak_sare_indizeekin(i,j)=ceiling( partikula_guztiak(i,j)/R_auzo ) 
    if (partikulak_sare_indizeekin(i,j)==0) then   ! ---> KONTUZ hau balio du zenbaki positiboak direlako, bestela -0.zerbait guztiak 1 bezala jartzen ari zara
      partikulak_sare_indizeekin(i,j)=1
    endif
!write(unit=42,fmt=*) ".",partikulak_sare_indizeekin(i,j),"."
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
!write(unit=42,fmt=*) zatiKopurua
      momentuz_sare_zati_bilduma(zatiKopurua,1:3)=partikulak_sare_indizeekin(i,1:3)
    endif 
    eaDago=.false.
  enddo
!write(unit=42,fmt=*) ".",zatiKopurua,"."
       
  allocate(sare_zati_bilduma(zatiKopurua,3))
  do i=1,zatiKopurua
    sare_zati_bilduma(i,1:3)= momentuz_sare_zati_bilduma(i,1:3)
!write(unit=42,fmt=*) "...",i,sare_zati_bilduma(i,1:3),"..."
  enddo

!---

!  allocate(momentuz_zati_sare_hiztegia(100,100,100,1000))
  erroZatiKop     =floor(sqrt(real(zatiKopurua)))
  erroPartikulaKop=floor(sqrt(real(npartikula)))
!====debug====
if (erroZatiKop**3*erroPartikulaKop>10**9) then
 write(unit=109,fmt=*) "errorea auzokoak moduluan (209 lerroan):"  
 write(unit=109,fmt=*) "--> memoria gehiegi eskatu da sare hiztegirako"
 stop  
endif
!====-----====

write(unit=42,fmt=*) ".","hmmmmmmmm","."
  allocate(momentuz_zati_sare_hiztegia(erroZatiKop,erroZatiKop,erroZatiKop,erroPartikulaKop))
  allocate(hiztegiIndizeKontadore(erroZatiKop,erroZatiKop,erroZatiKop))
  hiztegiIndizeKontadore(:,:,:)=1
  xsareKoordMax=maxval(sare_zati_bilduma(:,1))      !lortu zenbat sare zati x indize positibo dauden
  ysareKoordMax=maxval(sare_zati_bilduma(:,2))      ! "                     y
  zsareKoordMax=maxval(sare_zati_bilduma(:,3))      ! "                     z
write(unit=42,fmt=*) ".","hmmmmmmmm","."
  
  do i=1,npartikula
    do j=1,zatiKopurua 
      if (partikulak_sare_indizeekin(i,1)==sare_zati_bilduma(j,1).and.&
          partikulak_sare_indizeekin(i,2)==sare_zati_bilduma(j,2).and.&
          partikulak_sare_indizeekin(i,3)==sare_zati_bilduma(j,3)     &
      ) then
        if (sare_zati_bilduma(j,1)<0) then
          xhiztegi = xsareKoordMax+abs(sare_zati_bilduma(j,1))  !hiztegiIndizea negatiboentzat max+1->max+abs(min) joango da, ezin baita minus izan
        else
          xhiztegi = sare_zati_bilduma(j,1)                     !hiztegiIndizea positiboentzat 1->max joango da
        endif
        if (sare_zati_bilduma(j,2)<0) then
          yhiztegi = ysareKoordMax+abs(sare_zati_bilduma(j,2))
        else
          yhiztegi = sare_zati_bilduma(j,2)
        endif
        if (sare_zati_bilduma(j,3)<0) then
          zhiztegi = xsareKoordMax+abs(sare_zati_bilduma(j,3))
        else
          zhiztegi = sare_zati_bilduma(j,3)
        endif
        momentuz_zati_sare_hiztegia(xhiztegi,yhiztegi,zhiztegi,&
                                    hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)) = i  !gorde sare zati koordenatu honetan zein partikula dauden
        hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi) = &
                                    hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)  + 1  !sare zati koord hauek berriro agertzean hurrengo array indizean gorde
!====debug====
if (hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)>erroPartikulaKop-1) then
 write(unit=109,fmt=*) "errorea auzokoak moduluan (225 lerroan):"  
 write(unit=109,fmt=*) "--> memoria maximoa baino indize gehio gordetzen sahiatu da"
 stop  
endif
!====-----====

!write(unit=42,fmt=*) ".....",xhiztegi,yhiztegi,zhiztegi,hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi),"....."
        exit
      endif
    enddo 
  enddo

  indizeMax=maxval(hiztegiIndizeKontadore(:,:,:))-1 !lortu zer zati sare daukan ptukopuru handiena (-1 azken bueltan +1 extra egiten baitu)
  xsareKoordMin=minval(sare_zati_bilduma(:,1))      !lortu sare zati x indize minimoa, negatiboak daudenean hauek geitu behar baitira ere
  ysareKoordMin=minval(sare_zati_bilduma(:,2))      ! "                     y 
  zsareKoordMin=minval(sare_zati_bilduma(:,3))      ! "                     z
!  xsareKoordDif=abs(xsareKoordMax-xsareKoordMin)    !max eta min arteko diferentzia
!  ysareKoordDif=abs(ysareKoordMax-ysareKoordMin)    ! "
!  zsareKoordDif=abs(zsareKoordMax-zsareKoordMin)    ! "
write(unit=42,fmt=*) ".",indizeMax,"."
write(unit=42,fmt=*) ".",xsareKoordMax,"."
write(unit=42,fmt=*) ".",ysareKoordMax,"."
write(unit=42,fmt=*) ".",zsareKoordMax,"."
!write(unit=42,fmt=*) ".",xsareKoordMin,"."
!write(unit=42,fmt=*) ".",ysareKoordMin,"."
!write(unit=42,fmt=*) ".",zsareKoordMin,"."
  if ()
  allocate(zati_sare_hiztegia(xsareKoordMax+xsareKoordMin,ysareKoordMax+ysareKoordMin,zsareKoordMax+zsareKoordMin,indizeMax))
!  zati_sare_hiztegia(:,:,:,:)=0  !bete guztia 0-ekin horrela 0 bat badago hutsik dagoela kontsidera dezakegu
  do i=1,xsareKoordMax+xsareKoordMin
  do j=1,ysareKoordMax+ysareKoordMin
  do k=1,zsareKoordMax+zsareKoordMin
    do l=1,indizeMax
      zati_sare_hiztegia(i,j,k,l)=momentuz_zati_sare_hiztegia(i,j,k,l)
!write(unit=42,fmt=*) "¬¬",i,j,k,"¬¬",zati_sare_hiztegia(i,j,k,l),"¬¬"
    enddo
  enddo
  enddo
  enddo

!---

deallocate(partikulak_sare_indizeekin)
deallocate(momentuz_sare_zati_bilduma)
deallocate(sare_zati_bilduma)
deallocate(momentuz_zati_sare_hiztegia)
deallocate(hiztegiIndizeKontadore)


endsubroutine zati_sarea_sortu
endmodule auzokoak
