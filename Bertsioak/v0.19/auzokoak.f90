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
  integer,dimension(:),allocatable             :: momentuz_auzokoen_indizeak !,momentuz_auzokoen_indizeak2
  integer                                      :: xzsKoordMax, yzsKoordMax, zzsKoordMax


  npartikula=size(p_guztiak,1)
!====debug====
if (indizea>npartikula) then
 write(unit=109,fmt=*) "(4 errorea) arazoa auzokoak moduluan (9 lerroan):"  
 write(unit=109,fmt=*) "--> jasotako indizea partikula kopurua baino handiagoa da"
 stop  
endif
!====-----====

  ! ea ze sare zatian i partikula dagoen bilatu

  i_ren_sare_koord(1:3)=ceiling( p_guztiak(indizea,1:3)/R_auzo )


!write(unit=42,fmt=*) "...hmmmmmmmmm..."
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
  xzsKoordMax=size(zs_hiztegia,1)  !zati sarearen limiteak
  yzsKoordMax=size(zs_hiztegia,2)
  zzsKoordMax=size(zs_hiztegia,3)
  npartikulaZatiko=size(zs_hiztegia,4)
  allocate(momentuz_auzokoen_indizeak(npartikula))
!write(unit=42,fmt=*) "...hmmmmmmmmm..."
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

      if ((i_ren_27_auzo_zatiak(i,1)>0.and.i_ren_27_auzo_zatiak(i,1)<=xzsKoordMax).and.&   ! 27 zatitxoetatik gure saretik kanpo dauden ikusi, bestela segitu bilatzen
          (i_ren_27_auzo_zatiak(i,2)>0.and.i_ren_27_auzo_zatiak(i,2)<=yzsKoordMax).and.&
          (i_ren_27_auzo_zatiak(i,3)>0.and.i_ren_27_auzo_zatiak(i,3)<=zzsKoordMax)     &
         ) then

        if (zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j)/=0) then        !soilik hutsik ez badago egin kalkuloak
          j_x=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),1)  ! j partikularen x koord
          j_y=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),2)  ! j partikularen y koord
          j_z=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),3)  ! j partikularen z koord
!write(unit=42,fmt=*) j_x,j_y,j_z
!write(unit=42,fmt=*) "...pururu...",i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3)
          distantzia_ij=sqrt( (i_x-j_x)**2+(i_y-j_y)**2+(i_z-j_z)**2 ) !--> distantzia kartesiarretan konprobatu nahi dugu, ez sistema lokalean
          if (distantzia_ij<R_auzo) then
            momentuz_auzokoen_indizeak(n_auzo)=zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j)
            n_auzo=n_auzo+1
          endif!3.if-a
        else
          cycle
        endif  !2.if-a

      else
        cycle
      endif    !1.if-a

    enddo
  enddo
!write(unit=42,fmt=*) "...hmmmmmmmmm..."

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
!write(unit=42,fmt=*) "...hmmmmmmmmm..."

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
  

  npartikula=size(partikula_guztiak,1)
  allocate(partikulak_sare_indizeekin(npartikula,3))

  do i=1,npartikula   ! --> partikula guztiei sarearen zer zatitan dauden gorde
  do j=1,3  
    partikulak_sare_indizeekin(i,j)=ceiling( partikula_guztiak(i,j)/R_auzo ) 
    if (partikulak_sare_indizeekin(i,j)==0) then   ! ---> KONTUZ hau balio du zenbaki positiboak direlako, bestela -0.zerbait guztiak 1 bezala jartzen ari zara
      partikulak_sare_indizeekin(i,j)=1            !      berdin duena lehen kuadrantean soilik lanean baitgaude
    endif
!====debug====
if (partikula_guztiak(i,j)<0) then
 write(unit=109,fmt=*) "(3 errorea) arazoa auzokoak moduluan (133 lerroan):"  
 write(unit=109,fmt=*) "--> jasotako puntu hodeiak zenbaki negatiboak ditu"
 stop  
endif
!====-----====
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
      momentuz_sare_zati_bilduma(zatiKopurua,1:3)=partikulak_sare_indizeekin(i,1:3)
    endif 
    eaDago=.false.
  enddo
       
  allocate(sare_zati_bilduma(zatiKopurua,3))
  do i=1,zatiKopurua
    sare_zati_bilduma(i,1:3)= momentuz_sare_zati_bilduma(i,1:3)
  enddo

!---

  erroZatiKop     =floor(sqrt(real(zatiKopurua)))
  erroPartikulaKop=floor(sqrt(real(npartikula)))
!====debug====
if (erroZatiKop**3*erroPartikulaKop>10**9) then
 write(unit=109,fmt=*) "(1 errorea) arazoa auzokoak moduluan (211 lerroan):"  
 write(unit=109,fmt=*) "--> memoria gehiegi eskatu da sare hiztegirako"
 stop  
endif
!====-----====
  allocate(momentuz_zati_sare_hiztegia(erroZatiKop,erroZatiKop,erroZatiKop,erroPartikulaKop))
  allocate(hiztegiIndizeKontadore(erroZatiKop,erroZatiKop,erroZatiKop))
  hiztegiIndizeKontadore(:,:,:)=1
  momentuz_zati_sare_hiztegia(:,:,:,:)=0  !bete guztia 0-ekin horrela 0 bat badago hutsik dagoela kontsidera dezakegu
  
  do i=1,npartikula
    do j=1,zatiKopurua 
      if (partikulak_sare_indizeekin(i,1)==sare_zati_bilduma(j,1).and.&
          partikulak_sare_indizeekin(i,2)==sare_zati_bilduma(j,2).and.&
          partikulak_sare_indizeekin(i,3)==sare_zati_bilduma(j,3)     &
      ) then
        xhiztegi = sare_zati_bilduma(j,1)
        yhiztegi = sare_zati_bilduma(j,2)
        zhiztegi = sare_zati_bilduma(j,3)
        momentuz_zati_sare_hiztegia(xhiztegi,yhiztegi,zhiztegi,&
                                    hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)) = i  !gorde sare zati koordenatu honetan zein partikula dauden
        hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi) = &
                                    hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)  + 1  !sare zati koord hauek berriro agertzean hurrengo array indizean gorde
!====debug====
if (hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)>erroPartikulaKop+1) then  ! +1 egin zeren +1 extra bat dago kontadorean
 write(unit=109,fmt=*) "(2 errorea) arazoa auzokoak moduluan (225 lerroan):"  
 write(unit=109,fmt=*) "--> memoria maximoa baino indize gehio gordetzen sahiatu da"
 stop  
endif
!====-----====

        exit
      endif
    enddo 
  enddo

  indizeMax=maxval(hiztegiIndizeKontadore(:,:,:))-1 !lortu zer zati sare daukan ptukopuru handiena (-1 azken bueltan +1 extra egiten baitu)
  xsareKoordMax=maxval(sare_zati_bilduma(:,1))      !lortu zenbat sare zati x indize dauden
  ysareKoordMax=maxval(sare_zati_bilduma(:,2))      ! "                     y
  zsareKoordMax=maxval(sare_zati_bilduma(:,3))      ! "                     z
  allocate(zati_sare_hiztegia(xsareKoordMax,ysareKoordMax,zsareKoordMax,indizeMax))
  do i=1,xsareKoordMax
  do j=1,ysareKoordMax
  do k=1,zsareKoordMax
    do l=1,indizeMax
      zati_sare_hiztegia(i,j,k,l)=momentuz_zati_sare_hiztegia(i,j,k,l)
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
