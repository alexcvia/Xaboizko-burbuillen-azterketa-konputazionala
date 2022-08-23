module auzokoak
use tipos
use parametroak
implicit none

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
  integer,dimension(:),allocatable             :: momentuz_auzokoen_indizeak
  integer                                      :: xzsKoordMax, yzsKoordMax, zzsKoordMax, npartikulaZatiko

  real(kind=dp)                                :: xmin,ymin,zmin  !0-tik asten ez bada, desplazatzeko
  integer                                      :: indize_xmin,indize_ymin,indize_zmin

  npartikula=size(p_guztiak,1)
!====debug====
if (indizea>npartikula) then
 write(unit=109,fmt=*) "(4 errorea) arazoa auzokoak moduluan (9 lerroan):"  
 write(unit=109,fmt=*) "--> jasotako indizea partikula kopurua baino handiagoa da"
 stop  
endif
!====-----====

  ! ea ze sare zatian i partikula dagoen bilatu

  xmin=minval(p_guztiak(:,1))
  indize_xmin=ceiling(xmin/R_auzo)
  ymin=minval(p_guztiak(:,2))
  indize_ymin=ceiling(ymin/R_auzo)
  zmin=minval(p_guztiak(:,3))
  indize_zmin=ceiling(zmin/R_auzo)

  i_ren_sare_koord(1:3)=ceiling( p_guztiak(indizea,1:3)/R_auzo )

  i_ren_sare_koord(1)=i_ren_sare_koord(1)-indize_xmin+1  ! horrela xmin izango da indize=1 balioa eta hortik asiko da kontatzen
  i_ren_sare_koord(2)=i_ren_sare_koord(2)-indize_ymin+1  ! --hau egin behar zati_sarea-n horrela berdefinitugulako indizeak
  i_ren_sare_koord(3)=i_ren_sare_koord(3)-indize_zmin+1  ! --horrela edozein koordenatuetan zentratu dezakegularik punto hodeia


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
  

  ! 27 zatitxoetan bilatu benetan r<R_auzo diren puntuak (gogoratu i=gure partikula eta j=auzokoak)
  i_x=p_guztiak(indizea,1)  ! i partikularen koord
  i_y=p_guztiak(indizea,2)
  i_z=p_guztiak(indizea,3)
  xzsKoordMax=size(zs_hiztegia,1)  !zati sarearen limiteak
  yzsKoordMax=size(zs_hiztegia,2)
  zzsKoordMax=size(zs_hiztegia,3)
  npartikulaZatiko=size(zs_hiztegia,4)
  allocate(momentuz_auzokoen_indizeak(npartikula))
  n_auzo=1
  do i=1,27
    do j=1,npartikulaZatiko 

      if ((i_ren_27_auzo_zatiak(i,1)>0.and.i_ren_27_auzo_zatiak(i,1)<=xzsKoordMax).and.&   ! 27 zatitxoetatik gure saretik kanpo dauden ikusi, bestela segitu bilatzen
          (i_ren_27_auzo_zatiak(i,2)>0.and.i_ren_27_auzo_zatiak(i,2)<=yzsKoordMax).and.&
          (i_ren_27_auzo_zatiak(i,3)>0.and.i_ren_27_auzo_zatiak(i,3)<=zzsKoordMax)     &
         ) then

        if (zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j)/=0) then        !soilik hutsik ez badago egin kalkuloak
          j_x=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),1)  ! j partikularen x koord
          j_y=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),2)  ! j partikularen y koord
          j_z=p_guztiak(zs_hiztegia(i_ren_27_auzo_zatiak(i,1),i_ren_27_auzo_zatiak(i,2),i_ren_27_auzo_zatiak(i,3),j),3)  ! j partikularen z koord
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

  ! gorde auzokoen indizeak
  allocate(auzokoen_indizeak(n_auzo-2)) ! ken 1 egiten dut ze n_auzo +1 extra bat dauka +1 array-an idatzei ondoren egiten baita
                                        ! ken beste 1 zeren auzokoen zerrendan i partikula ez dugu nahi   
  k=1                                     
  do i=1,n_auzo-1 !emen soilik -1 ze bai nahi dugu i partikula ere kontatu zerrendatik kendu ahal izateko
    if (momentuz_auzokoen_indizeak(i)==indizea)then
      cycle
    endif
    auzokoen_indizeak(k)=momentuz_auzokoen_indizeak(i)
    k=k+1
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

  real(kind=dp)                                     :: xmin,ymin,zmin  !0-tik asten ez bada, desplazatzeko
  integer                                           :: indize_xmin,indize_ymin,indize_zmin

  npartikula=size(partikula_guztiak,1)
  allocate(partikulak_sare_indizeekin(npartikula,3))

  xmin=minval(partikula_guztiak(:,1))
  indize_xmin=ceiling(xmin/R_auzo)
  ymin=minval(partikula_guztiak(:,2))
  indize_ymin=ceiling(ymin/R_auzo)
  zmin=minval(partikula_guztiak(:,3))
  indize_zmin=ceiling(zmin/R_auzo)

  do i=1,npartikula   ! --> partikula guztiei sarearen zer zatitan dauden gorde
  do j=1,3  
    partikulak_sare_indizeekin(i,j)=ceiling( partikula_guztiak(i,j)/R_auzo )

    if     (j==1) then
      partikulak_sare_indizeekin(i,j)=partikulak_sare_indizeekin(i,j)-indize_xmin+1 !horrela xmin izango da indize=1 balioa eta hortik asiko da kontatzen
    elseif (j==2) then
      partikulak_sare_indizeekin(i,j)=partikulak_sare_indizeekin(i,j)-indize_ymin+1 !horrela ymin izango da indize=1 balioa eta hortik asiko da kontatzen
    elseif (j==3) then
      partikulak_sare_indizeekin(i,j)=partikulak_sare_indizeekin(i,j)-indize_zmin+1 !horrela zmin izango da indize=1 balioa eta hortik asiko da kontatzen
    endif

    if (partikulak_sare_indizeekin(i,j)==0) then   ! ---> KONTUZ hau balio du zenbaki positiboak direlako, bestela -0.zerbait guztiak 1 bezala jartzen ari zara
      partikulak_sare_indizeekin(i,j)=1            !      berdin duena lehen kuadrantean soilik lanean baitgaude
    endif
!====debug====
if (partikula_guztiak(i,j)<0) then
 write(unit=109,fmt=*) "(3 errorea) arazoa auzokoak moduluan (170 lerroan):"  
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

!====debug====
if (zatiKopurua>=npartikula) then
 write(unit=109,fmt=*) "(5 errorea) arazoa auzokoak moduluan (218 lerroan):"  
 write(unit=109,fmt=*) "--> zatiKopurua>=npartikula, R_auzoko txikiagoa da partikulen arteko distantzia baino"
 stop  
endif
!====-----====

!---

  erroZatiKop      =floor(sqrt(real(zatiKopurua)))        ! estimatu memoria kopuru onargarri bat
  erroPartikulaKop =floor(sqrt(real(npartikula)))
  erroZatiKop      =max(erroZatiKop,mMin_iKoord)          ! parametroetan jarritako memoria minimoa alokatu nahi dugu gutxienez
  erroPartikulaKop =max(erroPartikulaKop,mMin_iPartikula) ! beraz gutxienez balio hori izateko, bien maximoa behar
!====debug====
if (erroZatiKop**3*erroPartikulaKop>10**9) then
 write(unit=109,fmt=*) "(1 errorea) arazoa auzokoak moduluan (215 lerroan):"  
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
!====debug====
if ( (zhiztegi>erroZatiKop).or.(xhiztegi>erroZatiKop).or.(yhiztegi>erroZatiKop) ) then
 write(unit=109,fmt=*) "(8 errorea) arazoa auzokoak moduluan (255 lerroan):"  
 write(unit=109,fmt="(a,i5,a)") " --> koordenatu indize memoria maximoa (",erroZatiKop,") baino indize gehio gordetzen sahiatu da"
 if (zhiztegi>erroZatiKop) then
  write(unit=109,fmt=*) "--> errorea z koordenatu indizean"
  write(unit=109,fmt=*) "--> sartu nahi izan den indizea:",zhiztegi
 elseif (xhiztegi>erroZatiKop) then
  write(unit=109,fmt=*) "--> errorea x koordenatu indizean"
  write(unit=109,fmt=*) "--> sartu nahi izan den indizea:",xhiztegi
 else
  write(unit=109,fmt=*) "--> errorea y koordenatu indizean"
  write(unit=109,fmt=*) "--> sartu nahi izan den indizea:",yhiztegi
 endif
 stop  
endif
!====-----====
        momentuz_zati_sare_hiztegia(xhiztegi,yhiztegi,zhiztegi,&
                                    hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)) = i  !gorde sare zati koordenatu honetan zein partikula dauden

        hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi) = &
                                    hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)  + 1  !sare zati koord hauek berriro agertzean hurrengo array indizean gorde
!====debug====
if (hiztegiIndizeKontadore(xhiztegi,yhiztegi,zhiztegi)>erroPartikulaKop+1) then  ! +1 egin zeren +1 extra bat dago kontadorean
 write(unit=109,fmt=*) "(2 errorea) arazoa auzokoak moduluan (254 lerroan):"  
 write(unit=109,fmt="(a,i5,a)") " --> partikula indize memoria maximoa (",erroPartikulaKop+1,") baino indize gehio gordetzen sahiatu da"
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
