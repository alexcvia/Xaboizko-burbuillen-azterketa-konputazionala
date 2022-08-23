program probatu_prestaketa
use tipos
use parametroak
use auzokoak
use funtzioak
use mcf_diagonalizacion ! kontuz, konpilatzean matrices.f90 da honen modulu nagusia
use mcf_slineales
use lokala
use bektoreak
use prestaketa
use indarrak
use muga
implicit none

real(kind=dp)                             :: x,y,z,kte
integer                                   :: i,j,k,l,n,m,kontadore,i_partikula
integer                                   :: n_auzo,x_auzo,y_auzo,z_auzo,errore
integer                                   :: i_kopurua ! zenbat i_partikula ezberdin bilatu
integer                                   :: i_hasiera,i_biderkatzailea
real(kind=dp)                             :: random1,random2
real(kind=dp),dimension(:,:),allocatable  :: f ! puntu hodeia gordetzeko
integer,dimension(:,:,:,:),allocatable    :: f_sare
integer,dimension(:),allocatable          :: auzokoen_indizeak

real(kind=dp),dimension(3,3)              :: i_oinarria

real(kind=dp),dimension(2,2)              :: g             ! i-ren tentsore metrikoa
real(kind=dp),dimension(2,2)              :: g_1           ! g-ren alderantzizkoa
real(kind=dp)                             :: det_g         ! g-ren determinantea
real(kind=dp)                             :: det_g_1       ! g_1-ren determinantea

real(kind=dp),dimension(3)                :: Rij_gainazal  ! i eta bere auzokoen arteko posizio erlatiboa
real(kind=dp),dimension(3)                :: Uij_gainazal  ! i eta bere auzokoen arteko abiadura erlatiboa
!real(kind=dp),dimension(3)                :: ui,uj         ! i eta j-ren abiadura gordetzeko bektorea
integer,dimension(:),allocatable          :: j_auzokoen_indizeak
real(kind=dp),dimension(3,3)              :: j_oinarria

real(kind=dp), dimension(3)               :: gainazal_gradientea  ! i eta j arteko gainazal gradientea

real(kind=dp)                             :: dibergentzia_j
real(kind=dp)                             :: dibergentzia_i       ! i-ren auzokoen dibergentzia_j guztien batura
real(kind=dp)                             :: kurbadura_lokala_j   
real(kind=dp)                             :: kurbadura_lokala_i   ! i-ren auzokoen kurbadura_lokala_j guztien batura
!real(kind=dp)                             :: gainazal_tentsioa_i
!real(kind=dp)                             :: presioa_i
real(kind=dp),dimension(:),allocatable    :: h_gorde              ! hi guztiak gordetzeko
real(kind=dp),dimension(:),allocatable    :: kontzentrazioa_gorde ! kontzentrazioa gordetzeko i bakoitzarako 
real(kind=dp),dimension(:,:,:),allocatable:: Rij_gorde            ! Rij_gainazal gordetzeko, askotan behar baita (i,j,koord)        KONTUZ: hau j erabili, ez j_
real(kind=dp),dimension(:,:,:),allocatable:: ggrad_gorde          ! gainazal gradientea gordetzeko, askotan behar baita (i,j,koord) KONTUZ: hau j erabili, ez j_

real(kind=dp),dimension(3)                :: bortizitate_indarra_j
real(kind=dp),dimension(3)                :: bortizitate_indarra_i! i-ren auzokoen bortizitate_indarra_j guztien batura
real(kind=dp),dimension(3)                :: presio_indarra_j
real(kind=dp),dimension(3)                :: presio_indarra_i     ! j guztien batura
real(kind=dp),dimension(3)                :: Marangoni_indarra_j
real(kind=dp),dimension(3)                :: Marangoni_indarra_i  ! j guztien batura
real(kind=dp),dimension(3)                :: kapilare_indarra_j
real(kind=dp),dimension(3)                :: kapilare_indarra_i   ! j guztien batura
real(kind=dp),dimension(3)                :: biskositate_indarra_j
real(kind=dp),dimension(3)                :: biskositate_indarra_i! j guztien batura
real(kind=dp),dimension(3)                :: kanpo_indarra_i
real(kind=dp),dimension(:),allocatable    :: presioa_gorde        ! i bakoitzaren presioak gordetzeko
real(kind=dp),dimension(:),allocatable    :: bortizitatea_gorde   ! i bakoizaren bortizitatea gordetzeko
real(kind=dp),dimension(:),allocatable    :: tentsioa_gorde       ! i bakoizaren gainazal tentsioa gordetzeko
real(kind=dp),dimension(:,:),allocatable  :: u_gorde              ! i bakoizaren gainazal abiadura gordetzeko (i,koord)
integer                                   :: j_                   ! loop-etan auzokoen_indizeak(j) errezago idazteko

integer                                   :: t                    ! denbora iterazioa
real(kind=dp),dimension(:),allocatable    :: h_desplazatua_gorde  ! altuera desplazatuak gordetzeko koloreen irudikaketarako
real(kind=dp)                             :: kontzentrazio_kalk   ! kontzentrazioaren aldaketa kalkulatzeko aldagai laguntzailea
integer                                   :: nj_max               ! auzokoen indize memoria esleipen maximoa Rij eta ggrad-erako

integer,dimension(:),allocatable          :: muga_indizeak
real(kind=dp)                             :: x0muga,y0muga,z0muga ! HB-etan muga kanpoko partikulak marrazteko
real(kind=dp)                             :: xmuga,ymuga,zmuga    ! HB-etan muga kanpoko partikulak marrazteko
logical                                   :: ea_mugakoa           ! ez kalkulatzeko muga partikulen iterazioak

open(unit=42,action="write",status="replace",file="datuMuga.dat")   ! posizioak
open(unit=43,action="write",status="replace",file="datuMuga2.dat")  ! dibergentzia, kurbadura, gainazal tentsioa, presioa eta indarrak
open(unit=44,action="write",status="replace",file="datuMuga3.dat")  ! algoritmo 3
open(unit=45,action="write",status="replace",file="datuMuga4.dat") 
open(unit=52,action="write",status="replace",file="gnuplotKomandoak.dat")
open(unit=109,action="write",status="replace",file="erroreenLog.dat")

  
!-----------------------------------------------------
! puntu hodeia eraiki

  n=floor(sqrt(real(npartikula_0)))
  !n=100
    allocate(f(n*n,3))

    kontadore=1
    do i=1,n
      !x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
      x=muga_x0+(muga_xf-muga_x0)*(i-1)/(n-1)
      do j=1,n
        !y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
        y=muga_y0+(muga_yf-muga_y0)*(j-1)/(n-1)
        z=z0+2.0_dp*0.01_dp
        !z=(0.1*(3*sin(x*100.0_dp)+2*cos(y*100.0_dp)+4*sin((2*x+y)*100.0_dp))+2.0_dp)*0.01 !eskala aldatu xy-ren dimentsioetara egokitzeko
        !z=0.1*(3*sin(x+2*cos(y)+4*sin(2*x+y)))+2.0_dp
        f(kontadore,1:3)=(/x,y,z/)
        !write(unit=42,fmt=*) f(kontadore,1:3)
        kontadore=kontadore+1
      enddo
    enddo


!-----------------------------------------------------
! lortu mugan dauden partikulen indizeak
 
  allocate(muga_indizeak(npartikula_0)) 
  call lortu_muga_indize_karratu(f,muga_indizeak) ! (partikulak,muga_indizeak)
  write(unit=45,fmt=*) size(muga_indizeak,1)
  do i=1,npartikula_0
    write(unit=45,fmt="(3f20.14,i5)") f(muga_indizeak(i),1:3),muga_indizeak(i)
  enddo

!-----------------------------------------------------
! zenbat i_partikulen auzokoak bilatu ezarri eta loopa egin

  i_kopurua=npartikula_0
  nj_max=floor(sqrt(real(i_kopurua)))*2     ! estimatu memoria kopuru onargarri bat
  nj_max=max(nj_max,mMin_jauzoko)              ! parametroetan jarritako memoria minimoa alokatu nahi dugu gutxienez

  allocate(h_gorde(i_kopurua))
  allocate(kontzentrazioa_gorde(i_kopurua))
  allocate(Rij_gorde(i_kopurua,nj_max,3))   ! sqrt(i_kop) estimazio nahiko ona da, ze beti egongo dira hori baino auzoko gutxiago
  allocate(ggrad_gorde(i_kopurua,nj_max,3)) ! berdin

  allocate(presioa_gorde(i_kopurua))
  allocate(bortizitatea_gorde(i_kopurua))
  allocate(tentsioa_gorde(i_kopurua))
  allocate(u_gorde(i_kopurua,3))

  allocate(h_desplazatua_gorde(i_kopurua))


!-----------------------------------------------------
! H.B.
! partikula guztien altuera numerikoa lortu 0. iterazioan
! definitu hasierako u eta bortizitatea

  h_gorde(:)             =hi_0
  kontzentrazioa_gorde(:)=kontzentrazioa_0
  Rij_gorde(:,:,:)       =0.0_dp
  
  presioa_gorde(:)       =0.0_dp
  !bortizitatea_gorde(:)  =0.0_dp
  tentsioa_gorde(:)      =gamma_0
  !u_gorde(:,:)           =0.002_dp
  
  !h_desplazatua_gorde(:) =0.0_dp

  call random_seed()
  call zati_sarea_sortu(f,f_sare)

  do i=1,i_kopurua
    h_gorde(i)=0.0_dp
    call i_ren_auzokoak(f,f_sare,i,auzokoen_indizeak)
    call lortu_i_ren_sistema_lokala(f,i,auzokoen_indizeak,i_oinarria) 
    do j=1,size(auzokoen_indizeak,1)
      j_=auzokoen_indizeak(j)
      call lortu_Rij_gainazal(f(i,1:3),f(j_,1:3),i_oinarria(1:3,3),Rij_gorde(i,j,1:3)) ! (ri,rj,ni,Rij_gainazal)
      h_gorde(i)=h_gorde(i)+Vi_0*h_kernel(norm2(Rij_gorde(i,j,1:3)),R_auzo)            ! hi=hi+Vj*Wij j guztietarako (horregatik behar dut ere Rij)
    enddo
     
    !if ( (f(i,1)>(luzera_x0*0.25)).and.(f(i,1)<(luzera_x0*0.75)).and.(f(i,2)>(luzera_y0*0.25)).and.(f(i,2)<(luzera_y0*0.75)) ) then
      call random_number(random1)
      u_gorde(i,1)=(random1-0.5_dp)*0.00001_dp  ! (0,1) izatetik (-0.5,0.5) izatera pasatu zorizko zenbakia
      call random_number(random1)
      u_gorde(i,2)=(random1-0.5_dp)*0.00001_dp
      u_gorde(i,3)=0.0_dp   ! =0 planoan gaude hasieran
    !else
    !  u_gorde(i,1:3)=0.0_dp
    !endif

    call random_number(random2)
    bortizitatea_gorde(i)=random2*bortizitate_0
    h_desplazatua_gorde(i)=h_gorde(i)
  enddo


!-----------------------------------------------------
! hasi denbora loop orokorra
do t=1,t_total

!-----------------------------------------------------
! zati sarea sortu iterazio bakoitzean behin

  call zati_sarea_sortu(f,f_sare)

!-----------------------------------------------------
! hasi i partikula bakoitzaren loop-a

  write(unit=42,fmt=*)
  write(unit=42,fmt=*)
  write(unit=44,fmt=*) 
  write(unit=44,fmt=*) 
  write(unit=44,fmt=*) "t=",t
  !write(unit=45,fmt=*)
  !write(unit=45,fmt=*)

do i=1,i_kopurua                                 
  
  ea_mugakoa=.false.
  
  do j=1,size(muga_indizeak,1)
  if (i==muga_indizeak(j)) then ! ikusi ea mugako partikula den
    ea_mugakoa=.true.
    u_gorde(muga_indizeak(j),1:3)=0.0_dp ! muga partikulen abiadura =0 berdefinitu
    write(unit=42,fmt="(4f20.14)") f(i,1:3),norm2(u_gorde(i,1:3)) ! idatzi posizioa eta abiadura modulua (iterazioa saltatzean ez baitugu marrazten)
    exit
  endif
  enddo
  if (ea_mugakoa) then
    cycle                       ! mugakoa bada saltatu iterazio hau
  endif

!-----------------------------------------------------
! lortu auzokoen zerrenda

  call i_ren_auzokoak(f,f_sare,i,auzokoen_indizeak)

!====debug====
if (size(auzokoen_indizeak,1)>nj_max) then
 write(unit=109,fmt=*) "(9 errorea) arazoa programa nagusian (224 lerroan):"  
 write(unit=109,fmt="(a,i5,a)") " --> auzokoetarako esleitutako memoria (",nj_max,") baino auzoko gehio daude"
 write(unit=109,fmt=*) "--> i partikula indizea:",i
 write(unit=109,fmt=*) "--> t iterazio indizea:",t
 write(unit=109,fmt=*) "--> auzoko kopurua:",size(auzokoen_indizeak,1)
 stop  
endif
!====-----====
!-----------------------------------------------------
! lortu sistema lokalaren oinarri bektoreak
  
  call lortu_i_ren_sistema_lokala(f,i,auzokoen_indizeak,i_oinarria) 
  
!-----------------------------------------------------
! lortu ij tentsore metrikoa

  call  lortu_tentsore_metrikoa_i(f,i,auzokoen_indizeak,i_oinarria,g,g_1,det_g,det_g_1) 

!-----------------------------------------------------
! algoritmo 1: prestaketa

  
  ! batuketak erreseteatu
  dibergentzia_i       =0.0_dp
  kurbadura_lokala_i   =0.0_dp

  do j=1,size(auzokoen_indizeak,1)

    j_=auzokoen_indizeak(j)

  ! lortu posizio bektore erlatiboak sistema lokal bakoitzeko
    call lortu_Rij_gainazal(         f(i,1:3),f(j_,1:3),i_oinarria(1:3,3),Rij_gorde(i,j,1:3))
                                      !(ri,rj,ni,Rij_gainazal)
    call lortu_gainazal_gradientea(  g_1,Rij_gorde(i,j,1:3),i_oinarria,ggrad_gorde(i,j,1:3))
                                      !(g_1,Rij_gainazal,iren_oinarria,gainazal_gradientea)
    
  ! lortu abiadura bektore erlatiboak sistema lokal bakoitzeko
    call i_ren_auzokoak(             f,f_sare,j_,j_auzokoen_indizeak)     
                                      !(p_guztiak,zs_hiztegia,indizea,auzokoen_indizeak)
    call lortu_i_ren_sistema_lokala( f,j_,j_auzokoen_indizeak,j_oinarria) 
                                      !(p_guztiak,indizea,iren_auzo,iren_oinarria)
    call lortu_Uij_gainazal(         f(i,1:3),f(j_,1:3),u_gorde(i,1:3),u_gorde(j_,1:3),i_oinarria,j_oinarria,Uij_gainazal)
                                      !(ri,rj,ui,uj,iren_oinarria,jren_oinarria,Uij_gainazal)
  ! algoritmo1
    call lortu_dibergentzia_j(       Vi_0,h_gorde(j_),Uij_gainazal,ggrad_gorde(i,j,1:3),dibergentzia_j)
                                      !(Vj,hj,Uij,gainazal_gradientea,dibergentzia_j)
    call lortu_kurbadura_lokala_j(   Vi_0,h_gorde(j_),h_gorde(i),Rij_gorde(i,j,1:3),ggrad_gorde(i,j,1:3),kurbadura_lokala_j)
                                      !(Vj,hj,hi,Rij,gainazal_gradientea,kurbadura_lokala_j)
    dibergentzia_i    =dibergentzia_i    +dibergentzia_j
    kurbadura_lokala_i=kurbadura_lokala_i+kurbadura_lokala_j

  enddo

  ! algoritmo1 bukatu
  call lortu_gainazal_tentsioa_i(   kontzentrazioa_gorde(i),tentsioa_gorde(i))
                                     !(xaboi_kontzentrazioa_i,gainazal_tentsioa_i)
  call lortu_presioa_i(             h_gorde(i),tentsioa_gorde(i),kurbadura_lokala_i,dibergentzia_i,presioa_gorde(i))
                                     !(hi,gainazal_tentsioa_i,kurbadura_lokala_i,dibergentzia_i,presioa_i)
  if ( (mod(i,123)==0).or.(t==1) ) then
  write(unit=43,fmt=*)
  write(unit=43,fmt=*)
  write(unit=43,fmt=*) "i=",i
  write(unit=43,fmt="(a,2f20.14)") "g_1           --->"     ,g_1(1,1),g_1(1,2)
  write(unit=43,fmt="(a,2f20.14)") "g_1           --->"     ,g_1(2,1),g_1(2,2)
  write(unit=43,fmt="(a,e24.14)")  "dibergentzia_i--->"     ,dibergentzia_i
  write(unit=43,fmt="(a,e24.14)")  "kurbadura_lokala_i--->" ,kurbadura_lokala_i
  write(unit=43,fmt="(a,e24.14)")  "gainazal_tentsioa_i--->",tentsioa_gorde(i)
  write(unit=43,fmt="(a,e24.14)")  "presioa_i--->"          ,presioa_gorde(i)
  endif
  
!-----------------------------------------------------
! algoritmoen 2: indarren kalkulua
   
  ! batuketak erreseteatu
  bortizitate_indarra_i=0.0_dp
  presio_indarra_i     =0.0_dp
  Marangoni_indarra_i  =0.0_dp
  kapilare_indarra_i   =0.0_dp
  biskositate_indarra_i=0.0_dp

  do j=1,size(auzokoen_indizeak,1)

    j_=auzokoen_indizeak(j)

    call lortu_bortizitate_indarra_j(  bortizitatea_gorde(j_),Rij_gorde(i,j,1:3),j_oinarria(1:3,3),bortizitate_indarra_j)
                                        !(bortizitate_j,Rij_gainazal,nj,bortizitate_indarra_j)
    call lortu_presio_indarra_j(       h_gorde(i),h_gorde(j_),Vi_0,Vi_0,presioa_gorde(i),presioa_gorde(j_),ggrad_gorde(i,j,1:3),presio_indarra_j)
                                        !(hi,hj,Vi,Vj,pi,pj,gainazal_gradientea,presio_indarra_j)
    call lortu_Marangoni_indarra_j(    h_gorde(i),h_gorde(j_),Vi_0,Vi_0,tentsioa_gorde(i),tentsioa_gorde(j_),ggrad_gorde(i,j,1:3),Marangoni_indarra_j)
                                        !(hi,hj,Vi,Vj,tentsio_i,tentsio_j,gainazal_gradientea,Marangoni_indarra_j)
    call lortu_kapilare_indarra_j(     h_gorde(i),h_gorde(j_),Vi_0,Vi_0,tentsioa_gorde(i),i_oinarria(1:3,3),&
                                       f(i,1:3),f(j_,1:3),ggrad_gorde(i,j,1:3),Rij_gorde(i,j,1:3),kapilare_indarra_j)
                                        !(hi,hj,Vi,Vj,tentsio_i,ni,ri,rj,gainazal_gradientea,Rij_gainazal,kapilare_indarra_j) 
    call lortu_biskositate_indarra_j(  h_gorde(j_),Vi_0,Vi_0,i_oinarria(1:3,3),u_gorde(i,1:3),u_gorde(j_,1:3),ggrad_gorde(i,j,1:3),Rij_gorde(i,j,1:3),biskositate_indarra_j)
                                        !(hj,Vi,Vj,ni,ui,uj,gainazal_gradientea,Rij_gainazal,biskositate_indarra_j)
    bortizitate_indarra_i=bortizitate_indarra_i+bortizitate_indarra_j
    presio_indarra_i     =presio_indarra_i     +presio_indarra_j
    Marangoni_indarra_i  =Marangoni_indarra_i  +Marangoni_indarra_j
    kapilare_indarra_i   =kapilare_indarra_i   +kapilare_indarra_j
    biskositate_indarra_i=biskositate_indarra_i+biskositate_indarra_j

  enddo

  !indarrak lortzen bukatu
  !erdiko partikulei x norabidean indar bat jarriko diegu
  if (t<=50) then
    if ( (f(i,1)>((luzera_x0*0.45_dp+x0))).and.(f(i,1)<((luzera_x0*0.55_dp+x0))).and.(f(i,2)>((luzera_y0*0.45_dp+y0))).and.(f(i,2)<((luzera_y0*0.55_dp+y0))) ) then
      call lortu_kanpo_indarra_i(          mi_0,extra_indarra_1,kanpo_indarra_i)            ! (mi,f_ext,kanpo_indarra_i) 
    else
      call lortu_kanpo_indarra_i(          mi_0,(/0.0_dp,0.0_dp,0.0_dp/),kanpo_indarra_i)   ! (mi,f_ext,kanpo_indarra_i) 
    endif 
  else
    call lortu_kanpo_indarra_i(          mi_0,(/0.0_dp,0.0_dp,0.0_dp/),kanpo_indarra_i)   ! (mi,f_ext,kanpo_indarra_i) 
  endif 

  if ( (mod(i,123)==0).or.(t==1) ) then
  write(unit=43,fmt="(a,3e24.14)")  "--->bortizitate_indarra_i" ,bortizitate_indarra_i
  write(unit=43,fmt="(a,3e24.14)")  "--->presio_indarra_i"      ,presio_indarra_i
  write(unit=43,fmt="(a,3e24.14)")  "--->Marangoni_indarra_i"   ,Marangoni_indarra_i
  write(unit=43,fmt="(a,3e24.14)")  "--->kapilare_indarra_i"    ,kapilare_indarra_i
  write(unit=43,fmt="(a,3e24.14)")  "--->biskositate_indarra_i" ,biskositate_indarra_i 
  write(unit=43,fmt="(a,3e24.14)")  "--->kanpo_indarra_i"       ,kanpo_indarra_i
  endif

!-----------------------------------------------------
! algoritmoen 3: denbora aktualizazioa

  !lortu altuera desplazatua partikula bakoitzerako t-garren iterazioan
    h_desplazatua_gorde(i)=h_desplazatua_gorde(i)-h_desplazatua_gorde(i)*dibergentzia_i*delta_t

  
  !aldatu surfaktantearen kontzentrazioaren difusioa t-garren iterazioan
    kontzentrazio_kalk=0.0_dp
    do j=1,size(auzokoen_indizeak,1)
      j_=auzokoen_indizeak(j) !ez dut behar hemen
      kontzentrazio_kalk=kontzentrazio_kalk+(Vi_0*h_gorde(j_))*(kontzentrazioa_gorde(j_)-kontzentrazioa_gorde(i))&
                                           *2.0_dp*(norm2(ggrad_gorde(i,j,1:3))/norm2(Rij_gorde(i,j,1:3)))
    enddo
    kontzentrazioa_gorde(i)=kontzentrazioa_gorde(i)+alpha_c*delta_t*kontzentrazio_kalk

  !aldatu abiadura
    u_gorde(i,1:3)=u_gorde(i,1:3)+(delta_t/mi_0)*(kanpo_indarra_i+bortizitate_indarra_i+presio_indarra_i&
                                                 +Marangoni_indarra_i+kapilare_indarra_i+biskositate_indarra_i)
    if (norm2(u_gorde(i,1:3))>ui_max) then
      u_gorde(i,1:3)=u_gorde(i,1:3)/norm2(u_gorde(i,1:3)) ! normalizatu
      u_gorde(i,1:3)=u_gorde(i,1:3)*ui_max                ! ui_max-ren abiadura forzatu, norabidea mantenduz
!====debug====
 write(unit=109,fmt=*) "(11 errorea) arazoa programa nagusian (369 lerroan):"
 write(unit=109,fmt=*) "--> ui-k abiadura maximoa gainditu du"
 write(unit=109,fmt=*) "--> i partikula indizea:",i
 write(unit=109,fmt=*) "--> t iterazio indizea:",t
!====-----====
    endif

  !aldatu posizioa
    f(i,1:3)=f(i,1:3)+u_gorde(i,1:3)*delta_t
    call lortu_islapen_karratu(f(i,1:3),u_gorde(i,1:3))  ! (pos,ui)
    write(unit=42,fmt="(4f20.14)") f(i,1:3),norm2(u_gorde(i,1:3))            ! idatzi posizioa eta abiadura modulua
    !write(unit=42,fmt="(4f20.14)") f(i,1:3),h_desplazatua_gorde(i)          ! idatzi posizioa eta h_desplazatua
    !write(unit=45,fmt="(4f20.14)") f(i,1:2),0.016_dp,norm2(u_gorde(i,1:3))  ! idatzi posizioa, marraztutako z_min eta abiadura modulua

!====debug====
if ( (f(i,1)<x0).or.(f(i,1)>(luzera_x0+x0)).or.(f(i,2)<y0).or.(f(i,2)>(luzera_y0+y0))  ) then
 write(unit=109,fmt=*) "(10 errorea) arazoa programa nagusian (383 lerroan):"  
 write(unit=109,fmt=*) "--> i partikula mugatik kanpo atera da"
 write(unit=109,fmt=*) "--> i partikula indizea:",i
 write(unit=109,fmt=*) "--> t iterazio indizea:",t
 stop  
endif
!====-----====

  !lortu altuera numerikoa partikula bakoitzerako t-garren iterazioan
    h_gorde(i)=0.0_dp
    do j=1,size(auzokoen_indizeak,1)
      j_=auzokoen_indizeak(j) !ez dut behar hemen
      h_gorde(i)=h_gorde(i)+Vi_0*h_kernel(norm2(Rij_gorde(i,j,1:3)),R_auzo)    ! hi=hi+Vj*Wij j guztietarako (horregatik behar dut ere Rij)
    enddo
  
  if ( (mod(i,123)==0).or.(t==1) ) then
  write(unit=44,fmt=*) "i=",i
  write(unit=44,fmt="(a,3e24.14)")  "--->h_desplazatua_gorde(i)" ,h_desplazatua_gorde(i)
  write(unit=44,fmt="(a,3e24.14)")  "--->kontzentrazioa_gorde(i)",kontzentrazioa_gorde(i)
  write(unit=44,fmt="(a,3e24.14)")  "--->u_gorde(i)"             ,u_gorde(i,1:3)
  write(unit=44,fmt="(a,3e24.14)")  "--->h_gorde(i)"             ,h_gorde(i)
  endif

!-----------------------------------------------------

enddo!-----partikula guztien gaineko loop-a itxi
enddo!-----denbora loop-a itxi

!=============================================================================================================================================0
! gnuplot komandoak guztia marrazteko

! ---> komandoak unit 42 <---
! m*2 gauza gorde ditut, auzoko eta bektoreak tartekatzen baitira fitxategian, beraz do-an kontutan hartu

  !write(unit=52,fmt="(a,i5,a)") 'splot "datuMuga.dat" index', 0,' using 1:2:3 pt 7 ps 1 notitle'           ! puntu hodeia marraztu (kendu diot ",\" soilik hau baitago)

  !do i=1,i_kopurua*2,2 ! m*2 gauza, binaka kontatuz, ze berruan ya daukagu i+1 indizea          
  !  write(unit=52,fmt="(a,i5,a)") '"datuMuga.dat" index', i,' using 1:2:3 pt 7 ps 1 notitle,\'               ! auzokoak marraztu
  !  write(unit=52,fmt="(a,i5,a)") '"datuMuga.dat" index', i+1,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "black" notitle,\'  ! bektoreak marraztu
  !enddo

  !write(unit=52,fmt="(a,i5,a)") '"datuMuga.dat" index', i_kopurua*2-1,' using 1:2:3 pt 7 ps 1 notitle,\'          
  !write(unit=52,fmt="(a,i5,a)") '"datuMuga.dat" index', i_kopurua*2,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "black" notitle,\' ! azkena ",\" izan behar orain segitzeko

  write(unit=52,fmt="(a)") 'set hidden3d'                                                                ! gnuplot prestatu
  write(unit=52,fmt="(a)") 'set xlabel "x"'                                                              ! gnuplot prestatu
  write(unit=52,fmt="(a)") 'set ylabel "y"'                                                              ! gnuplot prestatu
  write(unit=52,fmt="(a)") 'set zlabel "z"'                                                              ! gnuplot prestatu
  write(unit=52,fmt="(a,f12.7,a,f12.7,a)") 'set xrange [',muga_x0,':',muga_xf,']'                        ! gnuplot prestatu
  write(unit=52,fmt="(a,f12.7,a,f12.7,a)") 'set yrange [',muga_y0,':',muga_yf,']'                        ! gnuplot prestatu
  write(unit=52,fmt="(a,f12.7,a)")         'set zrange [',minval(f(:,3)),':1.02001]'                                   ! gnuplot prestatu
  write(unit=52,fmt="(a)")                 'set cbrange [0.0:0.0005]'                                    ! 10**-4
  do i=0,t_total-1 !0-tik hasi behar index-erako
    write(unit=52,fmt="(a,i5,a)") 'splot "datuMuga.dat" index', i,' using 1:2:3:4 pt 7 ps 1 palette notitle'  ! posizioak marraztu t bakoitzean
    !write(unit=52,fmt="(a,i5,a)") '"datuMuga4.dat" index', i,' using 1:2:3:4 pt 7 ps 1 palette notitle'  ! abiaduren moduluak  marraztu t bakoitzean
  enddo

! ---> komandoak unit 43 <---
!  i_kopurua*2+1 -etik hasi behar -> EZ, ze beste fitxategi bat da
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
!  
!  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
!    write(unit=52,fmt="(a,i5,a)") '"datuMuga2.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "blue" notitle,\'  ! posizio erlatiboak marraztu
!  enddo
!
!  write(unit=52,fmt="(a,i5,a)") '"datuMuga2.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "blue" notitle'  ! azkena ",\" gabe
!
! ---> komandoak unit 44 <---
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
!  
!  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
!    write(unit=52,fmt="(a,i5,a)") '"datuMuga3.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "purple" notitle,\'  ! posizio erlatiboak marraztu
!  enddo
!
!  write(unit=52,fmt="(a,i5,a)") '"datuMuga3.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "purple" notitle,\'  ! azkena ",\" izan behar segitzeko
!
! ---> komandoak unit 45 <---
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
!  
!  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
!    write(unit=52,fmt="(a,i5,a)") '"datuMuga4.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "turquoise" notitle,\'  ! posizio erlatiboak marraztu
!  enddo
!
!  write(unit=52,fmt="(a,i5,a)") '"uga4.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "turquoise" notitle'  ! azkena ",\" ez izateko
!-------
close(unit=42)
close(unit=43)
close(unit=44)
close(unit=45)
close(unit=52)
close(unit=109)
deallocate(f)
deallocate(f_sare)
deallocate(auzokoen_indizeak)

deallocate(h_gorde)
deallocate(kontzentrazioa_gorde)
deallocate(Rij_gorde)
deallocate(ggrad_gorde)

deallocate(presioa_gorde)
deallocate(bortizitatea_gorde)
deallocate(tentsioa_gorde)
deallocate(u_gorde)

deallocate(h_desplazatua_gorde)

deallocate(muga_indizeak)

endprogram probatu_prestaketa
