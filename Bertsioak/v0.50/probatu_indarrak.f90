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
implicit none

real(kind=dp)                             :: x,y,z,kte
integer                                   :: i,j,k,l,n,m,kontadore,i_partikula
integer                                   :: n_auzo,x_auzo,y_auzo,z_auzo,errore
integer                                   :: i_kopurua ! zenbat i_partikula ezberdin bilatu
integer                                   :: i_hasiera,i_biderkatzailea
real(kind=dp)                             :: random
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

open(unit=42,action="write",status="replace",file="datuIndarrak.dat")   ! posizioak
open(unit=43,action="write",status="replace",file="datuIndarrak2.dat")  ! dibergentzia, kurbadura, gainazal tentsioa, presioa eta indarrak
open(unit=44,action="write",status="replace",file="datuIndarrak3.dat")  ! algoritmo 3
!open(unit=45,action="write",status="replace",file="datuIndarrak4.dat") 
open(unit=52,action="write",status="replace",file="gnuplotKomandoak.dat")
open(unit=109,action="write",status="replace",file="erroreenLog.dat")

  
!-----------------------------------------------------
! artikuluko olatu funtzioarekin puntu hodeia eraiki

  n=floor(sqrt(real(npartikula_0)))
  !n=100
    allocate(f(n*n,3))
    kontadore=1
    do i=1,n
      !x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
      x=0.0_dp+(luzera_x0-0.0_dp)*(i-1)/(n-1)
      do j=1,n
        !y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
        y=0.0_dp+(luzera_y0-0.0_dp)*(j-1)/(n-1)
        z=(0.1*(3*sin(x*100.0_dp)+2*cos(y*100.0_dp)+4*sin((2*x+y)*100.0_dp))+2.0_dp)*0.01 !eskala aldatu xy-ren dimentsioetara egokitzeko
        !z=0.1*(3*sin(x+2*cos(y)+4*sin(2*x+y)))+2.0_dp
        f(kontadore,1:3)=(/x,y,z/)
        write(unit=42,fmt=*) f(kontadore,1:3)
        kontadore=kontadore+1
      enddo
    enddo


!-----------------------------------------------------
! zenbat i_partikulen auzokoak bilatu ezarri eta loopa egin

  i_kopurua=npartikula_0
  allocate(h_gorde(i_kopurua))
  allocate(kontzentrazioa_gorde(i_kopurua))
  allocate(Rij_gorde(i_kopurua,floor(sqrt(real(i_kopurua))),3))   ! sqrt(i_kop) estimazio nahiko ona da, ze beti egongo dira hori baino auzoko gutxiago
  allocate(ggrad_gorde(i_kopurua,floor(sqrt(real(i_kopurua))),3)) ! berdin

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

    call random_number(random)
    u_gorde(i,1)=random*0.01
    call random_number(random)
    u_gorde(i,2)=random*0.01
    call random_number(random)
    u_gorde(i,3)=random*0.01
    call random_number(random)
    bortizitatea_gorde(i)=random*bortizitate_0

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

do i=1,i_kopurua                                 

!-----------------------------------------------------
! lortu auzokoen zerrenda

  call i_ren_auzokoak(f,f_sare,i,auzokoen_indizeak)

!-----------------------------------------------------
! lortu sistema lokalaren oinarri bektoreak
  
  call lortu_i_ren_sistema_lokala(f,i,auzokoen_indizeak,i_oinarria) 
  
!-----------------------------------------------------
! lortu ij tentsore metrikoa

  call  lortu_tentsore_metrikoa_i(f,i,auzokoen_indizeak,i_oinarria,g,g_1,det_g,det_g_1) 

!-----------------------------------------------------
! algoritmo 1: prestaketa

  write(unit=43,fmt=*)
  write(unit=43,fmt=*)
  
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
  if (mod(i,1000)==0) then
  write(unit=43,fmt="(a,f20.14)")  "dibergentzia_i--->"     ,dibergentzia_i
  write(unit=43,fmt="(a,f20.14)")  "kurbadura_lokala_i--->" ,kurbadura_lokala_i
  write(unit=43,fmt="(a,f20.14)")  "gainazal_tentsioa_i--->",tentsioa_gorde(i)
  write(unit=43,fmt="(a,f20.14)")  "presioa_i--->"          ,presioa_gorde(i)
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
  call lortu_kanpo_indarra_i(          mi_0,(/0.0_dp,0.0_dp,0.0_dp/),kanpo_indarra_i)   ! (mi,f_ext,kanpo_indarra_i)  
  if (mod(i,1000)==0) then
  write(unit=43,fmt="(a,3f20.14)")  "--->bortizitate_indarra_i" ,bortizitate_indarra_i
  write(unit=43,fmt="(a,3f20.14)")  "--->presio_indarra_i"      ,presio_indarra_i
  write(unit=43,fmt="(a,3f20.14)")  "--->Marangoni_indarra_i"   ,Marangoni_indarra_i
  write(unit=43,fmt="(a,3f20.14)")  "--->kapilare_indarra_i"    ,kapilare_indarra_i
  write(unit=43,fmt="(a,3f20.14)")  "--->biskositate_indarra_i" ,biskositate_indarra_i 
  write(unit=43,fmt="(a,3f20.14)")  "--->kanpo_indarra_i"       ,kanpo_indarra_i
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
  !aldatu posizioa
    f(i,1:3)=f(i,1:3)+u_gorde(i,1:3)*delta_t
    write(unit=42,fmt=*) f(i,1:3) 

  !lortu altuera numerikoa partikula bakoitzerako t-garren iterazioan
    h_gorde(i)=0.0_dp
    do j=1,size(auzokoen_indizeak,1)
      j_=auzokoen_indizeak(j) !ez dut behar hemen
      h_gorde(i)=h_gorde(i)+Vi_0*h_kernel(norm2(Rij_gorde(i,j,1:3)),R_auzo)    ! hi=hi+Vj*Wij j guztietarako (horregatik behar dut ere Rij)
    enddo
  
  if (mod(i,1000)==0) then
  write(unit=44,fmt=*) "i=",i
  write(unit=44,fmt="(a,3f20.14)")  "--->h_desplazatua_gorde(i)" ,h_desplazatua_gorde(i)
  write(unit=44,fmt="(a,3f20.14)")  "--->kontzentrazioa_gorde(i)",kontzentrazioa_gorde(i)
  write(unit=44,fmt="(a,3f20.14)")  "--->u_gorde(i)"             ,u_gorde(i,1:3)
  write(unit=44,fmt="(a,3f20.14)")  "--->h_gorde(i)"             ,h_gorde(i)
  endif

!-----------------------------------------------------

enddo!-----partikula guztien gaineko loop-a itxi
  write(unit=44,fmt=*) 
  write(unit=44,fmt=*) "t=",t
enddo!-----denbora loop-a itxi

!=============================================================================================================================================0
! gnuplot komandoak guztia marrazteko

! ---> komandoak unit 42 <---
! m*2 gauza gorde ditut, auzoko eta bektoreak tartekatzen baitira fitxategian, beraz do-an kontutan hartu

  write(unit=52,fmt="(a,i5,a)") 'splot "datuIndarrak.dat" index', 0,' using 1:2:3 pt 7 ps 1 notitle'           ! puntu hodeia marraztu (kendu diot ",\" soilik hau baitago)

  !do i=1,i_kopurua*2,2 ! m*2 gauza, binaka kontatuz, ze berruan ya daukagu i+1 indizea          
  !  write(unit=52,fmt="(a,i5,a)") '"datuIndarrak.dat" index', i,' using 1:2:3 pt 7 ps 1 notitle,\'               ! auzokoak marraztu
  !  write(unit=52,fmt="(a,i5,a)") '"datuIndarrak.dat" index', i+1,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "black" notitle,\'  ! bektoreak marraztu
  !enddo

  !write(unit=52,fmt="(a,i5,a)") '"datuIndarrak.dat" index', i_kopurua*2-1,' using 1:2:3 pt 7 ps 1 notitle,\'          
  !write(unit=52,fmt="(a,i5,a)") '"datuIndarrak.dat" index', i_kopurua*2,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "black" notitle,\' ! azkena ",\" izan behar orain segitzeko


! ---> komandoak unit 43 <---
!  i_kopurua*2+1 -etik hasi behar -> EZ, ze beste fitxategi bat da
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
!  
!  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
!    write(unit=52,fmt="(a,i5,a)") '"datuIndarrak2.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "blue" notitle,\'  ! posizio erlatiboak marraztu
!  enddo
!
!  write(unit=52,fmt="(a,i5,a)") '"datuIndarrak2.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "blue" notitle'  ! azkena ",\" gabe
!
! ---> komandoak unit 44 <---
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
!  
!  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
!    write(unit=52,fmt="(a,i5,a)") '"datuIndarrak3.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "purple" notitle,\'  ! posizio erlatiboak marraztu
!  enddo
!
!  write(unit=52,fmt="(a,i5,a)") '"datuIndarrak3.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "purple" notitle,\'  ! azkena ",\" izan behar segitzeko
!
! ---> komandoak unit 45 <---
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
!  
!  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
!    write(unit=52,fmt="(a,i5,a)") '"datuIndarrak4.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "turquoise" notitle,\'  ! posizio erlatiboak marraztu
!  enddo
!
!  write(unit=52,fmt="(a,i5,a)") '"datuIndarrak4.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "turquoise" notitle'  ! azkena ",\" ez izateko
!-------
close(unit=42)
close(unit=43)
close(unit=44)
!close(unit=45)
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

endprogram probatu_prestaketa
