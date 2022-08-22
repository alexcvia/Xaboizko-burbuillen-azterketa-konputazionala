program probatu_bektoreak
use tipos
use parametroak
use auzokoak
use funtzioak
use mcf_diagonalizacion ! kontuz, konpilatzean matrices.f90 da honen modulu nagusia
use mcf_slineales
use lokala
use bektoreak

real(kind=dp)                           :: x,y,z,kte
integer                                 :: i,j,k,l,n,m,kontadore,i_partikula
integer                                 :: n_auzo,x_auzo,y_auzo,z_auzo,errore
integer                                 :: i_kopurua ! zenbat i_partikula ezberdin bilatu
integer                                 :: i_hasiera,i_biderkatzailea
real(kind=dp)                           :: random
real(kind=dp),dimension(:,:),allocatable:: f ! puntu hodeia gordetzeko
integer,dimension(:,:,:,:),allocatable  :: f_sare
integer,dimension(:),allocatable        :: auzokoen_indizeak

real(kind=dp),dimension(3,3)            :: i_oinarria

real(kind=dp),dimension(2,2)            :: g             ! i-ren tentsore metrikoa
real(kind=dp),dimension(2,2)            :: g_1           ! g-ren alderantzizkoa
real(kind=dp)                           :: det_g         ! g-ren determinantea
real(kind=dp)                           :: det_g_1       ! g_1-ren determinantea

real(kind=dp), dimension(3)             :: Rij_gainazal  ! i eta bere auzokoen arteko posizio erlatiboa
real(kind=dp), dimension(3)             :: Uij_gainazal  ! i eta bere auzokoen arteko abiadura erlatiboa
real(kind=dp), dimension(3)             :: ui,uj         ! i eta j-ren abiadura gordetzeko bektorea
integer,dimension(:),allocatable        :: j_auzokoen_indizeak
real(kind=dp),dimension(3,3)            :: j_oinarria

real(kind=dp), dimension(2)             :: gainazal_gradientea  ! i eta j arteko gainazal gradientea
real(kind=dp), dimension(2)             :: gradientea           ! gainazal gradienteen batura (probak egiteko da ez dut benetan behar)


open(unit=42,action="write",status="replace",file="datuBektoreak.dat")
open(unit=43,action="write",status="replace",file="datuBektoreak2.dat")
open(unit=44,action="write",status="replace",file="datuBektoreak3.dat")
open(unit=45,action="write",status="replace",file="datuBektoreak4.dat")
open(unit=52,action="write",status="replace",file="gnuplotKomandoak.dat")
open(unit=109,action="write",status="replace",file="erroreenLog.dat")

  
!-----------------------------------------------------
! artikuluko olatu funtzioarekin puntu hodeia eraiki

  n=100
    allocate(f(n*n,3))
    kontadore=1
    do i=1,n
      x=0.0_dp+(10.0_dp-0.0_dp)*(i-1)/(n-1)
      do j=1,n
        y=0.0_dp+(10.0_dp-0.0_dp)*(j-1)/(n-1)
        z=0.1*(3*sin(x)+2*cos(y)+4*sin(2*x+y))+2.0_dp
        f(kontadore,1:3)=(/x,y,z/)
        write(unit=42,fmt=*) f(kontadore,1:3)
        kontadore=kontadore+1
      enddo
    enddo


!-----------------------------------------------------
! zenbat i_partikulen auzokoak bilatu ezarri eta loopa egin

  i_kopurua=25
  !i_hasiera=200
  !i_biderkatzailea=(n*n-i_hasiera)/i_kopurua
  call random_seed()
  gradientea=0.0_dp
  
do m=1,i_kopurua                                 !------ indize ugariko loop

    !i_partikula=i_hasiera+m*i_biderkatzailea    ! ez gara partikula kopurutik pasa nahi
    !i_partikula=505+4*725 
    call random_number(random)
    i_partikula=floor(random*n*n)


!-----------------------------------------------------
! lortu auzokoen zerrenda, oraingoz i_partukula baterako

  call zati_sarea_sortu(f,f_sare) ! hau ez da do kanpoan egon behar?

  call i_ren_auzokoak(f,f_sare,i_partikula,auzokoen_indizeak)
   
  write(unit=42,fmt=*)
  write(unit=42,fmt=*)
  do i=1,size(auzokoen_indizeak,1)
    write(unit=42,fmt=*) f(auzokoen_indizeak(i),1:3)
  enddo
  

   
!-----------------------------------------------------
! lortu sistema lokalaren oinarri bektoreak
  
  call lortu_i_ren_sistema_lokala(f,i_partikula,auzokoen_indizeak,i_oinarria) 
  
  write(unit=42,fmt=*)
  write(unit=42,fmt=*)
  write(unit=42,fmt="(6f20.14)")  f(i_partikula,1:3),i_oinarria(1:3,1)
  write(unit=42,fmt="(6f20.14)")  f(i_partikula,1:3),i_oinarria(1:3,2)
  write(unit=42,fmt="(6f20.14)")  f(i_partikula,1:3),i_oinarria(1:3,3)
!  write(unit=42,fmt=*) i_oinarria(1,1),i_oinarria(2,1),i_oinarria(3,1)
!  write(unit=42,fmt=*) i_oinarria(1,2),i_oinarria(2,2),i_oinarria(3,2)
!  write(unit=42,fmt=*) i_oinarria(1,3),i_oinarria(2,3),i_oinarria(3,3)



!-----------------------------------------------------
! lortu ij tentsore metrikoa

  call  lortu_ij_tentsore_metrikoa(f,i_partikula,auzokoen_indizeak,i_oinarria,g,g_1,det_g,det_g_1) 
  write(unit=42,fmt=*)
  write(unit=42,fmt=*) g(1,1:2)
  write(unit=42,fmt=*) g(2,1:2)
  write(unit=42,fmt=*) det_g
  write(unit=42,fmt=*) g_1(1,1:2)
  write(unit=42,fmt=*) g_1(2,1:2)
  write(unit=42,fmt=*) det_g_1

!-----------------------------------------------------
! lortu posizio bektore erlatiboak sistema lokal bakoitzeko

  write(unit=43,fmt=*)
  write(unit=43,fmt=*)
  write(unit=45,fmt=*)
  write(unit=45,fmt=*)
  do i=1,size(auzokoen_indizeak,1)
    if (auzokoen_indizeak(i)==i_partikula)then ! Rij nahi dugu, ez Rii
      cycle
    endif

    call lortu_Rij_gainazal(f(i_partikula,1:3),f(auzokoen_indizeak(i),1:3),i_oinarria(1:3,3),Rij_gainazal) !(ri,rj,ni,Rij_gainazal)

    write(unit=43,fmt="(6f20.14)")  f(i_partikula,1:2),-2.0_dp,Rij_gainazal(1:3) !soilik bektore bakar bat da (z=-2 planoan marraztuko dugu)

    !-----------------------------------------------------
    ! lortu ij gainazal_gradientea
    call lortu_gainazal_gradientea(g_1,Rij_gainazal,i_oinarria,gainazal_gradientea) !(g_1,Rij_gainazal,iren_oinarria,gainazal_gradientea)

    !write(unit=45,fmt="(6f20.14)")  f(i_partikula,1:2),0.0_dp,gainazal_gradientea(1:2),0.0_dp !2D denez, marrazteko z=0 gehitu (z=0 planoan marraztuko dugu)
    gradientea=gradientea+gainazal_gradientea    

  enddo

    write(unit=45,fmt="(6f20.14)")  f(i_partikula,1:2),0.0_dp,gradientea(1:2),0.0_dp !2D denez, marrazteko z=0 gehitu (z=0 planoan marraztuko dugu)

!-----------------------------------------------------
! lortu abiadura bektore erlatiboak sistema lokal bakoitzeko

  ui(:)=0.2_dp
  uj(:)=0.14_dp
  write(unit=44,fmt=*)
  write(unit=44,fmt=*)
  do i=1,size(auzokoen_indizeak,1)
    if (auzokoen_indizeak(i)==i_partikula)then ! Uij nahi dugu, ez Uii
      cycle
    endif
    
    call i_ren_auzokoak(f,f_sare,auzokoen_indizeak(i),j_auzokoen_indizeak) !(p_guztiak,zs_hiztegia,indizea,auzokoen_indizeak)
    call lortu_i_ren_sistema_lokala(f,auzokoen_indizeak(i),j_auzokoen_indizeak,j_oinarria) !(p_guztiak,indizea,iren_auzo,iren_oinarria)
    call lortu_Uij_gainazal(f(i_partikula,1:3),f(auzokoen_indizeak(i),1:3),ui,uj,i_oinarria,j_oinarria,Uij_gainazal) !(ri,rj,ui,uj,iren_oinarria,jren_oinarria,Uij_gainazal)

    write(unit=44,fmt="(2i5,a)") auzokoen_indizeak(i),i_partikula,"<<<<<--j,i"
    write(unit=44,fmt="(6f20.14)")  f(auzokoen_indizeak(i),1:3),Uij_gainazal(1:3) !soilik bektore bakar bat da (partikula bakoitzean marraztuko dugu)
  enddo





!-----------------------------------------------------

enddo!-----i_partikula ugariko loop-a itxi

!-----------------------------------------------------
! gnuplot komandoak guztia marrazteko

! ---> komandoak unit 42 <---
! m*2 gauza gorde ditut, auzoko eta bektoreak tartekatzen baitira fitxategian, beraz do-an kontutan hartu

  write(unit=52,fmt="(a,i5,a)") 'splot "datuBektoreak.dat" index', 0,' using 1:2:3 pt 7 ps 1 notitle,\'           ! puntu hodeia marraztu
  !write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', 1,' using 1:2:3 pt 7 ps 1 notitle,\'

  do i=1,i_kopurua*2,2 ! m*2 gauza, binaka kontatuz, ze berruan ya daukagu i+1 indizea          
    !write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', i,' using 1:2:3 pt 7 ps 1 notitle,\'
    write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', i,' using 1:2:3 pt 7 ps 1 notitle,\'               ! auzokoak marraztu
    write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', i+1,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "black" notitle,\'  ! bektoreak marraztu
  enddo

  !write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', 13-1,' using 1:2:3 pt 7 ps 1 notitle'
  write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', i_kopurua*2-1,' using 1:2:3 pt 7 ps 1 notitle,\'          
  write(unit=52,fmt="(a,i5,a)") '"datuBektoreak.dat" index', i_kopurua*2,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "black" notitle,\' ! azkena ",\" izan behar orain segitzeko


! ---> komandoak unit 43 <---
!  i_kopurua*2+1 -etik hasi behar -> EZ, ze beste fitxategi bat da
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
  
  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
    write(unit=52,fmt="(a,i5,a)") '"datuBektoreak2.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "blue" notitle,\'  ! posizio erlatiboak marraztu
  enddo

  write(unit=52,fmt="(a,i5,a)") '"datuBektoreak2.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "blue" notitle,\'  ! azkena ",\" izan behar segitzeko

! ---> komandoak unit 44 <---
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
  
  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
    write(unit=52,fmt="(a,i5,a)") '"datuBektoreak3.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "purple" notitle,\'  ! posizio erlatiboak marraztu
  enddo

  write(unit=52,fmt="(a,i5,a)") '"datuBektoreak3.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "purple" notitle,\'  ! azkena ",\" izan behar segitzeko

! ---> komandoak unit 45 <---
!  ez dut hasierako lerroa behar goian baitago, eta soilik fitxategi hasieran jarri behar baita
  
  do i=0,i_kopurua-1 !0-tik hasi behar index-erako
    write(unit=52,fmt="(a,i5,a)") '"datuBektoreak4.dat" index', i,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "turquoise" notitle,\'  ! posizio erlatiboak marraztu
  enddo

  write(unit=52,fmt="(a,i5,a)") '"datuBektoreak4.dat" index', i_kopurua,' using 1:2:3:4:5:6 w vectors lw 2 lt rgb "turquoise" notitle'  ! azkena ",\" ez izateko
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


endprogram probatu_bektoreak
