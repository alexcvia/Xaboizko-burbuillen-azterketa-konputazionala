program marraztu
use tipos
use parametroak
implicit none

integer:: i,j,k
real(kind=dp)                             :: zmin_plot,zmax_plot  ! marraztean ez ateratzeko irudiaren mugetatik

open(unit=52,action="write",status="replace",file="gnuplotKomandoak.dat")

!zmin_plot=1.0194947_dp
!zmax_plot=1.0213949_dp
!zmin_plot=1.0191567_dp
!zmax_plot=1.0250117_dp
!zmin_plot=1.0058744_dp
!zmax_plot=1.0200353_dp
!zmin_plot=1.0129226_dp
!zmax_plot=1.0219642_dp
zmin_plot=1.0129019_dp
zmax_plot=1.0252903_dp

  write(unit=52,fmt="(a)") 'set hidden3d'                                                                ! gnuplot prestatu
  write(unit=52,fmt="(a)") 'set xlabel "x"'                                                              ! gnuplot prestatu
  write(unit=52,fmt="(a)") 'set ylabel "y"'                                                              ! gnuplot prestatu
  write(unit=52,fmt="(a)") 'set zlabel "z"'                                                              ! gnuplot prestatu
  write(unit=52,fmt="(a,f12.7,a,f12.7,a)") 'set xrange [',muga_x0,':',muga_xf,']'                        ! gnuplot prestatu
  write(unit=52,fmt="(a,f12.7,a,f12.7,a)") 'set yrange [',muga_y0,':',muga_yf,']'                        ! gnuplot prestatu
  write(unit=52,fmt="(a,f12.7,a,f12.7,a)") 'set zrange [',zmin_plot,':',zmax_plot,']'                    ! gnuplot prestatu
  write(unit=52,fmt="(a)")                 'set cbrange [0.0:0.0001]'                                    ! 10**-4
  do i=0,2008-1 !0-tik hasi behar index-erako
    write(unit=52,fmt="(a,i5,a)") 'splot "datuMuga.dat" index', i,' using 1:2:3:4 pt 7 ps 1 palette notitle'  ! posizioak marraztu t bakoitzean
    !write(unit=52,fmt="(a,i5,a)") '"datuMuga4.dat" index', i,' using 1:2:3:4 pt 7 ps 1 palette notitle'  ! abiaduren moduluak  marraztu t bakoitzean
  enddo

close(unit=52)
endprogram marraztu
