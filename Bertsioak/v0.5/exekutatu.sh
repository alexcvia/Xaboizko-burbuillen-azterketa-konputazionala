#!/bin/bash

echo konpilatzen
ifort -o probatu_funtzioak tipos.f90 parametroak.f90 funtzioak.f90 probatu_funtzioak.f90 

echo bidali.sh aldatzen
vim -E -s bidali.sh <<EOF
:22d
:wq
EOF
sed -i '/grep -v/a ./probatu_funtzioak' bidali.sh   #<<<----- aldatu bidaliko den lana

echo lana bidaltzen
lanIzena=$(qsub bidali.sh)                          #lana bidali eta lanEgoera aldagaian sartu behar bere id-a
lanId=`echo $lanIzena | awk -F' ' '{print $3}'`
lanEgoera=`qstat | grep $lanId`


echo lanaren egoera ikusten
i=0
while [ -n "$lanEgoera" ]; do
  sleep 2s
  ((i+=2))
  echo "$i"s itxaron dira
  lanEgoera=`qstat | grep $lanId`
done

echo lanaren emaitza irudikatzen
sleep 1s
gnuplot -persist <<-EOFMarker
    plot "datufuntzio.dat" using 1:2 pt 7 ps 1
EOFMarker



exit
