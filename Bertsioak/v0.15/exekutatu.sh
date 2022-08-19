#!/bin/bash

echo konpilatzen
ifort -o probatu_auzokoak tipos.f90 parametroak.f90 auzokoak.f90 probatu_auzokoak.f90 

echo bidali.sh aldatzen
vim -E -s bidali.sh <<EOF
:22d
:wq
EOF
sed -i '/grep -v/a ./probatu_auzokoak' bidali.sh   #<<<----- aldatu bidaliko den lana

#exit     #<<<----jarri bakarrik konpilatu nahi baduzu

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

#exit     #<<<----jarri ez marrazteko

echo lanaren emaitza irudikatzen
sleep 1s
gnuplot -persist <<-EOFMarker
  load "gnuplotKomandoak.dat"
EOFMarker



exit
