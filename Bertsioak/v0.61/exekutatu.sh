#!/bin/bash

echo konpilatzen
#ifort -o probatu_muga tipos.f90 parametroak.f90 auzokoak.f90 funtzioak.f90 mcf_matrices.f90 lokala.f90 bektoreak.f90 prestaketa.f90 indarrak.f90 muga.f90 probatu_muga.f90
ifort -o marraztu tipos.f90 parametroak.f90 marraztu.f90  #<---- gnuploten emaitz ez bukatuak marraztu nahi baditut

rm erroreenLog.dat  #<--- erreseteatu erroreenLog-a

echo bidali.sh aldatzen
vim -E -s bidali.sh <<EOF
:22d
:wq
EOF
#sed -i '/grep -v/a ./probatu_muga' bidali.sh   #<<<----- aldatu bidaliko den lana
sed -i '/grep -v/a ./marraztu' bidali.sh   #<<<----- aldatu bidaliko den lana

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

echo ikusten ea errorerik dagoen erroreenLog-an
sleep 1s
if [[ -s erroreenLog.dat ]]; then
  cat erroreenLog.dat
  exit
fi
echo ez dago errorerik

#exit
echo lanaren emaitza irudikatzen
sleep 1s
gnuplot -persist <<-EOFMarker
  load "gnuplotKomandoak.dat"
EOFMarker



exit
