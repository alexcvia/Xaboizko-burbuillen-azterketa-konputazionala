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
lanIzena=$(qsub bidali.sh) #lana bidali eta lanEgoera aldagaian sartu bere id-a
#sleep 30s
lanId=`echo $lanIzena | awk -F' ' '{print $3}'`
lanEgoera=`qstat | grep $lanId`
#komodin="X"
#lanTextua=`echo $lanEgoera | awk '{print $lanEgoera}'` 
#lanTextua="$lanTextua $komodin" #lanEgoera string bihurtu eta "X" gehitu

#echo ">" "$lanIzena" 
#echo ">" "$lanId" 
#echo ">" "$lanTextua" 


echo lanaren egoera ikusten
i=0
while [ -n "$lanEgoera" ]; do
#  lanEgoera=$qstat
#  if [ -z "$lanEgoera" ]; then
#    break
#  fi
  sleep 2s
  ((i+=2))
  echo "$i"s itxaron dira
  lanEgoera=`qstat | grep $lanId`
#  lanEgoera=`echo $lanEgoera`
done

#gnuplot "plot 'datufuntzio.dat' u 1:2" 



exit
