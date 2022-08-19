#!/bin/bash

echo exekutatzen

#fort -o probatu_funtzioak tipos.f90 parametroak.f90 funtzioak.f90 probatu_funtzioak.f90 

vim -E -s bidali.sh <<EOF
:22d
:wq
EOF

sed -i '/grep -v/a ./probatu_funtzioak' bidali.sh

#bidali.sh

#gnuplot

exit
