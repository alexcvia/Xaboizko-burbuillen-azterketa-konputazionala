#!/bin/bash

############################################################
# Simple script for qsub on cryst2. JJLS 170508

############################################################
# modify to suit your job
#####exe=/share/apps/dl_poly/dl_poly_2.19-ifc10-mpich2/execute/DLPOLY.X
# set the requested no. of cpus at the end of the next line

############################################################
# no modifications needed below this line

#$ -cwd
#$ -j y
#$ -S /bin/bash
#$ -V

# mpd.hosts file with a line machineX:ncpusX for each of the X nodes
# (does the grep do anything???)
grep -v `hostname` $TMP/machines | uniq -c | awk '{ print $2,":",$1}' | tr -d \ > mpd.hosts
./probatu_muga


# job commands
#rm mpd.hosts

exit 0
