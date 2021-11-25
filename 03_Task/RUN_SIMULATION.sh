#!/bin/bash
gfortran ./SOURCE/*.f90	-o ./OUT/analysis -J ./OUT
gfortran ./SOURCE_HOT/*.f90 -I ./OUT -o ./OUT/md_hot -J ./OUT
gfortran ./SOURCE_COLD/*.f90 -o ./OUT/md_cold -J ./OUT
./OUT/md_hot
./OUT/md_cold
./OUT/analysis
gnuplot ./SOURCE/Plot_Task3
