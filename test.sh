#!/bin/bash

make clean
make all

printf 'type   func   n   time \n' > result.data


for i in 100 200 300 400 500 700 800 900 1100 1300 1500 ;
do
	printf 'basic  mm ' >> result.data
	out/main_mm_basic "$i" >> result.data
	printf 'basic  gauss  ' >> result.data
	out/main_gauss_basic "$i" >> result.data
	printf 'coarray  mm  ' >> result.data
	cafrun -np 4 out/main_mm_coarray "$i" >> result.data
	#printf 'coarray   gauss ' >> result.data
	#cafrun -np 4 out/main_gauss_coarray "$i" >> result.data
done
