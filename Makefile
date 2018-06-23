.PHONY: all


IFORTFLAGS=-coarray -funroll-all-loops -WB -std08 -module . -implicitnone -fpp -warn all -pedantic -fpp -Iout/ -g
GFORTFLAGS=-O3 -ffree-form -std=f2008 -fimplicit-none -Wall -pedantic -fbounds-check -cpp

all: out/main

out/main:
	mkdir -p out 
	gfortran $(GFORTFLAGS) src/library.f90 src/test.f90 src/main.f90 -o $@_mm_basic -D "BASIC=1"
	gfortran $(GFORTFLAGS)  src/library.f90 src/test.f90 src/main2.f90 -o $@_gauss_basic -D "BASIC=1"
	caf $(GFORTFLAGS)  src/library.f90 src/test.f90 src/main.f90 -o $@_mm_coarray -D "COARRAY=1"
	caf $(GFORTFLAGS)  src/library.f90 src/test.f90 src/main2.f90 -o $@_gauss_coarray -D "COARRAY=1"
clean:
	rm -rf out
	mkdir out