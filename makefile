makefile :
part 1 : etudeerrquad.f90
	gfortran -c -fcheck=all etudeerrquad.f90
	gfortran -o exe.exe etudeerrquad.o
	./exe.exe
