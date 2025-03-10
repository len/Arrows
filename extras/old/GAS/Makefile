all: gas.so

gas.so: *.c
	gcc -O3 -ffast-math -fPIC -shared -I/usr/include/openblas -lopenblas *.c -o gas.so
