


all: main


main: Main.hs CExtras.hs MathExtras.hs cExtras.c		
	ghc Main.hs cExtras.c -o main -O3  -lm

CExtras.hs: CExtras.chs 
	c2hs CExtras.chs

clean: 
	rm *.hi
	rm *.o 
	rm main