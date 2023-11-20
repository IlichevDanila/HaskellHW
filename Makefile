all: numeric

numeric: Main.hs
	cpp Main.hs > Main_preprocessed.hs
	ghc Main_preprocessed.hs -o interpreter
	rm Main_preprocessed.hs
	rm Main_preprocessed.hi
	rm Main_preprocessed.o

ascii: Main.hs
	cpp -DASCII_IO Main.hs > Main_preprocessed.hs
	ghc Main_preprocessed.hs -o interpreter
	rm Main_preprocessed.hs
	rm Main_preprocessed.hi
	rm Main_preprocessed.o

clear:
	rm -f interpreter
	rm -f Main_preprocessed.hs
	rm -f *.hi
	rm -f *.o