all: numeric

numeric: Main.hs
	cpp Main.hs > Main_preprocessed.hs
	ghc Main_preprocessed.hs -o main

ascii: Main.hs
	cpp -DASCII_IO Main.hs > Main_preprocessed.hs
	ghc Main_preprocessed.hs -o main

clear:
	rm -f main
	rm -f Main_preprocessed.hs