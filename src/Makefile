all: Sfl

Lex.hs : Lex.x
	alex Lex.x

Happy.hs : Happy.y
	happy Happy.y
    
Sfl : Main.hs
	ghc --make Main
    
clean:
	rm -f *.o *.hi
