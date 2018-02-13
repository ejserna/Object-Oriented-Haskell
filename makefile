default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Scanner.x
	happy Parser.y
	ghc -o ObjectiveOrientedHaskell Scanner.hs Parser.hs

clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f ObjectiveOrientedHaskell
