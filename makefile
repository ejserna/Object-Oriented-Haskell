default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Scanner.x
	happy Parser.y
	ghc -o ObjectiveOrientedHaskell Scanner.hs Parser.hs


prueba1:
	cat Tests/1.txt | ./ObjectiveOrientedHaskell


clean:
	rm -f *.hs
	rm -f *.hi
	rm -f *.o
	rm -f *.info
	rm -f ObjectiveOrientedHaskell
