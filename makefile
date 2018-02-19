default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Scanner.x
	happy Parser.y
	ghc -o ObjectiveOrientedHaskell Scanner.hs Parser.hs


prueba1:
	cat Tests/1.txt | ./ObjectiveOrientedHaskell

install_dependencies:
	cabal install alex
	cabal install happy
	cabal install Decimal
	cabal install pretty-show

clean:
	rm -f Parser.hs
	rm -f Scanner.hs
	rm -f *.hi
	rm -f *.o
	rm -f *.info
	rm -f ObjectiveOrientedHaskell
