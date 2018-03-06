ARCH?=1.txt

default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Scanner.x
	happy Parser.y
	ghc -o ObjectiveOrientedHaskell DataTypes.hs SymbolTable.hs Expression.hs ClassSymbolTable.hs TypeChecker.hs Scanner.hs Parser.hs


pruebaClases:
	cat Tests/pruebaClases.txt | ./ObjectiveOrientedHaskell

pruebaVariables:
	cat Tests/pruebaVariables.txt | ./ObjectiveOrientedHaskell

prueba: 
	cat Tests/$(ARCH) | ./ObjectiveOrientedHaskell

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
	rm -f DS_Store
