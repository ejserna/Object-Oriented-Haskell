ARCH?=1.txt

default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Analysis/Scanner.x
	happy Analysis/Parser.y
	ghc -o ObjectiveOrientedHaskell DataTypes.hs CodeGen/ExpressionOptimizer.hs Analysis/SymbolTable.hs Analysis/Expression.hs Analysis/ClassSymbolTable.hs CodeGen/Quadruple.hs CodeGen/MemoryAllocator.hs Analysis/TypeChecker.hs Analysis/Scanner.hs Analysis/Parser.hs


pruebaClases:
	cat Tests/pruebaClases.txt | ./ObjectiveOrientedHaskell

pruebaVariables:
	cat Tests/pruebaVariables.txt | ./ObjectiveOrientedHaskell

pruebaFibo:
	cat Tests/fibonacciRecursivo.txt | ./ObjectiveOrientedHaskell

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
