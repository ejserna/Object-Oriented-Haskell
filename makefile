default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Analysis/Scanner.x
	happy Analysis/Parser.y
	ghc -o ObjectiveOrientedHaskell OrderedMap.hs CodeGen/MemoryLimits.hs CodeGen/Quadruple.hs DataTypes.hs CodeGen/CodeGenDataTypes.hs CodeGen/ExpressionOptimizer.hs Analysis/SymbolTable.hs Analysis/Semant.hs Analysis/Expression.hs Analysis/ClassSymbolTable.hs VirtualMachine.hs CodeGen/ExpressionCodeGen.hs CodeGen/CodeGen.hs CodeGen/MemoryAllocator.hs Analysis/TypeChecker.hs Analysis/Scanner.hs Analysis/Parser.hs


install_dependencies:
	cabal install alex
	cabal install happy
	cabal install Decimal
	cabal install pretty-show
	cabal install pretty-terminal
	cabal install ordered-containers
	cabal install either-5

clean:
	rm -f Parser.hs
	rm -f Scanner.hs
	rm -f *.hi
	rm -f *.o
	rm -f *.info
	rm -f ObjectiveOrientedHaskell
	rm -f DS_Store
