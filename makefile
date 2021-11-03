default:
	rm -f Parser.hs
	rm -f Scanner.hs
	/Users/eduardoserna/.cabal/bin/alex Analysis/Scanner.x
	/Users/eduardoserna/.cabal/bin/happy Analysis/Parser.y
	ghc -o ooh OrderedMap.hs CodeGen/MemoryLimits.hs CodeGen/Quadruple.hs DataTypes.hs CodeGen/CodeGenDataTypes.hs CodeGen/ExpressionOptimizer.hs Analysis/SymbolTable.hs Analysis/Semant.hs Analysis/Expression.hs Analysis/ClassSymbolTable.hs VirtualMachine.hs CodeGen/ExpressionCodeGen.hs CodeGen/CodeGen.hs CodeGen/MemoryAllocator.hs Analysis/TypeChecker.hs Analysis/Scanner.hs Analysis/Parser.hs


install_dependencies:
	cabal update
	# cabal install alex
	# cabal install happy
	cabal install --lib Stack-0.4.0
	cabal install --lib Decimal
	cabal install --lib pretty-show
	cabal install --lib pretty-terminal
	cabal install --lib ordered-containers
	cabal install --lib either-5

clean:
	rm -f Parser.hs
	rm -f Scanner.hs
	rm -f *.hi
	rm -f *.o
	rm -f *.info
	rm -f ooh
	rm -f DS_Store
