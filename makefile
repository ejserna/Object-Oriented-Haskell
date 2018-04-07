ARCH?=1.txt

default:
	rm -f Parser.hs
	rm -f Scanner.hs
	alex Analysis/Scanner.x
	happy Analysis/Parser.y
	ghc -o ObjectiveOrientedHaskell CodeGen/Quadruple.hs DataTypes.hs CodeGen/CodeGenDataTypes.hs CodeGen/ExpressionOptimizer.hs Analysis/SymbolTable.hs Analysis/Semant.hs Analysis/Expression.hs Analysis/ClassSymbolTable.hs VirtualMachine.hs CodeGen/ExpressionCodeGen.hs CodeGen/CodeGen.hs CodeGen/MemoryAllocator.hs Analysis/TypeChecker.hs Analysis/Scanner.hs Analysis/Parser.hs


pruebaClases:
	cat Tests/pruebaClases.txt | ./ObjectiveOrientedHaskell

pruebaVariables:
	cat Tests/pruebaVariables.txt | ./ObjectiveOrientedHaskell 

pruebaFibo:
	cat Tests/fibonacciRecursivo.txt | ./ObjectiveOrientedHaskell

pruebaDisplays:
	cat Tests/displays.txt | ./ObjectiveOrientedHaskell

pruebaMamada:
	cat Tests/DeepAssignment.txt | ./ObjectiveOrientedHaskell

pruebaEasy:
	cat Tests/pruebaSencilla.txt | ./ObjectiveOrientedHaskell

pruebaFiboLoop:
	cat Tests/fiboIterativo.txt | ./ObjectiveOrientedHaskell

prueba: 
	cat Tests/$(ARCH) | ./ObjectiveOrientedHaskell

install_dependencies:
	cabal install alex
	cabal install happy
	cabal install Decimal
	cabal install pretty-show
	cabal install Stack
	cabal install pretty-terminal

clean:
	rm -f Parser.hs
	rm -f Scanner.hs
	rm -f *.hi
	rm -f *.o
	rm -f *.info
	rm -f ObjectiveOrientedHaskell
	rm -f DS_Store
