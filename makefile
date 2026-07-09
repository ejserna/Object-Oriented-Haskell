# Portable build: alex/happy/ghc are resolved from PATH, with fallbacks to
# the usual cabal/stack install locations. Override with e.g. `make ALEX=/path/to/alex`.
ALEX  ?= $(shell command -v alex  2>/dev/null || \
           ls $(HOME)/.cabal/bin/alex $(HOME)/.local/bin/alex 2>/dev/null | head -1)
HAPPY ?= $(shell command -v happy 2>/dev/null || \
           ls $(HOME)/.cabal/bin/happy $(HOME)/.local/bin/happy 2>/dev/null | head -1)
GHC   ?= ghc

SRCS = OrderedMap.hs CodeGen/MemoryLimits.hs CodeGen/Quadruple.hs DataTypes.hs \
       CodeGen/CodeGenDataTypes.hs CodeGen/ExpressionOptimizer.hs Analysis/SymbolTable.hs \
       Analysis/Semant.hs Analysis/Expression.hs Analysis/ClassSymbolTable.hs VirtualMachine.hs \
       CodeGen/ExpressionCodeGen.hs CodeGen/CodeGen.hs CodeGen/MemoryAllocator.hs \
       Analysis/TypeChecker.hs Analysis/Scanner.hs Analysis/Parser.hs

default: ooh

ooh: Analysis/Scanner.hs Analysis/Parser.hs
	$(GHC) -o ooh $(SRCS)

Analysis/Scanner.hs: Analysis/Scanner.x
	@test -n "$(ALEX)" || { echo "error: alex not found. Install it (cabal install alex) or pass ALEX=/path/to/alex"; exit 1; }
	$(ALEX) Analysis/Scanner.x

Analysis/Parser.hs: Analysis/Parser.y
	@test -n "$(HAPPY)" || { echo "error: happy not found. Install it (cabal install happy) or pass HAPPY=/path/to/happy"; exit 1; }
	$(HAPPY) Analysis/Parser.y

install_dependencies:
	cabal update
	cabal install alex happy
	cabal install --lib Stack-0.4.0 Decimal pretty-show pretty-terminal ordered-containers either-5

clean:
	rm -f Analysis/Parser.hs Analysis/Scanner.hs
	rm -f *.hi *.o *.info Analysis/*.hi Analysis/*.o CodeGen/*.hi CodeGen/*.o
	rm -f ooh
	rm -f .DS_Store

.PHONY: default install_dependencies clean
