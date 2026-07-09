# Portable build: alex/happy/ghc are resolved from PATH, with fallbacks to
# the usual ghcup/cabal/stack install locations. Override with e.g. `make ALEX=/path/to/alex`.
TOOLDIRS = $(HOME)/.ghcup/bin $(HOME)/.cabal/bin $(HOME)/.local/bin
TOOLPATH = $(HOME)/.ghcup/bin:$(HOME)/.cabal/bin:$(HOME)/.local/bin

ALEX  ?= $(shell command -v alex  2>/dev/null || \
           ls $(addsuffix /alex,$(TOOLDIRS))  2>/dev/null | head -1)
HAPPY ?= $(shell command -v happy 2>/dev/null || \
           ls $(addsuffix /happy,$(TOOLDIRS)) 2>/dev/null | head -1)
GHC   ?= $(shell command -v ghc   2>/dev/null || \
           ls $(addsuffix /ghc,$(TOOLDIRS))   2>/dev/null | head -1)

SRCS = OrderedMap.hs CodeGen/MemoryLimits.hs CodeGen/Quadruple.hs DataTypes.hs \
       CodeGen/CodeGenDataTypes.hs CodeGen/ExpressionOptimizer.hs Analysis/SymbolTable.hs \
       Analysis/Semant.hs Analysis/Expression.hs Analysis/ClassSymbolTable.hs VirtualMachine.hs \
       CodeGen/ExpressionCodeGen.hs CodeGen/CodeGen.hs CodeGen/MemoryAllocator.hs \
       Analysis/TypeChecker.hs Analysis/Scanner.hs Analysis/Parser.hs

default: ooh

ooh: Analysis/Scanner.hs Analysis/Parser.hs
	$(GHC) -O2 -o ooh $(SRCS)

Analysis/Scanner.hs: Analysis/Scanner.x
	@test -n "$(ALEX)" || { echo "error: alex not found. Install it (cabal install alex) or pass ALEX=/path/to/alex"; exit 1; }
	$(ALEX) Analysis/Scanner.x

Analysis/Parser.hs: Analysis/Parser.y
	@test -n "$(HAPPY)" || { echo "error: happy not found. Install it (cabal install happy) or pass HAPPY=/path/to/happy"; exit 1; }
	$(HAPPY) Analysis/Parser.y

# Installs the GHC toolchain via ghcup if cabal is missing (Linux, macOS,
# FreeBSD, WSL — on native Windows use https://www.haskell.org/ghcup/ manually),
# then installs the build tools and library dependencies.
install_dependencies:
	@if ! command -v cabal >/dev/null 2>&1 && [ ! -x "$(HOME)/.ghcup/bin/cabal" ]; then \
		echo "==> cabal not found; installing GHC toolchain via ghcup"; \
		command -v curl >/dev/null 2>&1 || { echo "error: curl is required to bootstrap ghcup"; exit 1; }; \
		curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
			BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh; \
	fi
	@PATH="$(TOOLPATH):$$PATH"; export PATH; \
	cabal update && \
	cabal install alex happy --overwrite-policy=always && \
	cabal install --lib Stack Decimal pretty-show pretty-terminal ordered-containers either \
		containers unordered-containers hashable mtl transformers array bytestring text \
		--allow-newer=Stack:deepseq
	@echo '==> Done. If your shell cannot find ghc/cabal, add this to your profile: source ~/.ghcup/env'

clean:
	rm -f Analysis/Parser.hs Analysis/Scanner.hs
	rm -f *.hi *.o *.info Analysis/*.hi Analysis/*.o CodeGen/*.hi CodeGen/*.o
	rm -f ooh
	rm -f .DS_Store

.PHONY: default install_dependencies clean
