.SUFFIXES:

GHC = ghc-9.8.2

.PHONY: all
all: build

.PHONY: build
build:
	cabal v2-build all -w $(GHC) --enable-tests

.PHONY: test
test: build
	cabal v2-exec -w $(GHC) -- cabal-docspec -w $(GHC)
