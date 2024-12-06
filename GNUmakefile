.SUFFIXES:

.PHONY: all
all: build

.PHONY: build
build:
	cabal v2-build all --enable-tests

.PHONY: test
test:
	cabal v2-repl --build-depends=QuickCheck --with-ghc=doctest
