.SUFFIXES:

CABAL = cabal
DOCTEST = doctest
ALL_DOCTEST_FLAGS = $(DOCTEST_FLAGS)

ifdef PACKAGE_DB
ALL_DOCTEST_FLAGS += -package-db=$(PACKAGE_DB) -package QuickCheck
else ifdef PACKAGE_ENV
ALL_DOCTEST_FLAGS += -package-env=$(PACKAGE_ENV)
endif

.PHONY: all
all: build

.PHONY: build
build:
	$(CABAL) v2-build all

.PHONY: test
test:
	$(DOCTEST) src/Data/PFAD/Ch01.hs $(ALL_DOCTEST_FLAGS)
	$(DOCTEST) src/Data/PFAD/Ch02.hs $(ALL_DOCTEST_FLAGS)

.PHONY: clean
clean:
	$(CABAL) v2-clean
