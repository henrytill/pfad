.SUFFIXES:

.SHELLFLAGS += -e

CABAL = cabal
DOCTEST = doctest

ALL_DOCTEST_FLAGS = $(DOCTEST_FLAGS)

ifdef PACKAGE_DB
ALL_DOCTEST_FLAGS += -package-db=$(PACKAGE_DB) -package QuickCheck
else ifdef PACKAGE_ENV
ALL_DOCTEST_FLAGS += -package-env=$(PACKAGE_ENV)
endif

SRC =
SRC += src/Data/PFAD/Ch01.hs
SRC += src/Data/PFAD/Ch02.hs

all: build

build:
	$(CABAL) v2-build all

clean:
	$(CABAL) v2-clean

check test:
	@printf "Running doctests...\n"
	@for file in $(SRC); do \
	    printf "%s:\n" $$file; \
	    $(DOCTEST) $$file $(ALL_DOCTEST_FLAGS); \
	done

.PHONY: all build check clean test
