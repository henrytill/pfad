{ mkDerivation, array, base, criterion, doctest, QuickCheck, stdenv
}:
mkDerivation {
  pname = "pfad";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ array base ];
  testHaskellDepends = [ base doctest QuickCheck ];
  benchmarkHaskellDepends = [ base criterion ];
  doBenchmark = true;
  description = "Work from Bird's \"Pearls of Functional Algorithm Design\"";
  license = stdenv.lib.licenses.publicDomain;
}
