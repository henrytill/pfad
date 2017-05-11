{ mkDerivation, base, criterion, doctest, stdenv }:
mkDerivation {
  pname = "pfad";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest ];
  benchmarkHaskellDepends = [ base criterion ];
  withBenchmarkDepends = true;
  description = "Work from Bird's \"Pearls of Functional Algorithm Design\"";
  license = stdenv.lib.licenses.publicDomain;
}
