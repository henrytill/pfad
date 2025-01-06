{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    let
      makePfad =
        pkgs:
        {
          compiler ? "ghc982",
          doCheck ? true,
        }:
        let
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
          doctest = pkgs.haskell.packages.${compiler}.doctest;
          flags = "";
          src = builtins.path {
            path = ./.;
            name = "pfad-src";
          };
          pfad_ = call compiler "pfad" src flags { };
        in
        pkgs.haskell.lib.overrideCabal pfad_ (as: {
          inherit doCheck;
          isExecutable = true;
          isLibrary = true;
          doHaddock = false;
          testToolDepends = [ doctest ];
          checkPhase = ''
            ghc-pkg --package-db=$packageConfDir list
            make PACKAGE_DB="$packageConfDir" test
          '';
        });
      overlay = final: prev: { pfad = makePfad prev.pkgs { }; };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages.pfad = pkgs.pfad;
        packages.default = self.packages.${system}.pfad;
      }
    );
}
