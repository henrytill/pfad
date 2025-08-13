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
          compiler ? "ghc984",
          doCheck ? true,
        }:
        let
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
          src = builtins.path {
            path = ./.;
            name = "pfad-src";
          };
          flags = "";
          pfad_ = call compiler "pfad" src flags { };
          doctest = pkgs.haskell.packages.${compiler}.doctest;
        in
        pkgs.haskell.lib.overrideCabal pfad_ (as: {
          inherit doCheck;
          isExecutable = true;
          isLibrary = true;
          testToolDepends = [ doctest ];
          preHaddock = ''
            ghc-pkg --package-db=$packageConfDir list
            make PACKAGE_DB=$packageConfDir test
          '';
        });
      overlay = final: prev: { pfad = makePfad final { }; };
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
