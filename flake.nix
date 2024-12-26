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
      overlay = final: prev: { };
      makePfad =
        pkgs:
        {
          compiler ? "ghc982",
          doCheck ? true,
        }:
        let
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
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
        });
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
        packages.pfad = makePfad pkgs { };
        packages.default = self.packages.${system}.pfad;
      }
    );
}
