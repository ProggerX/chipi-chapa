{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    {
      flake-parts,
      nixpkgs,
      self,
      ...
    }@inputs:
    let
      fixGHC =
        pkg:
        pkg.override {
          enableRelocatedStaticLibs = true;
          enableShared = false;
          enableDwarf = false;
        };
      hs-project =
        {
          pkgs,
		  hp ? pkgs.haskellPackages,
          isShell ? false,
        }:
        hp.developPackage {
          root = ./.;
          returnShellEnv = isShell;
          modifier =
            drv:
            pkgs.haskell.lib.addBuildTools drv (
              with pkgs;
              [
                cabal-install
                haskell-language-server
              ]
            );
        };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.platforms.unix;
      perSystem =
        { pkgs, ... }:
        {
          packages.default = hs-project { inherit pkgs; };
          packages.static = pkgs.haskell.lib.overrideCabal (hs-project { inherit pkgs; hp = (pkgs.haskellPackages.override { ghc = fixGHC pkgs.ghc; }); }) (old: {
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags = [
              "--ghc-option=-optl=-static"
              "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              "--extra-lib-dirs=${
                pkgs.libffi.overrideAttrs (old: {
                  dontDisableStatic = true;
                  doChecks = false;
                })
              }/lib"
            ];
          });
          devShells.default = hs-project {
            inherit pkgs;
            isShell = true;
          };
        };
    };
}
