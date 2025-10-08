{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    { flake-parts, nixpkgs, ... }@inputs:
    let
      hs-project =
        {
          pkgs,
          isShell ? false,
        }:
        pkgs.haskellPackages.developPackage {
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
          packages.static = pkgs.haskell.lib.overrideCabal (hs-project { inherit pkgs; }) (old: {
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags = [
              "--ghc-option=-optl=-static"
              "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
              "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; doChecks = false; })}/lib"
            ];
          });
          devShells.default = hs-project {
            inherit pkgs;
            isShell = true;
          };
        };
    };
}
