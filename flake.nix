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
      hs-project =
        {
          pkgs,
          hp ? pkgs.haskellPackages,
          isShell ? false,
        }:
        (hp.developPackage {
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
        }).overrideAttrs {
			nativeBuildInputs = [ pkgs.makeWrapper pkgs.removeReferencesTo ];
			postInstall = ''wrapProgram $out/bin/chipi-chapa --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath [ pkgs.alsa-lib ]}'';
		};
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.platforms.unix;
      perSystem =
        { pkgs, ... }:
        {
          packages.default = hs-project { inherit pkgs; };
          devShells.default = hs-project {
            inherit pkgs;
            isShell = true;
          };
        };
    };
}
