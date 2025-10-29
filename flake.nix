{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    appimage.url = "github:ralismark/nix-appimage";
  };
  outputs =
    {
      flake-parts,
      nixpkgs,
      appimage,
      ...
    }@inputs:
    let
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
          packages.default = (hs-project { inherit pkgs; }).overrideAttrs {
            nativeBuildInputs = [
              pkgs.makeWrapper
              pkgs.removeReferencesTo
            ];
            postInstall = ''wrapProgram $out/bin/chipi-chapa --set LD_LIBRARY_PATH ${
              pkgs.lib.makeLibraryPath [ pkgs.alsa-lib ]
            }'';
          };
          packages.bin = (hs-project { inherit pkgs; }).overrideAttrs {
            nativeBuildInputs = [
              pkgs.patchelf
              pkgs.removeReferencesTo
            ];
            postInstall = ''patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $out/bin/chipi-chapa'';
          };
          # packages.appimage = appimage.lib.${pkgs.system}.mkAppImage {
          # 	program = "${hs-project { inherit pkgs; }}/bin/chipi-chapa";
          # };
          devShells.default = hs-project {
            inherit pkgs;
            isShell = true;
          };
        };
    };
}
