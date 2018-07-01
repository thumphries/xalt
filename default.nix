{ nixpkgs, compiler ? "ghc822" }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          gi-dbusmenugtk3 = pkgs.haskell.lib.addPkgconfigDepend haskellPackagesNew.gi-dbusmenugtk3 pkgs.gtk3;
          taffybar = haskellPackagesOld.taffybar.overrideDerivation (drv: {
            strictDeps = true;
            src = pkgs.fetchFromGitHub {
              owner = "taffybar";
              repo = "taffybar";
              rev = "2f2e2aa0ccb515883023226d744d80f4f901e6b1";
              sha256 = "00y18mbxr7qyjmj877b4h0g2b7c8mfq8wsy488mjgq7kmpq6ffvk";
            };

#             src = pkgs.fetchFromGitHub {
#                owner = "taffybar";
#                repo = "taffybar";
#                rev = "v2.1.1";
#                sha256 = "12g9i0wbh4i66vjhwzcawb27r9pm44z3la4693s6j21cig521dqq";
#             };
          });
        };
      };
    };
  };

  pkgs = { config = config; } // nixpkgs;
in
  pkgs.haskell.packages.${compiler}.callPackage ./xalternative.nix { }
