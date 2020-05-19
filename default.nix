{ nixpkgs ? (import simplir/nixpkgs.nix {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak;
  inherit (nixpkgs.stdenv) lib;

  all-cabal-hashes =
    let rev = "7482ee903ffef3c31b57bcdea07d455052557d38";
    in {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "26rnyxqmr93ahml0fjfa6hmjpmx8sbpfdr52krd3sd6ic9n5p5ix";
    };

  localDir = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  simplirNix = import ./simplir { inherit nixpkgs; };

  haskellOverrides = self: super:
    let
      trecCarPackages = {
        trec-car-types       = dontCheck (self.callCabal2nix "trec-car-types" (localDir ./trec-car-types) {});
        trec-car-tools       = dontCheck (self.callCabal2nix "trec-car-tools" (localDir ./trec-car-tools) {});
      };
    in trecCarPackages // { inherit trecCarPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc865.override {
    overrides = lib.composeExtensions simplirNix.haskellOverrides haskellOverrides;
  };
in {
  pkgs = nixpkgs;
  inherit haskellPackages haskellOverrides;
  inherit (haskellPackages) trecCarPackages;
  inherit (simplirNix) simplirPackages trec-eval;
  env = haskellPackages.ghcWithHoogle (pkgs: builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages);
  binaries = nixpkgs.symlinkJoin {
    name = "trec-car-binaries";
    paths = builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages;
  };
}
