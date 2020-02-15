# For use with nix-shell
{ nixpkgs ? (import ./simplir/nixpkgs.nix {}) }:

with nixpkgs;
let
  trec-eval = callPackage ./simplir/trec-eval.nix {};
in stdenv.mkDerivation rec {
  name = "mediawiki-annotate-develop";
  buildInputs = [
    curl binutils-unwrapped leveldb
    zlib icu icu.out expat lzma trec-eval kyotocabinet
    postgresql ghc cabal-install
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}
  '';
}
