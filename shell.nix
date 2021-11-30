{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  ghc = haskell.packages.ghc901.ghcWithPackages (p: with p; [ random ]);
in
stdenv.mkDerivation {
  name = "nyan";
  buildInputs = [ ghc ];
}
