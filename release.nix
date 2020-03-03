
{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> { };

in
  { min-max-pqueue = pkgs.haskellPackages.callPackage ./default.nix { inherit compiler; };
  }
