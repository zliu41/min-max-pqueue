{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, criterion, hedgehog
      , integer-logarithms, random, stdenv
      }:
      mkDerivation {
        pname = "min-max-pqueue";
        version = "0.1.0.1";
        src = ./.;
        libraryHaskellDepends = [ base containers ];
        testHaskellDepends = [ base containers hedgehog ];
        benchmarkHaskellDepends = [
          base containers criterion integer-logarithms random
        ];
        homepage = "https://github.com/zliu41/min-max-pqueue";
        description = "Double-ended priority queues";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
