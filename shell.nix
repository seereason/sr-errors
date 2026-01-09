{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, cereal, exceptions, lens, lib, mtl
      , safecopy, unexceptionalio-trans
      }:
      mkDerivation {
        pname = "sr-errors";
        version = "1.19";
        src = ./.;
        libraryHaskellDepends = [
          base cereal exceptions lens mtl safecopy unexceptionalio-trans
        ];
        homepage = "https://github.com/seereason/sr-errors";
        description = "Error set types";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
