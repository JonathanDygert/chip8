{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, finite-typelits, hpack
      , lens, MonadRandom, mtl, random, sdl2, stdenv, vector
      , vector-sized
      }:
      mkDerivation {
        pname = "chip8";
        version = "0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base finite-typelits lens MonadRandom mtl vector-sized
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          base bytestring lens mtl random sdl2 vector vector-sized
        ];
        prePatch = "hpack";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
