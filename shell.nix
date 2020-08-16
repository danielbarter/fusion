{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cassava, containers
      , cryptohash-md5, directory, fftw, JuicyPixels, random, stdenv
      , vector
      }:
      mkDerivation {
        pname = "fusion";
        version = "0.0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring cassava containers cryptohash-md5 directory
          JuicyPixels random vector
        ];
        executableSystemDepends = [ fftw ];
        homepage = "https://github.com/danielbarter/fusion";
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
