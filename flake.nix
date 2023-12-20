{
  inputs = {
    nixpkgs.url = "nixpkgs";
    utils.url = "github:numtide/flake-utils";
    rustoverlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, utils, rustoverlay }:
    utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rustoverlay) ];
        pkgs = import nixpkgs { inherit overlays system; };
        rust-toolchain = pkgs.rust-bin.stable.latest;

        inputgetter = pkgs.writeScriptBin "getinput" ''
          outpath=''${1:-input.txt}
          daynum=$(expr $(basename $PWD) + 0)

          url="https://adventofcode.com/2023/day/''${daynum}/input"

          echo $url

          exec ${pkgs.curl}/bin/curl -o $outpath -H "cookie: $(cat ../.cookies.txt)" $url
        '';

      in {
        devShell = pkgs.mkShell {
          name = "devshell";
          buildInputs = with pkgs; [
            inputgetter
            kotlin # day 1
            gcc # day 2
            scala # day 3, day 5
            ghc # day 4, day 8
            erlang # day 6
            ocaml # day 7, day 9, day 11?
            swiftPackages.swift # day 10
            swiftPackages.Foundation # day 10
            lua # day 12
            elixir # day 13
            rust-toolchain.default # day 14
            nodePackages.ts-node # day 17
          ];
        };
      });
}
