{
  inputs = {
    nixpkgs.url = "nixpkgs";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
            cargo # day 14
          ];
        };
      });
}
