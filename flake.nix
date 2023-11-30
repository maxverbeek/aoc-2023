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
          buildInputs = with pkgs; [ inputgetter kotlin ];
        };
      });
}
