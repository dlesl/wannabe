{ pkgs ? import <nixpkgs> { } }:
let release = pkgs.callPackage ./default.nix { };
in pkgs.dockerTools.buildImage {
  name = "wannabe";
  tag = "latest";
  contents = [ pkgs.bash pkgs.coreutils ];
  config.Cmd = [ "${release}/rel/wannabe/bin/wannabe" "-noshell" ];
}
