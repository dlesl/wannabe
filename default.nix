{ pkgs ? import ./pkgs.nix }:
let
  nix-filter = import (pkgs.fetchFromGitHub {
    owner = "numtide";
    repo = "nix-filter";
    rev = "3c9e33ed627e009428197b07216613206f06ed80";
    sha256 = "19w142crrkywxynmyw4rhz4nglrg64yjawfkw3j91qwkwbfjds84";
  });
in with pkgs.beam_minimal.packages.erlang.beamPackages;
rebar3Relx {
  pname = "wannabe";
  version = "0.1.0";
  src = nix-filter {
    root = ./.;
    include = [
      "rebar.config"
      (nix-filter.inDirectory "src")
      (nix-filter.inDirectory "config")
      (nix-filter.inDirectory "priv")
    ];
  };
  releaseType = "release";
  profile = "prod";
  beamDeps = builtins.attrValues (import ./rebar-deps.nix {
    inherit (pkgs) fetchHex fetchgit fetchFromGitHub;
    builder = buildRebar3;
  });
}
