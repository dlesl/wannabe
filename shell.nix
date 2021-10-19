{ pkgs ? import ./pkgs.nix }:

with pkgs;

mkShell {
  buildInputs = [
    bashInteractive
    erlang
    erlang-ls
    rebar3
    leiningen
    nodejs_latest
  ];
}
