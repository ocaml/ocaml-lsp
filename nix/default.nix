# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
let
  args = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_12) ocaml;
    selection = ./opam-selection.nix;
    src = ../.;
  };
  opam-selection = opam2nix.build args;
  localPackages = let
    strings = pkgs.lib.strings;
    contents = builtins.attrNames (builtins.readDir ../.);
  in builtins.filter (strings.hasSuffix ".opam") contents;
  resolve = opam2nix.resolve args (localPackages ++ [
    # dev deps
    "cinaps"
    "menhir"
    "ppx_yojson_conv"

    # test deps
    "ocamlformat=0.17.0"
    "ppx_expect"
  ]);

in {
  inherit (opam-selection) jsonrpc; # jsonrpc = opam-selection.jsonrpc
  inherit (opam-selection) lsp;
  inherit (opam-selection) ocaml-lsp-server;
  inherit resolve;
  opam = opam-selection;
}
