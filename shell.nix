let
  pkgs = (import <nixpkgs> { });
  local = (import ./default.nix { });
  strings = pkgs.lib.strings;
  inherit (pkgs) stdenv lib;
in with local;

pkgs.mkShell {
  inputsFrom = [ jsonrpc lsp ocaml-lsp-server ];
  buildInputs = (with pkgs; [
    yarn
    nodejs-14_x
    gnumake
    ocamlformat
    ocamlPackages.ocaml-lsp
  ]) ++ (with opam; [
    # dev
    cinaps
    menhir
    ppx_yojson_conv

    # test
    ppx_expect
    ocamlformat-rpc
  ]);
}
