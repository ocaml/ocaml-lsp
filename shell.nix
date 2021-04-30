let
  pkgs = (import <nixpkgs> { });
  lsp = (import ./default.nix { });
  inherit (pkgs) stdenv lib;
in with lsp;

pkgs.mkShell {
  inputsFrom = [ jsonrpc lsp ocaml-lsp-server ];
  buildInputs = (with pkgs; [ yarn nodejs-14_x gnumake ]) ++ (with opam; [
    # dev
    cinaps
    menhir
    ppx_yojson_conv

    # test
    ppx_expect
    ocamlformat
  ]);
}
