let
  pkgs = (import <nixpkgs> { });
  local = (import ./default.nix { });
  inherit (pkgs) stdenv lib;
  ocamlformat = (import ./ocamlformat.nix);
in with local;

pkgs.mkShell {
  inputsFrom = [ jsonrpc lsp ocaml-lsp-server ];
  buildInputs = (with pkgs; [
    yarn
    nodejs-14_x
    gnumake
  ]) ++ [ocamlformat] ++ (with opam; [
    # dev
    cinaps
    menhir
    ppx_yojson_conv

    # test
    ppx_expect
    ocamlformat-rpc
  ]);
}
