let
  pkgs = (import <nixpkgs> { });
  local = (import ./default.nix { });
  ocamlformat = (import ./ocamlformat.nix);
  inherit (pkgs) stdenv lib;
in with local;

pkgs.mkShell {
  buildInputs = (with pkgs; [
    yarn
    ocaml # until dune allows to avoid ocaml
    nodejs-14_x
    dune_3
  ]) ++ [ocamlformat];
}
