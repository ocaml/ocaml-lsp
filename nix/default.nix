# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, opam2nix }:
let
  strings = pkgs.lib.strings;
  args = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_12) ocaml;
    selection = ./opam-selection.nix;
    src = builtins.filterSource (path: type:
      let name = baseNameOf path;
      in if type == "directory" then
        name != ".git" && name != "_build" && name != "node_modules" && name
        != ".log"
      else
        (strings.hasSuffix ".opam" name || name == "unvendor.ml")) ../.;
  };
  opam-selection = opam2nix.build args;
  localPackages = let contents = builtins.attrNames (builtins.readDir ../.);
  in builtins.filter (strings.hasSuffix ".opam") contents;
  resolve = opam2nix.resolve args (localPackages ++ [
    # dev deps
    "cinaps"
    "menhir"
    "ppx_yojson_conv"

    # test deps
    "ppx_expect"
    "ocamlformat-rpc"
  ]);

in (builtins.listToAttrs (builtins.map (fname:
  let packageName = strings.removeSuffix ".opam" fname;
  in {
    name = packageName;
    value = builtins.getAttr packageName opam-selection;
  }) localPackages)) // {
    inherit resolve;
    opam = opam-selection;
  }
