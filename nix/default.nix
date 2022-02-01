# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, fetchFromGitHub, opam2nix }:
let
  strings = pkgs.lib.strings;
  ocamlformat = fetchFromGitHub {
		owner = "ocaml-ppx";
		repo = "ocamlformat";
		rev = "93a6b2f46cf31237c413c1d0ac604a8d69676297";
		sha256 = "sha256-2MFiuLa3pLdT0LYVFYjI4ocNsVcEpeWiyrCX2Aknh/g=";
  };
  ourSrc = let ignores = pkgs.lib.strings.fileContents ../.gitignore
             + builtins.foldl' (acc: e: acc + "\n" + e) "\n"
                 [ "nix" "shell.nix" "default.nix" ];
           in pkgs.nix-gitignore.gitignoreSourcePure ignores ../.;
  args = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_13) ocaml;
    selection = ./opam-selection.nix;
    src = {
      ocamlformat-rpc-lib = ocamlformat;
      ocamlformat-rpc = ocamlformat;
      lsp = ourSrc;
      jsonrpc = ourSrc;
      ocaml-lsp-server = ourSrc;
    };
  };
  opam-selection = opam2nix.build args;
  localPackages = let contents = builtins.attrNames (builtins.readDir ../.);
  in builtins.filter (strings.hasSuffix ".opam") contents;
  resolve = opam2nix.resolve args (localPackages ++
    ["${ocamlformat}/ocamlformat-rpc-lib.opam" "${ocamlformat}/ocamlformat-rpc.opam"] ++ [
    # dev deps
    "cinaps"
    "menhir=20211128"
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
