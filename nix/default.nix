# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, fetchFromGitHub, opam2nix }:
let
  strings = pkgs.lib.strings;
  ourSrc = let ignores = pkgs.lib.strings.fileContents ../.gitignore
             + builtins.foldl' (acc: e: acc + "\n" + e) "\n"
                 [ "nix" "shell.nix" "default.nix" ];
           in pkgs.nix-gitignore.gitignoreSourcePure ignores ../.;
  argsBase = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_14) ocaml;
    src = {
      lsp = ourSrc;
      jsonrpc = ourSrc;
      ocaml-lsp-server = ourSrc;
    };
  };
  argsResolve = argsBase // {
    selection = ./opam-selection.nix;
  };
  argsBuild = argsBase // {
    selection = (self:
      let selection = import ./opam-selection.nix self;
      in self.lib.attrsets.recursiveUpdate selection {
        selection.dune.buildInputs = self.lib.optional self.pkgs.stdenv.isDarwin (with self.pkgs.darwin.apple_sdk.frameworks; [
          Foundation
          CoreServices
        ]);
      });
  };
  opam-selection = opam2nix.build argsBuild;
  localPackages = let contents = builtins.attrNames (builtins.readDir ../.);
  in builtins.filter (strings.hasSuffix ".opam") contents;
  resolve = opam2nix.resolve argsResolve (localPackages ++ [
    # dev deps
    "cinaps"
    "menhir"
    "ppx_yojson_conv"
    "ocamlfind=1.9.3"

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
