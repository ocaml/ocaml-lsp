{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    git-subrepo-src = {
      url =
        "github:rgrinberg/git-subrepo?rev=8fb6be3fb1500ab845081fc26ecdb950e9c0438c";
      flake = false;
    };
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs, ... }@inputs:
    let package = "ocaml-lsp-server";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        localPackages = {
          jsonrpc = null;
          lsp = null;
        };
        devPackages = {
          menhir = null;
          ppx_yojson_conv = null;
          cinaps = null;
          ppx_expect = null;
          ocamlformat-rpc = null;
          ocamlfind = "1.9.2";
        };
        allPackages = localPackages // devPackages;
      in {
        legacyPackages = let
          scope =
            on.buildOpamProject { resolveArgs = { with-test = true; }; } package
            ./. (allPackages);
            overlay = final: prev: {
              git-subrepo =
                prev.git-subrepo.overrideAttr (old: {
                  src = inputs.git-subrepo-src;
                });
            };
        in scope.overrideScope' overlay;

        defaultPackage = self.legacyPackages.${system}.${package};

        devShell = pkgs.mkShell {
          nativeBuildInputs = let scope = self.legacyPackages.${system};
          in with pkgs;
          [
            # dev tools
            git-subrepo
            ocamlformat_0_21_0
            yarn
          ] ++ (builtins.map (s: builtins.getAttr s scope)
            (builtins.attrNames allPackages));
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
