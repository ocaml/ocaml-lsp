{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };

  outputs = { self, flake-utils, opam-nix, opam-repository, nixpkgs, ... }@inputs:
    let
      package = "ocaml-lsp-server";
      overlay = final: prev: {
        ${package} = prev.${package}.overrideAttrs (_: {
          # Do not add share/nix-support, so that dependencies from
          # the scope don't leak into dependent derivations
          doNixSupport = false;
        });
        dune-release = prev.dune-release.overrideAttrs (_: {
          doCheck = false;
        });
        ocamlPackages = prev.ocamlPackages.overrideScope' (oself: osuper:
          let
            fixPreBuild = o: {
              propagatedBuildInputs = o.propagatedBuildInputs ++ [ oself.pp ];
              preBuild = ''
                rm -r vendor/csexp vendor/pp
              '';
            };
          in
          {
            dyn = osuper.dyn.overrideAttrs fixPreBuild;
            dune-private-libs = osuper.dune-private-libs.overrideAttrs fixPreBuild;
            dune-glob = osuper.dune-glob.overrideAttrs fixPreBuild;
            dune-action-plugin = osuper.dune-action-plugin.overrideAttrs fixPreBuild;
            dune-rpc = osuper.dune-rpc.overrideAttrs fixPreBuild;
            stdune = osuper.stdune.overrideAttrs fixPreBuild;
          });
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { overlays = [ overlay ]; inherit system; };
        inherit (pkgs.ocamlPackages) buildDunePackage;
        fast = rec {

          # these packages do not use opam2nix
          jsonrpc = buildDunePackage {
            pname = "jsonrpc";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = with pkgs.ocamlPackages; [ ];
            doCheck = false;
          };

          lsp = buildDunePackage {
            pname = "lsp";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              jsonrpc
              yojson
              stdune
              ppx_yojson_conv_lib
              uutf
            ];
            checkInputs = with pkgs.ocamlPackages; [ cinaps ppx_expect ];
            doCheck = false;
          };

          ocaml-lsp = buildDunePackage {
            pname = "ocaml-lsp";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            checkInputs = with pkgs.ocamlPackages; [ ppx_expect ];
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              ocamlc-loc
              octavius
              dune-build-info
              re
              dune-rpc
              chrome-trace
              dyn
              fiber
              xdg
              ordering
              spawn
              pp
              csexp
              ocamlformat-rpc-lib
              stdune
              yojson
              ppx_yojson_conv_lib
              uutf
              lsp
              odoc-parser
              merlin-lib
            ];
            doCheck = false;
          };
        };
        on = opam-nix.lib.${system};
        localPackages = {
          jsonrpc = "*";
          lsp = "*";
          ocaml-lsp-server = "*";
        };
        devPackages = {
          menhir = "*";
          ppx_yojson_conv = "*";
          cinaps = "*";
          ppx_expect = "*";
          ocamlfind = "1.9.2";
        };
        packagesFromNames = set:
          (builtins.map (s: builtins.getAttr s scope)
            (builtins.attrNames set));
        allPackages = localPackages // devPackages;
        scope =
          (
            let
              scope =
                on.buildOpamProject
                  {
                    repos = [ opam-repository ];
                    inherit pkgs;
                    resolveArgs = { with-test = true; };
                  }
                  package
                  ./.
                  (allPackages);
            in
            scope.overrideScope' overlay
          );
        opam2nixPackages = nixpkgs.lib.filterAttrs (name: value: builtins.hasAttr name localPackages) scope;
      in
      {
        packages =
          opam2nixPackages //
          rec {
            # we have a package without opam2nix for easy consumption for nix users
            default = pkgs.ocamlPackages.buildDunePackage {
              pname = package;
              version = "n/a";
              src = ./.;
              duneVersion = "3";
              buildInputs = with pkgs.ocamlPackages; [
                ocamlc-loc
                odoc-parser
                dune-build-info
                re
                dune-rpc
                chrome-trace
                dyn
                fiber
                xdg
                ordering
                spawn
                pp
                csexp
                ocamlformat-rpc-lib
                stdune
                yojson
                ppx_yojson_conv_lib
                uutf
                lsp
                merlin-lib
              ];
              propagatedBuildInputs = [ ];
              doCheck = false;
              buildPhase = ''
                runHook preBuild
                dune build ${package}.install --release ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
                runHook postBuild
              '';
            };

            ocaml-lsp = fast.ocaml-lsp;
          };

        devShells = {
          opam2nix = pkgs.mkShell {
            buildInputs = (with pkgs;
              [
                # dev tools
                ocamlformat_0_24_1
                yarn
                dune-release
              ]) ++ packagesFromNames devPackages;
            inputsFrom = packagesFromNames opam2nixPackages;
          };

          default = pkgs.mkShell {
            buildInputs = (with pkgs;
              [
                # dev tools
                ocamlformat_0_24_1
                yarn
                dune-release

                ocamlPackages.ppx_expect
                ocamlPackages.utop
                ocamlPackages.cinaps
                ocamlPackages.ppx_yojson_conv
              ]);
            inputsFrom = [ fast.ocaml-lsp fast.jsonrpc fast.lsp ];
          };
        };
      });
}
