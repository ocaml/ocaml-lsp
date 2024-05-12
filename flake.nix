{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, nixpkgs, ... }@inputs:
    let
      package = "ocaml-lsp-server";
      basePackage = {
        duneVersion = "3";
        version = "n/a";
        src = ./.;
      };
      overlay = final: prev: {
        ocaml-lsp = prev.ocaml-lsp.overrideAttrs (_: {
          # Do not add share/nix-support, so that dependencies from
          # the scope don't leak into dependent derivations
          doNixSupport = false;
        });
        dune-release =
          prev.dune-release.overrideAttrs (_: { doCheck = false; });
        ocamlPackages = prev.ocamlPackages.overrideScope' (oself: osuper:
          let
            fixPreBuild = o: {
              propagatedBuildInputs = o.propagatedBuildInputs ++ [ oself.pp ];
              preBuild = ''
                rm -r vendor/csexp vendor/pp
              '';
            };
          in {
            dyn = osuper.dyn.overrideAttrs fixPreBuild;
            dune-private-libs =
              osuper.dune-private-libs.overrideAttrs fixPreBuild;
            dune-glob = osuper.dune-glob.overrideAttrs fixPreBuild;
            dune-action-plugin =
              osuper.dune-action-plugin.overrideAttrs fixPreBuild;
            dune-rpc = osuper.dune-rpc.overrideAttrs fixPreBuild;
            stdune = osuper.stdune.overrideAttrs fixPreBuild;
          });
      };
      makeOcamlLsp = pkgs:
        with pkgs.ocamlPackages;
        buildDunePackage (basePackage // {
          pname = package;
          checkInputs = with pkgs.ocamlPackages; [ ppx_expect ];
          buildInputs = [
            ocamlc-loc
            astring
            camlp-streams
            dune-build-info
            re
            dune-rpc
            chrome-trace
            dyn
            fiber
            xdg
            ordering
            spawn
            csexp
            ocamlformat-rpc-lib
            stdune
            yojson
            ppx_yojson_conv_lib
            uutf
            merlin-lib
          ];
          propagatedBuildInputs = [ ];
          doCheck = false;
          buildPhase = ''
            runHook preBuild
            dune build ${package}.install --release ''${enableParallelBuilding:+-j $NIX_BUILD_CORES}
            runHook postBuild
          '';
          meta = { mainProgram = "ocamllsp"; };
        });
    in {
      overlays.default = (final: prev: {
        ocamlPackages = prev.ocamlPackages.overrideScope
          (oself: osuper: with oself; { ocaml-lsp = makeLspPackage final; });
      });
    } // (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          overlays = [ overlay ];
          inherit system;
        };
        inherit (pkgs.ocamlPackages) buildDunePackage;
        fast = rec {

          jsonrpc = buildDunePackage (basePackage // {
            pname = "jsonrpc";
            propagatedBuildInputs = with pkgs.ocamlPackages; [ ];
            doCheck = false;
          });

          lsp = buildDunePackage (basePackage // {
            pname = "lsp";
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              jsonrpc
              yojson
              ppx_yojson_conv_lib
              uutf
            ];
            checkInputs = with pkgs.ocamlPackages; [ cinaps ppx_expect ];
            doCheck = false;
          });

          ocaml-lsp = makeOcamlLsp pkgs;
        };
      in {
        packages = rec {
          # we have a package without opam2nix for easy consumption for nix users
          default = makeOcamlLsp pkgs;

          ocaml-lsp = fast.ocaml-lsp;
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = (with pkgs; [
              # dev tools
              ocamlformat_0_26_1
              yarn

              ocamlPackages.ppx_expect
              ocamlPackages.utop
              ocamlPackages.cinaps
              ocamlPackages.ppx_yojson_conv
            ]);
            inputsFrom = [ fast.ocaml-lsp fast.jsonrpc fast.lsp ];
          };

          release = pkgs.mkShell { buildInputs = [ pkgs.dune-release ]; };
        };
      }));
}
