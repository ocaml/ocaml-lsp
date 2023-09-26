{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, nixpkgs, ... }@inputs:
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
              astring
              camlp-streams
              merlin-lib
            ];
            doCheck = false;
          };
        };
      in
      {
        packages =
          rec {
            # we have a package without opam2nix for easy consumption for nix users
            default = pkgs.ocamlPackages.buildDunePackage {
              pname = package;
              version = "n/a";
              src = ./.;
              duneVersion = "3";
              buildInputs = with pkgs.ocamlPackages; [
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
              meta = {
                mainProgram = "ocamllsp";
              };
            };

            ocaml-lsp = fast.ocaml-lsp;
          };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = (with pkgs;
              [
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

          release = pkgs.mkShell {
            buildInputs = [ pkgs.dune-release ];
          };
        };
      });
}
