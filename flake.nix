{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    merlin4_14 = {
      url = "github:ocaml/merlin/v4.14-414";
      flake = false;
    };
    merlin5_1 = {
      url = "github:ocaml/merlin/v4.14-501";
      flake = false;
    };
  };

  outputs = { self, flake-utils, nixpkgs, ... }@inputs:
    let
      package = "ocaml-lsp-server";
      basePackage = {
        duneVersion = "3";
        version = "n/a";
        src = ./.;
      };
      overlay = merlin: final: prev: {
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
            merlin-lib = osuper.merlin-lib.overrideAttrs (o: { src = merlin; });
          });
      };
      ocamlVersionOverlay =
        (ocaml: self: super: { ocamlPackages = ocaml super.ocaml-ng; });
      makePackages = pkgs:
        let buildDunePackage = pkgs.ocamlPackages.buildDunePackage;
        in rec {
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

          ocaml-lsp = with pkgs.ocamlPackages;
            buildDunePackage (basePackage // {
              pname = package;
              checkInputs = with pkgs.ocamlPackages; [ ppx_expect ];
              buildInputs = [
                jsonrpc
                lsp
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
        };
    in {
      overlays.default = (final: prev: {
        ocamlPackages = prev.ocamlPackages.overrideScope
          (oself: osuper: with oself; makePackages final);
      });
    } // (flake-utils.lib.eachDefaultSystem (system:
      let
        makeNixpkgs = ocaml: merlin:
          import nixpkgs {
            overlays = [ (ocamlVersionOverlay ocaml) (overlay merlin) ];
            inherit system;
          };
        pkgs_4_14 =
          makeNixpkgs (ocaml: ocaml.ocamlPackages_4_14) inputs.merlin4_14;
        pkgs_5_1 =
          makeNixpkgs (ocaml: ocaml.ocamlPackages_5_1) inputs.merlin5_1;
        packages_4_14 = makePackages pkgs_4_14;
        packages_5_1 = makePackages pkgs_5_1;
        mkShell = packages: nixpkgs:
          nixpkgs.mkShell {
            buildInputs = (with nixpkgs; [
              # dev tools
              ocamlformat_0_26_1
              yarn

              ocamlPackages.ppx_expect
              ocamlPackages.utop
              ocamlPackages.cinaps
              ocamlPackages.ppx_yojson_conv
            ]);
            inputsFrom = with packages; [ ocaml-lsp jsonrpc lsp ];
          };
      in {
        packages = (packages_4_14 // { default = packages_4_14.ocaml-lsp; });

        devShells = {
          default = mkShell packages_4_14 pkgs_4_14;

          ocaml5_1 = mkShell packages_5_1 pkgs_5_1;

          release =
            pkgs_4_14.mkShell { buildInputs = [ pkgs_4_14.dune-release ]; };
        };
      }));
}
