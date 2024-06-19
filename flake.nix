{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    merlin4_14 = {
      url = "github:ocaml/merlin/v4.16-414";
      flake = false;
    };
    merlin5_1 = {
      url = "github:ocaml/merlin/v4.16-501";
      flake = false;
    };
  };

  outputs = { self, flake-utils, nixpkgs, ... }@inputs:
    let
      package = "ocaml-lsp-server";
      ocamlformat = pkgs: pkgs.ocamlformat_0_26_2;
      basePackage = {
        duneVersion = "3";
        version = "n/a";
        src = ./.;
        doCheck = true;
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
            # TODO remove these hacks eventually
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
      makeLocalPackages = pkgs:
        let buildDunePackage = pkgs.ocamlPackages.buildDunePackage;
        in rec {
          jsonrpc = buildDunePackage (basePackage // {
            pname = "jsonrpc";
            propagatedBuildInputs = with pkgs.ocamlPackages; [ ];
          });

          lsp = buildDunePackage (basePackage // {
            pname = "lsp";
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              jsonrpc
              yojson
              ppx_yojson_conv_lib
              uutf
            ];
            checkInputs = let p = pkgs.ocamlPackages;
            in [ p.cinaps p.ppx_expect p.ppx_yojson_conv (ocamlformat pkgs) ];
          });

          ocaml-lsp = with pkgs.ocamlPackages;
            buildDunePackage (basePackage // {
              pname = package;
              checkInputs = let p = pkgs.ocamlPackages;
              in [
                p.ppx_expect
                p.ppx_yojson_conv
                (ocamlformat pkgs)
                pkgs.yarn
              ];
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
                base
              ];
              propagatedBuildInputs = [ ];
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
          (oself: osuper: with oself; makeLocalPackages final);
      });
    } // (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgsWithoutOverlays = (import nixpkgs { inherit system; });
        makeNixpkgs = ocaml: merlin:
          import nixpkgs {
            overlays = [ (ocamlVersionOverlay ocaml) (overlay merlin) ];
            inherit system;
          };
        pkgs_4_14 =
          makeNixpkgs (ocaml: ocaml.ocamlPackages_4_14) inputs.merlin4_14;
        pkgs_5_1 =
          makeNixpkgs (ocaml: ocaml.ocamlPackages_5_1) inputs.merlin5_1;
        localPackages_4_14 = makeLocalPackages pkgs_4_14;
        localPackages_5_1 = makeLocalPackages pkgs_5_1;
        devShell = localPackages: nixpkgs:
          nixpkgs.mkShell {
            buildInputs = [ nixpkgs.ocamlPackages.utop ];
            inputsFrom = builtins.attrValues localPackages;
          };
      in {
        packages =
          (localPackages_4_14 // { default = localPackages_4_14.ocaml-lsp; });

        devShells = {
          default = devShell localPackages_4_14 pkgs_4_14;

          ocaml5_1 = devShell localPackages_5_1 pkgs_5_1;

          release = pkgsWithoutOverlays.mkShell {
            buildInputs = [ pkgsWithoutOverlays.dune-release ];
          };

          fmt = pkgsWithoutOverlays.mkShell {
            buildInputs = [
              # TODO: get rid of ocaml once dune get format without ocaml being
              # present
              pkgsWithoutOverlays.ocaml
              (ocamlformat pkgsWithoutOverlays)
              pkgsWithoutOverlays.yarn
              pkgsWithoutOverlays.dune_3
            ];
          };

          check = pkgs_4_14.mkShell {
            inputsFrom = builtins.attrValues localPackages_4_14;
          };
        };
      }));
}
