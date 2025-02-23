{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    merlin5_3 = {
      url = "github:liam923/merlin/rename-holes";
      flake = false;
    };
    merlin5_2 = {
      url = "github:ocaml/merlin/502";
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
        ocamlPackages = prev.ocamlPackages.overrideScope (oself: osuper:
          let
            fixPreBuild = o: {
              propagatedBuildInputs = o.propagatedBuildInputs ++ [ oself.pp ];
              preBuild = ''
                rm -rf vendor/csexp vendor/pp
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
        (ocaml: self: super: { ocamlPackages = super.ocaml-ng.${ocaml}; });
      makeLocalPackages = pkgs:
        let buildDunePackage = pkgs.ocamlPackages.buildDunePackage;
        in rec {
          jsonrpc = buildDunePackage (basePackage // {
            pname = "jsonrpc";
            doCheck = false;
            propagatedBuildInputs = with pkgs.ocamlPackages; [ ];
          });

          lsp = buildDunePackage (basePackage // {
            pname = "lsp";
            doCheck = false;
            propagatedBuildInputs = with pkgs.ocamlPackages; [
              jsonrpc
              yojson
              ppx_yojson_conv_lib
              uutf
            ];
            checkInputs = let p = pkgs.ocamlPackages;
            in [
              p.stdune
              p.cinaps
              p.ppx_expect
              p.ppx_yojson_conv
              (ocamlformat pkgs)
            ];
          });

          ocaml-lsp = with pkgs.ocamlPackages;
            buildDunePackage (basePackage // {
              pname = package;
              doCheck = false;
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
          pkgsWithoutOverlays.appendOverlays [ (ocamlVersionOverlay ocaml) (overlay merlin) ];
        pkgs_5_2 = makeNixpkgs "ocamlPackages_5_2" inputs.merlin5_2;
        pkgs_5_3 = makeNixpkgs "ocamlPackages_5_3" inputs.merlin5_3;
        localPackages_5_2 = makeLocalPackages pkgs_5_2;
        localPackages_5_3 = makeLocalPackages pkgs_5_3;
        devShell = localPackages: nixpkgs:
          nixpkgs.mkShell {
            buildInputs = [ nixpkgs.ocamlPackages.utop ];
            inputsFrom =
              builtins.map (x: x.overrideAttrs (p: n: { doCheck = true; }))
              (builtins.attrValues localPackages);
          };
      in {
        packages = (localPackages_5_3 // {
          default = localPackages_5_3.ocaml-lsp;
          ocaml_5_2 = localPackages_5_2;
        });

        devShells = {
          default = devShell localPackages_5_3 pkgs_5_3;

          ocaml5_2 = devShell localPackages_5_2 pkgs_5_2;

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

          check = pkgs_5_3.mkShell {
            inputsFrom = builtins.attrValues localPackages_5_3;
          };
        };
      }));
}
