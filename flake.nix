{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      package = "ocaml-lsp-server";

      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = builtins.attrValues self.overlays;
      });
    in
    {
      overlays = {
        default = final: prev: {
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
              ocaml-lsp = self.packages.${prev.system}.ocaml-lsp;
            });
        };

        ocaml-lsp-only = final: prev: {
          ocamlPackages = prev.ocamlPackages.overrideScope' (_: _: {
            ocaml-lsp = self.packages.${prev.system}.ocaml-lsp;
          });
        };
      };

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
          inherit (pkgs.ocamlPackages) buildDunePackage;
        in
        {
          # we have a package without opam2nix for easy consumption for nix users
          default = self.packages.${system}.ocaml-lsp;

          ocaml-lsp = buildDunePackage {
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
              merlin-lib
            ] ++ [
              self.packages.${system}.lsp
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

          # stub builds
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
              yojson
              stdune
              ppx_yojson_conv_lib
              uutf
            ] ++ [
              self.packages.${system}.jsonrpc
            ];
            checkInputs = with pkgs.ocamlPackages; [ cinaps ppx_expect ];
            doCheck = false;
          };
        });

      devShells = forAllSystems (system:
        let pkgs = nixpkgsFor.${system}; in {
          default = pkgs.mkShell {
            name = package;
            inputsFrom = builtins.attrValues self.packages.${system};
            buildInputs = with pkgs; [
              # dev tools
              ocamlformat_0_26_1
              yarn

              ocamlPackages.ppx_expect
              ocamlPackages.utop
              ocamlPackages.cinaps
              ocamlPackages.ppx_yojson_conv
            ];
          };

          release = pkgs.mkShell {
            buildInputs = [ pkgs.dune-release ];
          };
        });
  };
}
