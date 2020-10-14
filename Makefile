TEST_E2E_DIR = ocaml-lsp-server/test/e2e

build:
	dune build
.PHONY: build

fmt:
	dune build @fmt --auto-promote
	cd $(TEST_E2E_DIR) && yarn fmt
.PHONY: fmt

gen:
	dune build @cinaps --auto-promote
	dune build @fmt --auto-promote
.PHONY: gen

fmt-check:
	dune build @fmt
	cd $(TEST_E2E_DIR) && yarn fmt-check
.PHONY: fmt-check

lsp-server:
	dune build @install
.PHONY: lsp-server

test: $(TEST_E2E_DIR)/node_modules lsp-server
	dune build @test @lsp/test/runtest && cd $(TEST_E2E_DIR) && dune exec -- yarn test
.PHONY: test

ocaml-test:
	dune build @test @lsp/test/runtest

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && yarn install --frozen-lockfile

opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests --include-submodules
	# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
