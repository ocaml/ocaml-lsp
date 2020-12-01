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


jest-promote:
	cd $(TEST_E2E_DIR) && yarn promote
.PHONY: jest-promote

dune-promote:
	dune promote
.PHONY: dune-promote

fmt-check:
	dune build @fmt
	cd $(TEST_E2E_DIR) && yarn fmt-check
.PHONY: fmt-check

lsp-server:
	dune build @install
.PHONY: lsp-server

test-e2e: $(TEST_E2E_DIR)/node_modules
	dune build @install
	cd $(TEST_E2E_DIR) && dune exec -- yarn test
.PHONY: test-e2e

test-ocaml:
	dune build @lsp-fiber/runtest @fiber-unix/runtest @jsonrpc-fiber/runtest
.PHONY: test-ocaml

test: test-ocaml test-e2e
.PHONY: test

clean:
	dune clean
.PHONY: clean

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && yarn install --frozen-lockfile

opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests --include-submodules
	# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
