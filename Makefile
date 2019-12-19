TEST_E2E_DIR = ocaml-lsp-server/test/e2e

build:
	dune build
.PHONY: build

fmt:
	dune build @fmt --auto-promote
	cd $(TEST_E2E_DIR) && yarn fmt
.PHONY: fmt

fmt-check:
	dune build @fmt
	cd $(TEST_E2E_DIR) && yarn fmt-check
.PHONY: fmt-check

lsp-server:
	dune build @install
.PHONY: lsp-server

test: $(TEST_E2E_DIR)/node_modules lsp-server
	cd $(TEST_E2E_DIR) && dune exec -- yarn test
.PHONY: test

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && yarn install --frozen-lockfile
