TEST_E2E_DIR = ocaml-lsp-server/test/e2e

build:
	dune build
.PHONY: build

lsp-server:
	dune build @install
.PHONY: lsp-server

test: $(TEST_E2E_DIR)/node_modules lsp-server
	cd $(TEST_E2E_DIR) && dune exec -- npm run test
.PHONY: test

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && npm install
