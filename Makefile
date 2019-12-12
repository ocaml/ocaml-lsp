TEST_E2E_DIR = ocaml-lsp-server/test/e2e

build:
	dune build
.PHONY: build

test: $(TEST_E2E_DIR)/node_modules
	cd $(TEST_E2E_DIR) && npm run test
.PHONY: test

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && npm install
