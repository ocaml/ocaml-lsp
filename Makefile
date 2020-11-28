.DEFAULT_GOAL := all

TEST_E2E_DIR = ocaml-lsp-server/test/e2e

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && yarn install

.PHONY: all
all:
	opam exec -- dune build --root . @install

.PHONY: dev
dev: ## Setup a development environment
	opam switch create --no-install . ocaml-base-compiler.4.11.1
	opam install -y dune-release merlin ocamlformat utop ocaml-lsp-server
	opam install --locked --deps-only --with-test --with-doc -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: install
install: all ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: lock
lock: ## Generate the lock files
	opam lock -y .

.PHONY: test
test: ## Run the unit tests
	opam exec -- dune build --root . @lsp-fiber/runtest @fiber-unix/runtest @jsonrpc-fiber/runtest

.PHONY: test-template
test-e2e: $(TEST_E2E_DIR)/node_modules all ## Run the template integration tests
	cd $(TEST_E2E_DIR) && opam exec -- dune exec -- yarn test

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . @fmt --auto-promote
	cd $(TEST_E2E_DIR) && yarn fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	opam exec -- dune build ---root . -watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . . -- -implicit-bindings

.PHONY: release
release: all ## Release on Opam
	dune-release distrib --skip-build --skip-lint --skip-tests --include-submodules
	# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit
