.DEFAULT_GOAL := all

TEST_E2E_DIR = ocaml-lsp-server/test/e2e

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && yarn install

-include Makefile.dev

.PHONY: all
all:
	dune build @all

# we don't use --with-test because it pulls test dependencies transitively in
# practice this ends up pulling a lot of extra packages which invariably
# results in a conflict
.PHONY: install-test-deps
install-test-deps:
	opam install menhir cinaps ppx_expect>=v0.14.0 \
		ocamlformat.$$(cat .ocamlformat | grep version | cut -d '=' -f 2) ocamlformat-rpc

.PHONY: dev
dev: ## Setup a development environment
	opam switch create --no-install . ocaml-base-compiler.4.12.0
	opam install -y dune-release merlin ocamlformat utop ocaml-lsp-server
	opam install --locked --deps-only --with-doc -y .
	$(MAKE) install-test-deps

.PHONY: install
install: ## Install the packages on the system
	dune build @install && dune install

.PHONY: lock
lock: ## Generate the lock files
	opam lock -y .

.PHONY: test
test-ocaml: ## Run the unit tests
	dune build @lsp/test/runtest @lsp-fiber/runtest @fiber-unix/runtest @jsonrpc-fiber/runtest

.PHONY: promote
promote:
	dune promote

.PHONY: check
check:
	dune build @check

.PHONY: test-e2e
test-e2e: $(TEST_E2E_DIR)/node_modules ## Run the template integration tests
	dune build @install && cd $(TEST_E2E_DIR) && dune exec -- yarn test

.PHONY: promote-e2e
promote-e2e: $(TEST_E2E_DIR)/node_modules
	dune build @install && cd $(TEST_E2E_DIR) && dune exec -- yarn run promote

.PHONY: test
test: test-ocaml test-e2e

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	dune clean

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	dune build @fmt --auto-promote
	cd $(TEST_E2E_DIR) && yarn fmt

.PHONY: watch
watch: ## Watch for the filesystem and rebuild on every change
	dune build --watch

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	dune utop . -- -implicit-bindings

.PHONY: release
release: ## Release on Opam
	dune-release distrib --skip-build --skip-lint --skip-tests --include-submodules
	# See https://github.com/ocamllabs/dune-release/issues/206
	DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: nix/opam-selection.nix
nix/opam-selection.nix:
	nix-shell -A resolve default.nix
