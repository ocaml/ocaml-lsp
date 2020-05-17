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
	dune build @test && cd $(TEST_E2E_DIR) && dune exec -- yarn test
.PHONY: test

$(TEST_E2E_DIR)/node_modules:
	cd $(TEST_E2E_DIR) && yarn install --frozen-lockfile

travis-test:
	curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
	echo "deb https://dl.yarnpkg.com/debian/ stable main" \
		| sudo tee /etc/apt/sources.list.d/yarn.list
	sudo apt update
	sudo apt install -y yarn
	cd $(TEST_E2E_DIR) && yarn install --frozen-lockfile
	cd $(TEST_E2E_DIR) && yarn test
