build:
	dune build
.PHONY: build

test: build ocaml-lsp-server/test/e2e/node_modules
	cd ocaml-lsp-server/test/e2e && npm run test
.PHONY: test

ocaml-lsp-server/test/e2e/node_modules:
	cd $(@D) && npm install


