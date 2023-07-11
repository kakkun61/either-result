PWSH = pwsh

.PHONY: build
build: build-deps
	cabal v2-build $(CABAL_OPTIONS)

.PHONY: build-deps
build-deps:
	cabal v2-build --only-dependencies $(CABAL_OPTIONS)

.PHONY: test
test: doctest spec

.PHONY: doctest
doctest: build-deps
	cabal v2-test doctest $(CABAL_OPTIONS)

.PHONY: spec
spec: build-deps
	cabal v2-test spec $(CABAL_OPTIONS)

.PHONY: repl
repl:
	cabal v2-repl $(CABAL_OPTIONS)

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, test | ForEach-Object { stylish-haskell -i $$_.FullName } }"
	stylish-haskell -i Setup.hs

.PHONY: setup-format
setup-format:
	cabal v2-install stylish-haskell --overwrite-policy=always

.PHONY: lint
lint:
	hlint src

.PHONY: setup-lint
setup-lint:
	cabal v2-install hlint --overwrite-policy=always

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: clean
clean:
	cabal v2-clean
