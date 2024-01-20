PACKAGE_YAML = package.yaml
NAME ?= my-xmonad.yaml
CABAL = $(NAME).cabal
STACK ?= stack

.PHONY: build
build: $(CABAL)
	$(STACK) build

.PHONY: build
install: build
	$(STACK) install

$(CABAL): $(PACKAGE_YAML)
	hpack
