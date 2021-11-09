.PHONY: default
default: all

LANGS = $(shell find src -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)

.PHONY: $(LANGS)

$(LANGS): % :
	dune build ./src/$@/$@.bc ./src/$@/$@.exe

.PHONY: all
all:
	dune build @all

.PHONY: clean
clean:
	dune clean
