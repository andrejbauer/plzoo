OCAMLBUILD=ocamlbuild

# Should we build native or bytecode by default?
BUILD=native
# BUILD=byte

LANGS = $(shell find src -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
SRCDIR = src

.PHONY: $(LANGS)

default:
	@echo "To compile all languages run:                 make all"
	@echo "To compile a single language <lang> run:      make <lang>"
	@echo "To compile bytecode add BUILD=byte:           make BUILD=byte ..."
	@echo "Available languages:"
	@echo "$(sort $(LANGS))"

all: $(LANGS)

$(LANGS): % :
	$(OCAMLBUILD) -use-menhir -menhir "menhir --explain" -libs unix -I $(SRCDIR) src/$@/$@.$(BUILD)

clean:
	$(OCAMLBUILD) -clean
