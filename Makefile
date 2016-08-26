OCAMLBUILD=ocamlbuild

LANGS = calc \
	calc_var \
	lambda \
	miniml \
	miniml_error \
	minihaskell \
	levy \
	miniprolog \
	sub \
	poly \
	boa

SRCDIR = src

.PHONY: $(LANGS)

default:
	@echo "To compile all languages run:"
	@echo "   make all"
	@echo "To compile a single language run:"
	@echo "   make <lang>"
	@echo "where <lang> is one of:"
	@echo "$(LANGS)"

all: $(LANGS)

$(LANGS): % :
	$(OCAMLBUILD) -use-menhir -menhir "menhir --explain" -libs unix -I $(SRCDIR) src/$@/$@.native

clean:
	$(OCAMLBUILD) -clean
