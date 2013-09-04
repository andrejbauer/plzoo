OCAMLBUILD=ocamlbuild

LANGS = \
	src/calc/calc \
	src/lambda/lambda \
	src/levy/levy \
	src/miniml/miniml \

SRCDIR = src

BYTETARGETS = $(LANGS:=.byte)
NATIVETARGETS = $(LANGS:=.native)

.PHONY: native byte

default: native

native: $(NATIVETARGETS)

byte: $(BYTETARGETS)

$(NATIVETARGETS): %.native : %.ml
	$(OCAMLBUILD) -use-menhir -libs unix -I $(SRCDIR) $@

$(BYTETARGETS): %.byte : %.ml
	$(OCAMLBUILD) -use-menhir -libs unix -I (SRCDIR) $@
clean:
	$(OCAMLBUILD) -clean
