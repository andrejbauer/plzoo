OCAMLBUILD=ocamlbuild

LANGS = lambda/lambda miniml/miniml

BYTETARGETS = $(LANGS:=.byte)
NATIVETARGETS = $(LANGS:=.native)

.PHONY: native byte

default: native

native: $(NATIVETARGETS)

byte: $(BYTETARGETS)

$(NATIVETARGETS): %.native : %.ml
	$(OCAMLBUILD) -use-menhir -libs unix $@

$(BYTETARGETS): %.byte : %.ml
	$(OCAMLBUILD) -use-menhir -libs unix $@
clean:
	$(OCAMLBUILD) -clean
