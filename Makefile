OCAMLBUILD=ocamlbuild

LANGS = miniml/miniml

BYTETARGETS = $(LANGS:=.byte)
NATIVETARGETS = $(LANGS:=.native)

.PHONY: native byte

default: native

native: $(NATIVETARGETS)

byte: $(BYTETARGETS)

$(NATIVETARGETS):
	$(OCAMLBUILD) -use-menhir -libs unix $@

$(BYTETARGETS):
	$(OCAMLBUILD) -use-menhir -libs unix $@
clean:
	$(OCAMLBUILD) -clean
