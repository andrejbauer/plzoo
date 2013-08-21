TARGET=lambda

OCAMLBUILD=ocamlbuild

.PHONY: byte native

default: byte

all: byte native

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

clean:
	$(OCAMLBUILD) -clean
