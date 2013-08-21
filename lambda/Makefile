default: native

native:
	ocamlbuild -lib unix -use-menhir lambda.native

byte:
	ocamlbuild -lib unix -use-menhir lambda.byte

clean:
	ocamlbuild -clean

doc:
	ocamlbuild -use-menhir -docflag -keep-code -lib unix lambda.docdir/index.html
