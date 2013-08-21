TARGET=minimlerror
LANG=miniml+error

SOURCES = \
	syntax.ml \
	type_check.ml \
	eval.ml \
	machine.ml \
	compile.ml \
	lexer.mll \
	parser.mly \
	minimlerror.ml

OCAMLBUILD=ocamlbuild
CAML2HTML=caml2html
OCAMLDOC=ocamldoc

default: byte

all: byte native html doc

byte:
	$(OCAMLBUILD) $(TARGET).byte

native:
	$(OCAMLBUILD) $(TARGET).native

web: html
	echo '<div class="lang">' > web.html
	echo "<h3>$(LANG)</h3>" >> web.html
	echo '<div class="version">Last update: ' >> web.html
	cat version.txt >> web.html
	echo '</div>' >> web.html
	echo '<div class="description">' >> web.html
	cat description.html >> web.html
	echo '</div>' >> web.html
	echo '<div class="download">Download source: <a href="src/$(LANG).zip">$(LANG).zip</a></div>' >> web.html
	/bin/echo -n '<div class="source"><a href="html/$(LANG).html">View source online</a> (' >> web.html
	cat $(SOURCES) | wc -l >> web.html
	echo ' lines)</div>' >> web.html
	echo "</div>" >> web.html

html:
	$(CAML2HTML) -nf -ln -noannot -o ../html/$(LANG).html $(SOURCES)

doc:
	$(OCAMLBUILD) $(LANG).docdir/index.html

clean:
	/bin/rm -f web.html
	$(OCAMLBUILD) -clean