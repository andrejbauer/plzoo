TARGET=calc

SOURCES = \
	syntax.ml \
	eval.ml \
	lexer.mll \
	parser.mly \
	calc.ml \

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
	echo "<h3>$(TARGET)</h3>" >> web.html
	echo '<div class="version">Last update: ' >> web.html
	cat version.txt >> web.html
	echo '</div>' >> web.html
	echo '<div class="description">' >> web.html
	cat description.html >> web.html
	echo '</div>' >> web.html
	echo '<div class="download">Download source: <a href="src/$(TARGET).zip">$(TARGET).zip</a></div>' >> web.html
	/bin/echo -n '<div class="source"><a href="html/$(TARGET).html">View source online</a> (' >> web.html
	cat $(SOURCES) | wc -l >> web.html
	echo ' lines)</div>' >> web.html
	echo "</div>" >> web.html

html:
	$(CAML2HTML) -nf -ln -noannot -o ../html/$(TARGET).html $(SOURCES)

clean:
	/bin/rm -f web.html
	$(OCAMLBUILD) -clean