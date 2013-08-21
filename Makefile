LANGS = \
	calc \
	miniml \
	miniml+error \
	minihaskell \
	poly \
	sub \
	boa \
	levy \
	miniprolog

ZIP=zip
WWWDIR=/var/www/andrej.com/plzoo

default: src web index.html

publish: web src
	rsync -e ssh -r index.html style.css src html www.andrej.com:$(WWWDIR)

web:
	mkdir -p html
	cat preamble.html > index.html
	for lang in $(LANGS); do \
		$(MAKE) -C $$lang web html; \
		(cat $$lang/web.html >> index.html) \
	done
	cat end.html >> index.html

src:
	mkdir -p src
	for lang in $(LANGS); do \
		$(MAKE) -C $$lang clean; \
		$(ZIP) -r src/$$lang.zip $$lang -x \*/.svn/\* ; \
	done

clean:
	for lang in $(LANGS); do $(MAKE) -C $$lang clean; done
	/bin/rm -rf html src index.html

.PHONY: src web clean
