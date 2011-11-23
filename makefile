
PORT := :11

MD := $(wildcard chap*/doc.md)

OUT_MD   := $(MD:.md=.markdown)
OUT_HTML := $(patsubst chap%/doc.md, html/chap%.html, $(MD))
OUT_INDX := html/index.html

all: html

%.markdown: %.md
	./make-md $^

html/%.html: %/doc.markdown
	./make-html $^

doc.pdf: latex/main.pdf
	cp -f $^ $@

latex/main.pdf: $(OUT_MD) latex/template.tex latex/config.yml
	./make-pdf

html: $(OUT_HTML) $(OUT_INDX)
$(OUT_INDX): $(OUT_HTML)
	./make-html-index

snap: $(MD)
	PATH=$(PWD)/script:$$PATH DISPLAY=$(PORT) ./make-snap $^

clean:
	rm -f doc.pdf $(OUT_MD) $(OUT_HTML) $(OUT_INDX)

post: $(OUT_INDX)
	@echo "- github: git clone git://github.com/tsgates/emacsbook.git"
	@echo "- index: http://emacsbook.taesoo.org"
	@for l in `grep -w -o chap. $(OUT_INDX)` ; do \
		echo "- $$l: http://emacsbook.taesoo.org/$$l.html"; \
	done
	@echo "===================="
	@w3m -dump $(OUT_INDX) | grep -v Fork

.PHONY: all clean html snap