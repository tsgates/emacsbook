
PORT := :15

MD := $(wildcard chap*/doc.md)

OUT_MD   := $(MD:.md=.markdown)
OUT_HTML := $(patsubst chap%/doc.md, html/chap%.html, $(MD))
OUT_INDX := html/index.html
OUT_PDF  := latex/main.pdf

all: html

%.markdown: %.md
	./make-md $^

html/%.html: html/template-chap.html
html/%.html: %/doc.markdown 
	./make-html $^

pdf: $(OUT_PDF)
$(OUT_PDF): $(OUT_MD) latex/template.tex latex/config.yml
	./make-pdf

html: $(OUT_HTML) $(OUT_INDX)
$(OUT_INDX): $(OUT_HTML) html/template-index.html
	./make-html-index

snap: $(MD)
	PATH=$(PWD)/script:$$PATH DISPLAY=$(PORT) ./make-snap $^

clean:
	rm -f $(OUT_PDF) $(OUT_MD) $(OUT_HTML) $(OUT_INDX)

post: $(OUT_INDX)
	@perl -e 'print "="x79,"\n"'
	@echo "- github: git clone git://github.com/tsgates/emacsbook.git"
	@echo "- index: http://emacsbook.taesoo.org"
	@for l in `grep -w -o chap. $(OUT_INDX)` ; do \
		echo "- $$l: http://emacsbook.taesoo.org/$$l.html"; \
	done
	@perl -e 'print "="x79,"\n"'
	@w3m -dump $(OUT_INDX) | grep -v "\(Fork\|Disqus\|CC\|Home\|Blog\|About\)"

.PHONY: all clean html pdf snap post
