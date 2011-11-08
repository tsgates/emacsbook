
MD := $(wildcard chap*/doc.md)

all: doc.pdf

%.markdown: %.md
	./gen $^

doc.pdf: latex/main.pdf
	cp -f $^ $@

latex/main.pdf: ${MD:.md=.markdown} latex/template.tex latex/config.yml
	./make-pdf

clean:
	rm out.md doc.pdf

html: ${MD:.md=.markdown} html/template*.html
	./make-html

.PHONY: all