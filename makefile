all: doc.pdf

out.md: doc.md images/*.png
	cp $< $@
	perl -pi -e 's/{{ proj }}/Ok/g' $@

doc.pdf: chap1/doc.md
	markdown2pdf -N $^ -o $@

clean:
	rm out.md doc.pdf

html:
	;

.PHONY: all