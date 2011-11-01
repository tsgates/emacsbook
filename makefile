all: doc.pdf

%.markdown: %.md
	./gen $^

out.md: chap*/doc.markdown
	cat $? > $@

doc.pdf: out.md
	markdown2pdf -N $^ -o $@

clean:
	rm out.md doc.pdf

html:
	;

.PHONY: all