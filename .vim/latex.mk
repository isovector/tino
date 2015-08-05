.PHONY: clean

%.pdf: %.tex $(DEPENDS)
	rubber -f --pdf -s $<
	rubber-info --check $<
	evince $@ &

clean:
	rm -rf *.aux *.bbl *.blg *.log *.pdf *.toc *.snm *.out *.nav tags

