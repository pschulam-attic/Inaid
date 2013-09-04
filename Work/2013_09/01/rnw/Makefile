DOC = simple_knn

$(DOC).pdf: $(DOC).Rnw
	pdfknitr $<

clean:
	cleanknitr

clobber: clean
	rm -f $(DOC).pdf

.PHONY: clean clobber
