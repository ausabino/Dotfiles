project = main
# tex	    = latex
tex	    = pdflatex
flags	= -file-line-error -shell-escape -halt-on-error -interaction nonstopmode
bibtex	= bibtex
build	= $(tex) $(flags) $(project).tex

pdf: ps
	ps2pdf ${project}.ps

pdf-print: ps
	ps2pdf -dColorConversionStrategy=/LeaveColorUnchanged -dPDFSETTINGS=/printer ${project}.ps

text: html
	html2text -width 100 -style pretty ${project}.html | sed -n '/./,$$p' | head -n-2 > ${project}.txt

html:
	@#latex2html -split +0 -info "" -no_navigation $(flags) ${project}
	htlatex ${project}

ps:	dvi
	dvips -t letter ${project}.dvi

dvi:
	$(build)
	$(bibtex) $(project) || true
	$(build)
	$(build)

clean-all: clean
	rm -f ${project}.pdf ${project}-compressed.pdf

clean:
	rm -f *.bcf *.ps *.log *.aux *.out *.bbl *.blg *.brf *.toc *.idx *krc *~ *.log *.nav *.snm *.soc *.loc
	rm -rf _*

read:
	xdg-open ${project}.pdf &

compressed: dvi
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.5 -dNOPAUSE -dQUIET -dBATCH -dPrinted=false -sOutputFile=${project}-compressed.pdf ${project}.pdf
