lesson=01
INPUT:=source$(lesson).md
OUTHTML:=lesson$(lesson).html
OUTPDF:=lesson$(lesson).pdf

all: README.md $(OUTHTML) $(OUTPDF) 

README.md: $(INPUT)
	pandoc -t gfm $(INPUT) -o README.md 

$(OUTHTML): $(INPUT)
	pandoc -s -t revealjs -F mermaid-filter $(INPUT) -o $(OUTHTML) -V theme=white

$(OUTPDF): $(INPUT)
	pandoc -t beamer -F mermaid-filter $(INPUT) -o $(OUTPDF) -V theme:boxes

