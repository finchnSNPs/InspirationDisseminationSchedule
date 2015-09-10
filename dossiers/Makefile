# Produce PDFs from all Markdown files in a directory
# Lincoln Mullen | http://lincolnmullen.com | lincoln@lincolnmullen.com

# List files to be made by finding all *.md files and appending .pdf
PDFS := $(patsubst %.md, pdf_files/%.md.pdf, $(wildcard *.md))

HTML := $(patsubst %.md, html_files/%.md.html, $(wildcard *.md))

# The all rule makes all the PDF files listed
pdf : $(PDFS)

html: $(HTML)                        
# This generic rule accepts PDF targets with corresponding Markdown 
# source, and makes them using pandoc
pdf_files/%.md.pdf : %.md
	pandoc $< -o $@
html_files/%.md.html : %.md
	pandoc $< -o $@

# Remove all PDF outputs
clean :
	rm $(PDFS) $(HTML)

move: 
	-mv *.md md_files/

all : html pdf move
	
# Remove all PDF outputs then build them again
rebuild : clean all
