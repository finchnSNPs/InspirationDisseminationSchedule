# Produce PDFs from all Markdown files in a directory
# Lincoln Mullen | http://lincolnmullen.com | lincoln@lincolnmullen.com

# List files to be made by finding all *.md files and appending .pdf
PDFS := $(patsubst %.md,%.md.pdf,$(wildcard *.md))

HTML := $(patsubst %.md,%.md.html,$(wildcard *.md))

# The all rule makes all the PDF files listed
pdf : $(PDFS)

html: $(HTML)                        
# This generic rule accepts PDF targets with corresponding Markdown 
# source, and makes them using pandoc
%.md.pdf : %.md
	pandoc $< -o pdf_files/$@
%.md.html : %.md
	pandoc $< -o html_files/$@

# Remove all PDF outputs
clean :
	rm $(PDFS) $(HTML)

move: 
	-mv *.md md_files/

all : html pdf move
	
# Remove all PDF outputs then build them again
rebuild : clean all
