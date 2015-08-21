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
	pandoc $< -o $@
%.md.html : %.md
	pandoc $< -o $@

# Remove all PDF outputs
clean :
	rm $(PDFS) $(HTML)

all : html pdf
	
# Remove all PDF outputs then build them again
rebuild : clean all
