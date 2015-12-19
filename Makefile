all : parse zipup move clean

# Note: This requires Libre Office installed on a mac
convert :
	/Applications/LibreOffice.app/Contents/MacOS/./soffice \
	--invisible --convert-to csv \
	RAW_DATA/2014_winter_ID_poll.ods \
	--outdir RAW_DATA;

# Note to self: add in checks for packages
parse :	
	Rscript parse_data.R

zipup :
	zip -r avail.zip availability.* dossiers/

move : 
	cp avail.zip ~/Google\ Drive/ID; \
	unzip -u -d ~/Google\ Drive/ID avail.zip

clean:
	$(RM) avail.zip availability.*

remove_whitespace :
	./remove_newlines.sh;\
	$(RM) RAW_DATA/*.bak
