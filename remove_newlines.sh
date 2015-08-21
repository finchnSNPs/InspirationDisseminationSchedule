#!/bin/sh

# This comes directly from here: 
# http://www.unix.com/shell-programming-and-scripting/136568-removal-new-line-character-double-quotes.html

sed -i .bak 'H;g;/^[^"]*"[^"]*\("[^"]*"[^"]*\)*$/d; s/^\n//; y/\n/ /; p; s/.*//; h' \
RAW_DATA/2014_winter_ID_poll.csv;\