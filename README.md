# Code for Compiling Inspiration Dissemination Schedule

This repository contains code that is needed to create the availability schedule
for the [Inspiration Dissemination](http://blogs.oregonstate.edu/inspiration) 
radio show at Oregon State University. 

## Setup

You will need to install R (preferably Rstudio), and pandoc.

You will also need the following packages:

```r
install.packages(c("dplyr", "lubridate", "reshape2", "ggplot2", "googlesheets"))
```

## Running for the first time

You will need to run this for the first time interactively so that googlesheets 
can authorize your session. If you are using this code, it is assumed that you 
are a host of Inspiration Dissemination. A browser window will pop up and it
will ask you to authorize R to access google sheets.

## Running otherwise

You can simply create all of the dossiers and the availability table by running

```
make render
```

If you want to run everything and move the zipped results to your google drive,
you can run

```
make all
```

Note: I know this works on mac, but I'm not so sure on other machines.
