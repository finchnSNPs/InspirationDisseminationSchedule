# Code for Compiling Inspiration Dissemination Schedule

This repository contains code that is needed to create the availability schedule
for the [Inspiration Dissemination](http://blogs.oregonstate.edu/inspiration) 
radio show at Oregon State University. 

## Setup

You will need to install R (preferably Rstudio), and pandoc.

You will also need to install the "scheduler" package:

```r
install.packages("devtools")
devtools::install_github("zkamvar/InspirationDisseminationSchedule/scheduler")
```

By making a package, I can ensure that the dependencies will be installed.

## Running for the first time

You will need to run this for the first time interactively so that googlesheets 
can authorize your session. If you are using this code, it is assumed that you 
are a host of Inspiration Dissemination. A browser window will pop up and it
will ask you to authorize R to access google sheets.

If you run it interactively from the terminal, use:

```r
source('parse_data.R', echo = TRUE)
```

This will get you to the part where you can view the availability chart with
the side effect that it won't create the dossiers. To create the dossiers 
without viewing the chart, run:


```r
source('parse_data.R', echo = FALSE)
```

## Running otherwise

You can simply create all of the dossiers and the availability table by running

```
make parse
```

If you want to run everything and move the zipped results to your google drive,
you can run

```
make all
```

Note: I know this works on mac, but I'm not so sure on other machines.
