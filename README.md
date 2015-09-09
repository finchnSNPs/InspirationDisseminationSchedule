# Code for Compiling Inspiration Dissemination Schedule

This repository contains code that is needed to create the availability schedule
for the [Inspiration Dissemination](http://blogs.oregonstate.edu/inspiration) 
radio show at Oregon State University. 

## Setup

You will need to install [R](https://cran.r-project.org) (preferably 
[Rstudio](https://rstudio.com)), and [pandoc](http://pandoc.org/installing.html).

You will also need to install the "scheduler" package located here so we can
ensure all the dependencies are installed. You can do that by running the 
following code:

```r
install.packages("devtools")
devtools::install_github("zkamvar/InspirationDisseminationSchedule/scheduler")
```

## Running for the first time

1. [**download the zip file of this repository by clicking here**](https://github.com/zkamvar/InspirationDisseminationSchedule/archive/master.zip). 
2. unzip the file and open the folder. If you are using Rstudio, double click on 
**ID_schedule.Rproj**. This will open up Rstudio in that folder.
3. run `source('parse_data.R', echo = TRUE)`. 
    - At this time, your browser window will pop up and ask you to authorize a google account for use. **Use the InspirationDissemination account.**

You will need to run this for the first time interactively so that googlesheets 
can authorize your session. If you are using this code, it is assumed that you 
are a host of Inspiration Dissemination.


## Creating the Dossiers

This can be done from within R or through a makefile. Note that this assumes you
have a unix system and have successfully installed pandoc.

### From the R console:

```r
source('parse_data.R', echo = FALSE)
```

### From the terminal:

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
