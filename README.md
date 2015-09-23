# Code for Compiling Inspiration Dissemination Schedule

This repository contains code that is needed to create the availability schedule
for the [Inspiration Dissemination](http://blogs.oregonstate.edu/inspiration) 
radio show at Oregon State University. 

## Setup

You will need to install [R](https://cran.r-project.org) (preferably 
[Rstudio](https://rstudio.com)), and [pandoc](http://pandoc.org/installing.html).

Run the following code:

```r
install.packages("devtools")
devtools::install_github("zkamvar/InspirationDisseminationSchedule/scheduler")
```

> Note for windows users: R might ask you if you want to use a 'personal
> library'. When this happens, choose "yes".

This will install the code and packages necessary to run the script called 
`parse_data.R`.


## Running for the first time

You will need to run this for the first time interactively so that googlesheets 
can authorize your session. If you are using this code, it is assumed that you 
are a host of Inspiration Dissemination.

***

1. [**download the zip file of this repository by clicking here**](https://github.com/zkamvar/InspirationDisseminationSchedule/archive/master.zip). 
2. unzip the file and open the folder. If you are using Rstudio, double click on 
**ID_schedule.Rproj**. This will open up Rstudio in that folder.
3. copy and paste the following command into your R console:

```r
source('parse_data.R', echo = TRUE)
```

***

At this time, your browser window will pop up and ask you to authorize a google
account for use. **Use the InspirationDissemination account.**

After this happens, you will see a lot of code flying by your screen and
eventually the cursor will dissappear and you will see the following:

```
> vis_plot # show the plot
Showing dynamic visualisation. Press Escape/Ctrl + C to stop.
```

A graph will also pop up in the viewer. You can use your mouse to hover over
cells in the graph to see who is available when and if you click on it, you can
see their research description.

When you are finished with this. You can either press Escape or Ctrl + C to stop
it, or you can press the red stop sign in Rstudio if that's what you're using.

> Don't worry if you see any warning messages. These are harmless.

## Creating the Dossiers

The dossiers are useful for sending to people to remind them what they signed up
with. You can create dossiers that will live in the [**dossiers/**](./dossiers) 
folder based on their file extension. Note that `*.md` stands for markdown,
which is a kind of text format that is easily converted into webpages like the
one you're reading.

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
