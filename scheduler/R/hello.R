#' Takes in comma separated months and days, returns a vector of POSIX dates.
#' 
#' @param x a string of comma separated dates.
#' @return a vector of POSIX dates.
#' 
#' @details One of the items in the survey asks the participants to choose the 
#'   date that they are available. This is rendered as a single column with
#'   comma separated values.
#'   
#' @export
#' @examples
#' get_availability(c("Jan 4 2015, Sep 6, March 20 2016"))
get_availability <- function(x){
  x <- utils::read.table(text = x, sep = ",", header = FALSE)  
  x <- unlist(x, use.names = FALSE)
  y <- setNames(lubridate::parse_date_time(x, c("md", "mdy")), x)
  return(y)
}

#' Takes in a vector of sundays and returns a logical vector stating which 
#' Sundays in the year the person is available.
#' 
#' @param x a vector of POSIX dates produced from \code{\link{get_availability}}
#' @param any_given_sunday a vector of POSIX dates defining the range of 
#'   possible Sundays.
#' @return a vector of logical elements indicating which dates are available for
#'   a person.
#' @export
save_the_date <- function(x, any_given_sunday){
  setNames(any_given_sunday %in% x, any_given_sunday)
}

#' Compare a vector of hierarchical categories.
#' 
#' @param x a character vector representing names of hierarchical categories.
#' @param set_missing a value or set of values to indicate the result should be 
#'   missing.
#' @return a character vector of length one specifying the hierarchical category
#'   chosen.
#' @details The participants are asked to provide what dates they are available 
#'   and what date in particular is best for them. We also are scheduling 
#'   participants all the time. Thus, we have a hierarchical structure for 
#'   deciding participants: Unavialable/Available < Preference < Scheduled. If 
#'   we have already scheduled the date, then we need to set that value as 
#'   missing so we know we can't schedule it.
#'   
#'   This is for use when creating a final table of availability since the
#'   original data is in a 3D array with dates, guests, and availability
#'   category.
#' 
#' @export
#' @examples
#' compare_array(c("Available", "Preference", NA))
#' compare_array(c("Available", NA, "Scheduled"))
#' compare_array(c("Unavailable", NA, NA))
compare_array <- function(x, set_missing = "Scheduled"){
  res <- x[max(which(!is.na(x)))]
  if (res %in% set_missing) return(NA)
  return(res)
}

#' Display function for html visualization
#' 
#' The info tip will display information of the participants.
#' 
#' @param x a data frame used to create the ggvis object.
#' 
#' @return a string encoding html.
#' @rdname infohover
#' @export
infohover <- function(x){
  if (is.null(x)) return(NULL)
  avail <- switch(x$value,
                  Available = "<b>yes<b>",
                  Unavailable = "no",
                  Preference = "<b><font color = 'red'>YES!!!</font></b>"
                  )
  sunday <- paste0(month(x$Sunday,label = TRUE), " ", day(x$Sunday), 
                   ", ", year(x$Sunday))
  if (x$Scheduled < 1){
    sdf   <- scheduler::scheduled$get()
    iddf  <- scheduler::IDS$get()
    guest <- sdf$Name[lubridate::ymd(sdf$Date) == lubridate::ymd(x$Sunday)]
    DEPT  <- iddf$Dept[iddf$Name == guest]
    return(paste0("<font color = 'gray'><h4>", sunday, ":</h4><h4>", 
                  guest, " (", DEPT, ")</h4></font>"))
  }
  paste0("<h4>", sunday, "</h4>", x$Guest, "<br>Available: ", avail)
}

#' @rdname infohover
#' @export
infoclick <- function(x){
  if (is.null(x)) return(NULL)
  avail <- switch(x$value,
                  Available = "<b>yes<b>",
                  Unavailable = "no",
                  Preference = "<b><font color = 'red'>YES!!!</font></b>"
                  )
  sunday <- paste0(month(x$Sunday,label = TRUE), " ", day(x$Sunday), 
                   ", ", year(x$Sunday))
  sdf   <- scheduler::scheduled$get()
  iddf  <- scheduler::IDS$get()
  if (x$Scheduled == 1){
    guest <- x$Guest
  } else {
    guest <- sdf$Name[lubridate::ymd(sdf$Date) == lubridate::ymd(x$Sunday)]
  }
  DEPT  <- iddf$Dept[iddf$Name == guest]
  INFO  <- iddf$Desc[iddf$Name == guest]
  res   <- paste0("<h4>", sunday, ": ", 
                  guest, " (", DEPT, ")</h4>",
                  "<h4>Research Description:</h4>",
                  INFO, "<br />")
  if (x$Scheduled == 1){
    res <- paste0(res, "Available: ", avail, "<br />") 
  }
  return(res)
}

#' Function to create a markdown dossier for each participant
#' 
#' @param x a name of a participant
#' @param df a data frame containing relevant information
#' @param xlist a list with one element per participant containing the dates for
#'   availability.
#' @param wd the location of the directory that contains the 'dossiers' folder.
#' @return nothing. 
#' @export
make_dossier <- function(x, df, xlist, wd = "."){
  dname <- gsub(" ", "_", x)
  i <- which(df$Name == x)
  f <- file(paste0(path.expand(wd), "/dossiers/", dname, ".md"), "w")
  cat("# ", x, "\n\n", file = f)
  cat("## Email: <", df$Email[i], ">\n\n", file = f, sep = "")
  cat("### ", df$Deg[i], "in", df$Dept[i], "\n\n", file = f)
  cat("### PI:", df$PI[i], "\n\n", file = f)
  cat("## Research Description\n\n", df$Desc[i], "\n\n", file = f)
  cat("## Availability\n", file = f)
  avail <- as.character(xlist[[x]])
  pref  <- df$Pref[i]
  pref  <- which(xlist[[x]] == pref)
  avail[pref] <- paste0("**", avail[pref], "**")
  avail <- ifelse(is.na(avail), names(avail), avail)
  cat("", avail, sep = "\n - ", file = f)
  close(f)
}



VALFUN <- function(){
  val <- NULL
  list(
    get = function(v) {if (missing(v)) val else val[[v]]},
    set = function(v) {val <<- v }
  )
}

scheduled <- VALFUN()
IDS       <- VALFUN()

#' @title Global Data
#' @name scheduled
#' @usage scheduled$get()
#' @description a list stored in an internal environment 
#' @rdname scheduled
#' @export
"scheduled"

#' @rdname scheduled
#' @usage IDS$get()
#' @export
"IDS"