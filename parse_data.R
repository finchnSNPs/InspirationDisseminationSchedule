#------------------------------------------------------------------------------#
# Zhian Kamvar, 2014-01-03
# This file will utilize libreoffice to convert the data in ods format to csv.
# The reason the data is not initially saved as csv is due to the fact that 
# google does not put quotations around the data itself, messing up the
# formatting.
#------------------------------------------------------------------------------#
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(googlesheets)

options(stringsAsFactors = FALSE)
any_given_sunday <- parse_date_time("Jan 4 2015", "mdy") + dweeks(0:101)
# lib_off_exec <- "/Applications/LibreOffice.app/Contents/MacOS/./soffice"
# ods_file     <- "RAW_DATA/2014_winter_ID_poll.ods"
# system(paste(lib_off_exec, "--invisible --convert-to csv", ods_file, 
#               "--outdir RAW_DATA"))
# IDS <- read.table("RAW_DATA/2014_winter_ID_poll.csv", sep = ",", head = TRUE,
#                   allowEscapes = TRUE)
# Sun Aug  9 18:38:57 2015 ------------------------------
# Note:
# 
# Switching to a new system using googlesheets The first time you use this
# script, a window will open where you will select the account you want 
# associated with google sheets (choose Inspiration Dissemination). After that,
# your authorization will be written to a file and you will never be asked
# again.
IDS <- gs_title("2014_winter_ID_poll") %>% gs_read()

category_names <- c("Timestamp", "Name", "Dept", "Deg", "PI", 
                    "Avail", "Pref", "Desc", "Email")
names(IDS) <- category_names

scheduled <- read.table("scheduled.csv", sep = ",", head = TRUE)
scheduled$Date <- parse_date_time(scheduled$Date, c("md", "mdy"))

# Takes in comma separated months and days, returns a vector of POSIX dates.
get_availability <- function(x){
  x <- read.table(text = x, sep = ",", head = FALSE)  
  x <- unlist(x, use.names = FALSE)
  y <- setNames(parse_date_time(x, c("md", "mdy")), x)
  return(y)
}

# Takes in a vector of sundays and returns a logical vector stating which
# Sundays in the year the person is available.
save_the_date <- function(x, any_given_sunday){
  setNames(any_given_sunday %in% x, any_given_sunday)
}

avail_list <- lapply(1:nrow(IDS), function(x) get_availability(IDS$Avail[x]))
names(avail_list) <- IDS$Name
IDS$Pref <- parse_date_time(IDS$Pref, "mdy")

sundays <- length(any_given_sunday)
avail_array <- array(dim = c(sundays, length(avail_list), 3), 
                     dimnames = list(Sundays = as.character(any_given_sunday),
                                     Guest   = names(avail_list),
                                     c("Available", "Preference", "Filled")))
is_available <- vapply(avail_list, save_the_date, logical(sundays), any_given_sunday)
is_preference <- vapply(1:nrow(IDS), function(i) save_the_date(IDS$Pref[i], any_given_sunday), logical(sundays))
avail_array[, , "Available"]  <- ifelse(is_available, "Available", "Unavailable")
avail_array[, , "Preference"] <- ifelse(is_preference, "Preference", NA)
avail_array[as.character(scheduled$Date), , "Filled"] <- "Scheduled"

compare_array <- function(x){
  res <- x[max(which(!is.na(x)))]
  if (res == "Scheduled") return(NA)
  return(res)
}
#avail_array[!rownames(avail_array) %in% as.character(scheduled$Date), , "Filled"] <- FALSE

unscheduled <- !dimnames(avail_array)$Guest %in% scheduled$Name
scheduled_to <- which(any_given_sunday == scheduled$Date[nrow(scheduled)])
availdf <- melt(avail_array[, unscheduled, "Available"])
preferencedf <- melt(avail_array[, unscheduled, "Preference"])
scheduledf <- melt(avail_array[, unscheduled, "Filled"])


avail_plot <- 
  ggplot(availdf, aes(x = Sundays, y = Guest, fill = value)) + 
  geom_tile() +
  geom_tile(aes(x = Sundays, y = Guest, fill = value, alpha = ifelse(is.na(value), 0, 1)), 
            data = preferencedf) + 
  geom_tile(aes(x = Sundays, y = Guest, fill = value, alpha = ifelse(is.na(value), 0, 0.75)), 
            data = scheduledf) +
  theme_classic() + 
  scale_alpha(guide = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(name = "Availability", 
                    labels = c("Available", "Unavailable", "Preference", "Scheduled"),
                    breaks = c("Available", "Unavailable", "Preference", "Scheduled"),
                    values = c(Preference = "#D7191C", Available = "#FDAE61", 
                               Scheduled = "grey25", Unavailable = "#2C7BB6")) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0))

if (interactive()) avail_plot

ggsave(filename = "availability.pdf", width = 11, height = 5.5)
outmat <- t(apply(avail_array, 1:2, compare_array))
write.table(x = outmat, file = "availability.csv", sep = ",", col.names = NA)

make_dossier <- function(x, df, xlist){
  dname <- sub(" ", "_", x)
  i <- which(df$Name == x)
  f <- file(paste("dossiers", paste(dname, "md", sep = "."), sep = "/"), "w")
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

for (i in names(avail_list)){
  make_dossier(i, IDS, avail_list)
}

system("cd dossiers; make all")
