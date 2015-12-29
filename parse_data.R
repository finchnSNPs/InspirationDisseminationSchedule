#------------------------------------------------------------------------------#
#' Zhian Kamvar, 2014-01-03
#' This file will utilize libreoffice to convert the data in ods format to csv.
#' The reason the data is not initially saved as csv is due to the fact that 
#' google does not put quotations around the data itself, messing up the
#' formatting.
#------------------------------------------------------------------------------#
library(scheduler)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggvis)
library(googlesheets)
#' Sun Aug  9 18:38:57 2015 ------------------------------
#' Note:
#' 
#' Switching to a new system using googlesheets The first time you use this
#' script, a window will open where you will select the account you want 
#' associated with google sheets (choose Inspiration Dissemination). After that,
#' your authorization will be written to a file and you will never be asked
#' again.
#' 
#' Reading in the data
#' -------------------
#' 
#' Using two internal data holders, this allows the main data, IDS and scheduled
#' to be safely read even if the user replaces them in his or her global
#' environment.
#' 
#' ### Globals
#' 
#' These are variables for use throughout the script
HELLNO <- FALSE
options(stringsAsFactors = HELLNO)

any_given_sunday <- parse_date_time("Jan 4 2015", "mdy") + dweeks(0:101)
sundays          <- length(any_given_sunday)
category_names   <- c("Timestamp", "Name", "Email", "Dept", "PI", "Deg", 
                      "Avail", "Pref", "Desc")
#' ### Data
#' 
#' This will read in the data and save it in the global variables for
#' the scheduler package.
#' 
#' The google seet we are using at the moment is called "participants" and it
#' has 9 columns corresponding to the "category_names". If this is ever to 
#' change, the category_names variable and this section should change.
#' 
gs_title("participants") %>%        # register the google sheet called "participants"
  gs_read() %>%                     # read it in as a data frame
  setNames(category_names) %>%      # set the names
  mutate(Pref = parse_date_time(Pref, "mdy")) %>% # recode preference as POSIX
  (IDS$set)                         # store in the IDS internal data holder.
#' 
#' The procedure is similar here, except we are reading in the data for the 
#' guests that have already been scheduled.
gs_title("scheduled_guests") %>% 
  gs_read() %>% 
  mutate(Date = parse_date_time(Date, c("md", "mdy"))) %>%
  (scheduled$set)
#' 
#' Since it's nice to have the CSV of previous guests available for quick 
#' reference, here we are sorting the incoming spreadsheet, making the date a
#' character string, and then writing a table.
sched_out <- scheduled$get() %>% 
  `[`(order(.$Date), ) %>% 
  mutate(Date = paste(month(Date, label = TRUE), day(Date), year(Date)))

write.table(sched_out, 
            file = "scheduled.csv", 
            sep = ",", 
            row.names = FALSE,
            col.names = TRUE)
#' Parsing the availability for the guests requires the creation of a separate
#' vector for each guest containing POSIX dates. Since each guest can have a
#' different number of availabilities, this should be a list.
avail_list <- lapply(IDS$get("Avail"), get_availability) %>%
  setNames(IDS$get("Name"))
#' Creating a 3 dimensional array to store the values.
#' 
#' The rows will be the dates and columns will be the guests. The third
#' dimension will contain indicators of their availability, their preference,
#' and whether or not the slot has been filled.
avail_array <- array(dim = c(sundays, length(avail_list), 3), 
                     dimnames = list(Sundays = as.character(any_given_sunday),
                                     Guest   = names(avail_list),
                                     c("Available", "Preference", "Filled")))
#'
#' Analysis
#' --------
#' 
#' ### Filling the array

# Creating logical vectors
is_available  <- vapply(avail_list, save_the_date, logical(sundays), any_given_sunday)
is_preference <- vapply(IDS$get("Pref"), save_the_date, logical(sundays), any_given_sunday)

# Guest Availability
avail_array[, , "Available"]  <- ifelse(is_available, "Available", "Unavailable")

# Guest preference
avail_array[, , "Preference"] <- ifelse(is_preference, "Preference", NA)

# Scheduled slots
Sched_Sunday <- rownames(avail_array) %in% as.character(scheduled$get("Date"))
avail_array[Sched_Sunday, , "Filled"] <- "Scheduled"

# Removing the rows that are in the past.
avail_array <- avail_array[any_given_sunday > ymd(Sys.Date()), , , drop = FALSE]

# Who still needs to be scheduled?
unscheduled <- !dimnames(avail_array)$Guest %in% scheduled$get("Name")
#'
#' Graphs
#' ---------
#' 
#' Now that we have all of our data, we can create data frames to use for
#' plotting in ggplot2. This will create a pdf that can be shared.

# step 1: create the data frames for each layer.
availdf      <- melt(avail_array[, unscheduled, "Available", drop = FALSE])
preferencedf <- melt(avail_array[, unscheduled, "Preference", drop = FALSE])
scheduledf   <- melt(avail_array[, unscheduled, "Filled", drop = FALSE])


avail_plot <- 
  ggplot(availdf, aes(x = Sundays, y = Guest, fill = value)) + 
  geom_tile() +
  geom_tile(aes(x = Sundays, y = Guest, fill = value, 
                alpha = ifelse(is.na(value), 0, 1)), 
            data = preferencedf) + 
  geom_tile(aes(x = Sundays, y = Guest, fill = value, 
                alpha = ifelse(is.na(value), 0, 0.75)), 
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
ggsave(filename = "availability.pdf", width = 11, height = 5.5)
#'
#' Since it becomes difficult to look at the above plot when there are a large
#' number of participants, I realized it was better to display this using ggvis
#' so that, if run interactively, it could have a tooltip so you can hover over
#' a tile and see its date and if the guest is available.

# properties to make the x axis a little prettier
xax <- axis_props(labels = list(angle = -90, baseline = "middle", align = "right"))

vis_plot <- avail_array[, unscheduled , -3, drop = FALSE] %>%
  apply(1:2, compare_array) %>% # compare the availability hierarchy
  t %>%                         # transpose the result
  melt %>%                      # melt into a data frame
  group_by(Guest) %>%           # Add a scheduled column for opacity values
  mutate(Scheduled = ifelse(is.na(avail_array[, unique(Guest), 3]), 1, 0.5)) %>%
  ggvis(x = ~Sundays, y = ~Guest, fill = ~value, opacity := ~Scheduled) %>%
  layer_rects(width = band(), height = band()) %>%
  add_tooltip(html = infohover, "hover") %>% 
  add_tooltip(html = infoclick, "click") %>% 
  add_axis("x", title = "", properties = xax) %>%
  add_axis("y", title = "")

vis_plot # show the plot
#' Cleanup
#' -------
#' Now, we write out all the information we gathered and create the dossiers
#' so that we can take a look at them later.

# availablility table in text form
outmat <- t(apply(avail_array, 1:2, compare_array))
write.table(x = outmat, file = "availability.csv", sep = ",", col.names = NA)

# Writing the dossiers in markdown
for (i in names(avail_list)){
  fname <- make_filename(i, wd = ".", newdir = "/dossiers/md_files/")
  if (!file.exists(fname)){
    make_dossier(i, IDS$get(), avail_list, wd = ".")
  }
}

# processing the dossiers with pandoc
system("cd dossiers; make all")
