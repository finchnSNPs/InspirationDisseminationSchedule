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

options(stringsAsFactors = FALSE)
any_given_sunday <- parse_date_time("Jan 4 2015", "mdy") + dweeks(0:101)

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
category_names <- c("Timestamp", "Name", "Email", "Dept", "Deg", "PI", 
                    "Avail", "Pref", "Desc")
gs_title("participants") %>% # register the google sheet
  gs_read() %>%                     # read it in as a data frame
  setNames(category_names) %>%      # set the names
  mutate(Pref = parse_date_time(Pref, "mdy")) %>% # recode preference as POSIX
  (IDS$set)                         # store in the IDS internal data holder.

# The procedure is similar here.
read.table("scheduled.csv", sep = ",", head = TRUE) %>% 
  mutate(Date = parse_date_time(Date, c("md", "mdy"))) %>%
  (scheduled$set)

avail_list <- lapply(IDS$get("Avail"), get_availability) %>%
  setNames(IDS$get("Name"))

sundays <- length(any_given_sunday)
avail_array <- array(dim = c(sundays, length(avail_list), 3), 
                     dimnames = list(Sundays = as.character(any_given_sunday),
                                     Guest   = names(avail_list),
                                     c("Available", "Preference", "Filled")))
is_available <- vapply(avail_list, save_the_date, logical(sundays), any_given_sunday)
is_preference <- vapply(1:nrow(IDS$get()), function(i) save_the_date(IDS$get("Pref")[i], any_given_sunday), logical(sundays))
avail_array[, , "Available"]  <- ifelse(is_available, "Available", "Unavailable")
avail_array[, , "Preference"] <- ifelse(is_preference, "Preference", NA)
avail_array[as.character(scheduled$get("Date")), , "Filled"] <- "Scheduled"

avail_array <- avail_array[any_given_sunday > ymd(Sys.Date()), , , drop = FALSE]

unscheduled <- !dimnames(avail_array)$Guest %in% scheduled$get("Name")
scheduled_to <- which(any_given_sunday == scheduled$get("Date")[nrow(scheduled$get())])
availdf <- melt(avail_array[, unscheduled, "Available", drop = FALSE])
preferencedf <- melt(avail_array[, unscheduled, "Preference", drop = FALSE])
scheduledf <- melt(avail_array[, unscheduled, "Filled", drop = FALSE])


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

xax <- axis_props(labels = list(angle = -90, baseline = "middle", align = "right"))

vis_plot <- avail_array[, unscheduled , -3, drop = FALSE] %>%
  apply(1:2, compare_array) %>%
  t %>% 
  melt %>%
  group_by(Guest) %>% 
  mutate(Scheduled = ifelse(is.na(avail_array[, unique(Guest), 3]), 1, 0.5)) %>%
  ggvis(x = ~Sundays, y = ~Guest, fill = ~value, opacity := ~Scheduled) %>%
  layer_rects(width = band(), height = band()) %>%
  add_tooltip(html = infotip, "hover") %>% 
  add_axis("x", title = "", properties = xax) %>%
  add_axis("y", title = "")


if (interactive()) vis_plot 

ggsave(filename = "availability.pdf", width = 11, height = 5.5)
outmat <- t(apply(avail_array, 1:2, compare_array))
write.table(x = outmat, file = "availability.csv", sep = ",", col.names = NA)


for (i in names(avail_list)){
  make_dossier(i, IDS, avail_list)
}

system("cd dossiers; make all")
