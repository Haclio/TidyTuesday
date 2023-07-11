library(hms)
library(tidyverse)
library(here)
setwd("./2023/2023-W20")

# # Import and clean all the data, just once
# tuesdata <- tidytuesdayR::tt_load(2023, week = 20)

# url <- "https://www.spc.noaa.gov/wcm/data/1950-2022_actual_tornadoes.csv"

# # Some of the automatic column types are imperfect Get that spec and then
# # update it.
# tornadoes <- read_csv(url)
# spec(tornadoes) # Copy/pasted into col_types below then edited.
# tornadoes <- read_csv(
#   url,
#   col_types = cols(
#     om = col_integer(),
#     yr = col_integer(),
#     mo = col_integer(),
#     dy = col_integer(),
#     date = col_date(format = ""),
#     time = col_time(format = ""),
#     tz = col_integer(),
#     st = col_factor(),
#     stf = col_integer(),
#     stn = col_integer(),
#     mag = col_integer(),
#     inj = col_integer(),
#     fat = col_integer(),
#     loss = col_double(),
#     closs = col_double(),
#     slat = col_double(),
#     slon = col_double(),
#     elat = col_double(),
#     elon = col_double(),
#     len = col_double(),
#     wid = col_integer(),
#     ns = col_integer(),
#     sn = col_integer(),
#     sg = col_integer(),
#     f1 = col_integer(),
#     f2 = col_integer(),
#     f3 = col_integer(),
#     f4 = col_integer(),
#     fc = col_integer()
#   )
# )

# glimpse(tornadoes)

# # This table only contains one segment per tornado, so we can drop the sg
# # column.
# tornadoes$sg <- NULL

# # The tz column is confusing in the provided dictionary
# # (https://www.spc.noaa.gov/wcm/data/SPC_severe_database_description.pdf).
# # Investigate it to make sense of the various values.
# tornadoes |> 
#   count(tz)

# # The doc says 3 == CST, and 9 == GMT. 0 appears to be NA. What is 6? 
# tornadoes |> 
#   filter(tz == 6) |>
#   count(st)

# # All tornadoes with tz == 6 are in Mountain Time states, so we'll make that
# # assumption. Update time encoding.

# tornadoes <- tornadoes |> 
#   # We can't really judge even what day the recording was on for unknown tz, so
#   # drop those values.
#   filter(tz != 0) |> 
#   mutate(
#     # Make the remaining tz's more meaningful. We'll assume they meant Central
#     # (daylight or standard) for "CST", and likewise that they meant what we now
#     # call UTC for "GMT". "GMT" sometimes includes BST so we'll avoid using that
#     # name.
#     tz = case_match(
#       tz,
#       3 ~ "America/Chicago",
#       6 ~ "America/Denver",
#       9 ~ "UTC"
#     ),
#     # Add a datetime_utc column to normalize the times. ymd_hms only wants a
#     # single timezone (not a vector of them), so break it up with a case_match.
#     datetime_utc = case_match(
#       tz,
#       "America/Chicago" ~ lubridate::ymd_hms(
#         paste(date, time),
#         tz = "America/Chicago"
#       ),
#       "America/Denver" ~ lubridate::ymd_hms(
#         paste(date, time),
#         tz = "America/Denver"
#       ),
#       "UTC" ~ lubridate::ymd_hms(
#         paste(date, time),
#         tz = "UTC"
#       ) 
#     ) |> 
#       lubridate::with_tz("UTC"),
#     .after = tz
#   ) |> 
#   # Drop stn because it was discontinued and was inconsistent before being
#   # discontinued. closs (crop loss) has an unexplained discontinuity in 2016 and
#   # it isn't entirely clear what changed.
#   select(-"stn", -"closs") |> 
#   # Recode some more weird columns.
#   mutate(
#     # The mag column uses -9 for NA.
#     mag = na_if(mag, -9),
#     # The loss column is confusingly coded. Let's attempt to make it make sense.
#     # The documentation (last updated in 2010) explains that the coding changed in
#     # 1996. Observationally, it's clear that it changed again in 2016.
#     loss = case_when(
#       loss == 0 ~ NA,
#       yr < 1996 & loss == 1 ~ 50,
#       yr < 1996 & loss == 2 ~ 500,
#       yr < 1996 & loss == 3 ~ 5000,
#       yr < 1996 & loss == 4 ~ 50000,
#       yr < 1996 & loss == 5 ~ 500000,
#       yr < 1996 & loss == 6 ~ 5000000,
#       yr < 1996 & loss == 7 ~ 50000000,
#       yr < 1996 & loss == 8 ~ 500000000,
#       yr < 1996 & loss == 9 ~ 5000000000,
#       yr >= 1996 & yr < 2016 ~ loss * 1e6,
#       TRUE ~ loss
#     ),
#     # The fc column is really a "was mag estimated" column
#     fc = as.logical(fc)
#   )

# # Some of the remaining columns are confusing, but we'll explain them in the
# # dictionary and see what people find!

# write_csv(
#   tornadoes,
#   here(
#     "2023-W20",
#     "tornadoes.csv"
#   )
# )

#The next times, just read the created csv
tornadoes <- read.csv("C:/Users/nadalinl.AD/Documents/GitHub/TidyTuesday/2023-W20/tornadoes.csv")

tornadoes <- tornadoes |>
  mutate(region = case_when( #Adding a 'region' column
                          st == "CT" | st == "ME" | st == "MA" | st == "NH"| st == "RI"| st == "VT" ~ "Northeast",
                          st == "IL" | st == "IN" | st == "MI" | st == "OH"| st == "WI"| st == "IA" | st == "KS" | st == "MN" | st == "MO" | st == "NE" | st == "ND"| st == "SD" ~ "Midwest",
                          st == "DE" | st == "FL" | st == "GA" | st == "NC"| st == "SC"| st == "VA" | st == "DC" | st == "WV" | st == "AL" | st == "KY" | st == "MS"| st == "TN" | st == "AR" | st == "LA" | st == "OK" | st == "TX" ~ "South",
                          st == "WA" | st == "AZ" | st == "CO" | st == "ID"| st == "MT"| st == "NV" | st == "NM" | st == "UT" | st == "WY" | st == "AK" | st == "CA"| st == "HI" | st == "OR" ~ "West",
                          TRUE ~ "Other"
)) |>
mutate(region = factor(region, levels = c("West", "Midwest", "South", "Northeast", "Other"))) |> #Ordering of the 'region' factor levels
mutate(time = as_hms(time)) |> #Transforming the 'time' variable into an appropriate time class
mutate(mag = as.factor(mag)) #Transforming the 'mag' variable into a factor
tornadoes$mo <- month.abb[tornadoes$mo] #Replacing the months' numbers by their abbreviated names
tornadoes %<>% #Reordering the months factor levels
  mutate(mo = factor(mo, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

tperyear <- tornadoes |> #Number of tornadoes per year
  ggplot() +
  geom_bar(aes(x = yr), fill = "cadetblue") +
  scale_y_continuous(breaks = seq(0, 2000, 250), expand = c(0.005, 0)) + #Sets the axis breaks and distance from the data
  scale_x_continuous(breaks = seq(1950, 2022, 5), expand = c(0, 0)) + #Sets the axis breaks and distance from the data
  xlab("Year") + #Renaming axes
  ylab("Number of tornadoes") + #Renaming axes
  ggtitle("Total number of tornadoes each year, 1950-2022") + #Sets plot title
  theme(panel.grid.major.y = element_line(color = "white", linewidth = 0.1), 
  panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), #panel.grid.* sets the background grid
  axis.text.x = element_text(color = 'white'), axis.text.y = element_text(color = 'white'), #Changes the axes breaks text color
  axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), #Changes the axes title fontface
  text = element_text(color = 'white', size = 16), #Should set the color and size of all the text on the plot
  axis.ticks.x = element_line(color = "white", linewidth = 0.5), #Sets the color and width of the axis ticks
  panel.background = element_rect(fill = 'grey25'), plot.background = element_rect(fill = 'grey25')) #Sets the panel and background color
tperyear

tpermonth <- tornadoes |> #Number of tornadoes each month of the year
  ggplot() +
  geom_bar(aes(x = mo), fill = "cadetblue") +
  scale_y_continuous(breaks = seq(0, 15000, 1000), expand = c(0, 0)) +
  xlab("Month") +
  ylab("Number of tornadoes") +
  ggtitle("Total number of tornadoes each month, 1950-2022") +
  theme(panel.grid.major.y = element_line(color = "white", linewidth = 0.1),
  panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(color = 'white'), axis.text.y = element_text(color = 'white'),
  axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
  text = element_text(color = 'white', size = 16),
  panel.background = element_rect(fill = 'grey25'), plot.background = element_rect(fill = 'grey25'))
tpermonth
 
tperstate <- tornadoes |>  #Number of tornadoes per state
 ggplot() +
 geom_bar(aes(x = fct_infreq(st), fill = region)) +
 ggtitle("Total number of tornadoes per U.S. state, 1950-2022") +
 xlab("State") +
 ylab("Number of tornadoes (total, 1950-2022)") +
 scale_fill_manual("Region", values = c25[18:22]) +
 scale_y_continuous(breaks = seq(0, 10000, 1000), expand = c(0, 0)) +
 theme(legend.position = c(0.942, 0.875), legend.background = element_rect(fill = "grey25", color = "white"), #Sets the legend position and background color
  legend.key.size = unit(1, 'cm'), legend.title = element_text(size=16, face = "bold"), #Sets the legend title font size and fontface
  legend.text = element_text(size=16, face = "bold"), #Sets the legend text font size and fontface
  panel.grid.major.y = element_line(color = "white", linewidth = 0.1),
  panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold", size = 20),
  axis.text.x = element_text(color = 'white'), axis.text.y = element_text(color = 'white'),
  axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
  text = element_text(color = 'white', size = 12),
  panel.background = element_rect(fill = 'grey25'), plot.background = element_rect(fill = 'grey25'))
tperstate

ttimes <- tornadoes |> #Number of tornadoes per time of day
  # filter(yr >= 2000) |> #Is used to filter the data based on the 'year' variable
  ggplot() +
  geom_bar(aes(x = time), fill = "cadetblue") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_time(limits = as_hms(c("00:00:00", "23:59:59")), breaks = as_hms(c("00:00:00", "04:00:00", "08:00:00", "12:00:00", "16:00:00", "20:00:00", "24:00:00"))) + #Sets the limits and breaks for the x axis
  xlab("Time of day") +
  ylab("Number of tornadoes") +
  ggtitle("Tornadoes reported by time of day, 1950-2022") +
  theme(panel.grid.major.y = element_line(color = "white", linewidth = 0.1),
  panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(color = 'white'), axis.text.y = element_text(color = 'white'),
  axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
  text = element_text(color = 'white', size = 16),
  axis.ticks.x = element_line(color = "white", linewidth = 0.5),
  panel.background = element_rect(fill = 'grey25'), plot.background = element_rect(fill = 'grey25'))
ttimes

tdims <- tornadoes |> #Dimensions of tornado paths
  ggplot() +
  geom_point(aes(x = len, y = wid), color = "cadetblue") +
  scale_y_continuous(expand = c(0, 1)) +
  scale_x_continuous(expand = c(0, 1)) +
  xlab("Path length (miles)") +
  ylab("Path width (yards)") +
  ggtitle("Tornadoes path size, 1950-2022") +
  theme(panel.grid.major.y = element_line(color = "white", linewidth = 0.1),
  panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
  axis.text.x = element_text(color = 'white'), axis.text.y = element_text(color = 'white'),
  axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"),
  text = element_text(color = 'white', size = 16),
  axis.ticks.x = element_line(color = "white", linewidth = 0.5),
  panel.background = element_rect(fill = 'grey25'), plot.background = element_rect(fill = 'grey25'))
tdims