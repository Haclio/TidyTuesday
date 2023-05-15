setwd("./2023-W20")

# # Import and clean all the data
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

tornadoes <- read.csv("C:/Users/nadalinl.AD/Documents/GitHub/TidyTuesday/2023-W20/tornadoes.csv")

tornadoes <- tornadoes |>
  mutate(region = case_when(
                          st == "CT" | st == "ME" | st == "MA" | st == "NH"| st == "RI"| st == "VT" ~ "Northeast",
                          st == "IL" | st == "IN" | st == "MI" | st == "OH"| st == "WI"| st == "IA" | st == "KS" | st == "MN" | st == "MO" | st == "NE" | st == "ND"| st == "SD" ~ "Midwest",
                          st == "DE" | st == "FL" | st == "GA" | st == "NC"| st == "SC"| st == "VA" | st == "DC" | st == "WV" | st == "AL" | st == "KY" | st == "MS"| st == "TN" | st == "AR" | st == "LA" | st == "OK" | st == "TX" ~ "South",
                          st == "WA" | st == "AZ" | st == "CO" | st == "ID"| st == "MT"| st == "NV" | st == "NM" | st == "UT" | st == "WY" | st == "AK" | st == "CA"| st == "HI" | st == "OR" ~ "West",
                          TRUE ~ "Other"
)) |>
mutate(region = factor(region, levels = c("West", "Midwest", "South", "Northeast", "Other")))

tperstate  <- tornadoes |> 
 ggplot() +
 geom_bar(aes(x = fct_infreq(st), fill = region)) +
 ggtitle("Total number of tornadoes per U.S. state, 1950-2022") +
 xlab("State") +
 ylab("Number of tornadoes") +
 scale_fill_manual("Region", values = c25[2:6]) +
 theme_classic() +
 scale_y_continuous(expand = c(0, 0)) +
 theme(legend.position = c(0.6, 0.6))
tperstate
