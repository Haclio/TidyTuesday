setwd("./2023-W26")

library(tidyverse)
library(paletteer)
library(sf)
library(maps)
install.packages("usmap")
library(usmap)

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv') 
us_sum <- us_place_names |>
reframe(ncounty = n(), .by = c(state_name, county_name))
# us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')
# us_counties <- map_data("county") |>
    # mutate(across(c(region, subregion), ~gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", ., perl = TRUE))) |> #First character to uppercase
    # rename(state_name = "region", county_name = "subregion")
# us_counties_sf <- st_as_sf(x = us_counties, coords = c("long", "lat"), crs = 5070)

us_df <- merge(us_place_names, us_counties, by = c("state_name", "county_name"))
us_df <- merge(us_df, us_sum, by = c("state_name", "county_name"))

ggplot(data = us_df,
        mapping = aes(x = long, y = lat, group = group, fill = ncounty)) +
    geom_polygon() +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    guides(fill = FALSE) +
    scale_fill_paletteer_c(name = "Number of geographic names -\n county level", palette = "grDevices::Lajolla")



counties <- st_as_sf(map(c()"county", plot = FALSE, fill = TRUE))
us_counties <- counties |>
    mutate(state_name = sapply(strsplit(counties$ID, ","), "[", 1)) |>
    mutate(county_name = sapply(strsplit(counties$ID, ","), "[", 2)) |>
    mutate(across(c(state_name, county_name), ~gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", ., perl = TRUE)))

ggplot() +
    geom_sf(data = us_counties, aes(geometry = geometry, fill = n), color = "black")
map(c("U"), fill = TRUE)

plot_usmap(regions = "counties")
usmapdata$counties
us_map(regions = "counties")
