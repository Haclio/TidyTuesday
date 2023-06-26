setwd("./2023-W26")

library(tidyverse)
library(paletteer)
library(sf)
library(maps)
install.packages("usmap")
library(usmap)
install.packages("compare")
library(compare)
library(stringi)

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv') |>
    mutate(county_name = gsub("\\(city\\)", "city", county_name)) |>
    mutate(county_name = gsub("\\(CA\\)", "", county_name)) |>
    filter(!county_name == "U.S. Minor Outlying Islands") |>
    filter(!state_name == "American Samoa" & !state_name == "Alaska" & !state_name == "Hawaii" & !state_name == "United States Virgin Islands" & !state_name == "Puerto Rico" & !state_name == "Commonwealth of the Northern Mariana Islands" & !state_name == "Guam")

us_sum <- us_place_names |>
count(state_name, county_name)
# us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')
# us_counties <- map_data("county") |>
#     mutate(across(c(region, subregion), ~gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", ., perl = TRUE))) |> #First character to uppercase
#     rename(state_name = "region", county_name = "subregion") |>
#     mutate(across(c(state_name, county_name), ~str_to_sentence(.)))
us_counties <- get(data(fips_codes)) |>
    rename(county_name = "county") |>
    mutate(county_name = gsub(" County", "", county_name)) |>
    mutate(GEOID = paste0(state_code, county_code)) |>
    filter(!state_name == "American Samoa" & !state_name == "Alaska" & !state_name == "Hawaii" & !state_name == "United States Virgin Islands" & !state_name == "Puerto Rico" & !state_name == "Commonwealth of the Northern Mariana Islands" & !state_name == "Guam")

us_counties <- us_counties|>
    mutate(county_name = gsub(" Borough| Census Area| Municipality| City and Borough| Municipio| Parish", "", county_name))

us_poly <- tigris::counties(class = "sf", cb = TRUE)
# us_counties_simple <- us_counties |>
#         distinct(group, county_name, .keep_all = TRUE)
us_counties2 <- merge(us_poly, us_counties, by = "GEOID")
# us_counties2 <- st_transform(us_counties2, crs = 5070)
# us_counties_sf <- st_as_sf(x = us_counties, coords = c("long", "lat"), crs = 5070)

# us_df <- merge(us_place_names, us_counties, by = c("state_name", "county_name"))
us_df <- merge(us_counties2, us_sum, by = c("state_name", "county_name"))
us_df_max <- us_df |>
    filter(n > 1000)
us_df <- us_df |>
    filter(n <= 1000)
# ggplot(data = us_counties2,
#         mapping = aes(x = long, y = lat, group = group, fill = ncounty)) +
#     geom_polygon() +
#     coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#     guides(fill = FALSE) +
#     scale_fill_paletteer_c(name = "Number of geographic names -\n county level", palette = "grDevices::Lajolla")



# counties <- st_as_sf(map(c()"county", plot = FALSE, fill = TRUE))
# us_counties <- counties |>
#     mutate(state_name = sapply(strsplit(counties$ID, ","), "[", 1)) |>
#     mutate(county_name = sapply(strsplit(counties$ID, ","), "[", 2)) |>
#     mutate(across(c(state_name, county_name), ~gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", ., perl = TRUE)))

ggplot() +
    geom_sf(data = us_df, aes(geometry = geometry, fill = n)) +
    scale_fill_paletteer_c(name = "Number of geographic names -\n county level", palette = "grDevices::Viridis") +
    geom_sf(data = us_df_max, aes(geometry = geometry), fill = "red")
    coord_sf(default_crs = sf::st_crs(5070))
