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
install.packages("tidycensus")
library(tidycensus)

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv') |>
    mutate(county_name = gsub("\\(city\\)", "city", county_name)) |>
    mutate(county_name = gsub("\\(CA\\)", "", county_name)) |>
    filter(!county_name == "U.S. Minor Outlying Islands") |>
    filter(!state_name == "American Samoa" & !state_name == "Alaska" & !state_name == "Hawaii" & !state_name == "United States Virgin Islands" & !state_name == "Puerto Rico" & !state_name == "Commonwealth of the Northern Mariana Islands" & !state_name == "Guam") |>
    mutate(county_name = gsub("Doña", "Dona", county_name)) |>
    mutate(county_name = gsub("City city", "City", county_name))

us_sum <- us_place_names |>
    count(state_name, county_name)

#OK mais changer les colonnes pour rbind avec us_poly
ct_geom <- st_read("Connecticut/Connecticut.shp") |>
    mutate(new_region = gsub("CT", "Connecticut", new_region)) |>
    arrange(new_region) |>
    mutate(GEOID = seq(110, by = 10,length.out = nrow(ct_geom)))
# ct_geom <- ct_geom |>
#     st_as_sf(coords = "geometry", crs = 4326)
# us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')
# us_counties <- map_data("county") |>
#     mutate(across(c(region, subregion), ~gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", ., perl = TRUE))) |> #First character to uppercase
#     rename(state_name = "region", county_name = "subregion") |>
#     mutate(across(c(state_name, county_name), ~str_to_sentence(.)))
us_counties <- get(data(fips_codes)) |>
    rename(county_name = "county") |>
    mutate(county_name = gsub(" County", "", county_name)) |>
    mutate(GEOID = paste0(state_code, county_code)) |>
    filter(!state_name == "American Samoa" & !state_name == "Alaska" & !state_name == "Hawaii" & !state_name == "United States Virgin Islands" & !state_name == "Puerto Rico" & !state_name == "Commonwealth of the Northern Mariana Islands" & !state_name == "Guam") |>
    mutate(county_name = ifelse(state == "LA", gsub("La Salle", "LaSalle", county_name), county_name))

us_counties <- us_counties|>
    mutate(county_name = gsub(" Borough| Census Area| Municipality| City and Borough| Municipio| Parish", "", county_name))

us_poly <- tigris::counties(class = "sf", cb = TRUE)
us_states <- tigris::states(class = "sf", cb = TRUE) |>
    filter(!NAME == "American Samoa" & !NAME == "Alaska" & !NAME == "Hawaii" & !NAME == "United States Virgin Islands" & !NAME == "Puerto Rico" & !NAME == "Commonwealth of the Northern Mariana Islands" & !NAME == "Guam")

# us_counties_simple <- us_counties |>
#         distinct(group, county_name, .keep_all = TRUE)
us_counties2 <- merge(us_poly, us_counties, by = "GEOID")
# us_counties2 <- st_transform(us_counties2, crs = 5070)
# us_counties_sf <- st_as_sf(x = us_counties, coords = c("long", "lat"), crs = 5070)

# us_df <- merge(us_place_names, us_counties, by = c("state_name", "county_name"))
us_df <-2023-W26/Connecticut.csv merge(us_counties2, us_sum, by = c("state_name", "county_name"))
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
    geom_sf(data = us_df, aes(geometry = geometry, fill = as.numeric(n))) +
    geom_sf(data = us_states, aes(geometry = geometry), fill = NA, color = "black", linewidth = 0.3) +
    scale_fill_gradientn(name = "Number of geographic names", trans = "log", breaks = c(5, 25, 125, 625, 3125), colors = hcl.colors(20, "Lajolla", alpha = 0.8, rev = FALSE)) +
    theme_void() +
    theme(legend.key.width = unit(1.5, "cm"), legend.position = "bottom",
    panel.background = element_rect(fill = "#071D26", color = NA), plot.background = element_rect(fill = "#071D26", color = NA),
    legend.background = element_rect(fill = "#071D26", color = NA), legend.text = element_text(color = "#fff6ec", size = 20, face = "bold"),
    legend.title = element_text(color = "#fff6ec", size = 24, face = "bold", margin = margin(0, 0, 50, 0)), legend.title.align = 0.5,
    plot.title = element_text(face = "bold", color = "#fff6ec", size = 30, hjust = 0.5, vjust = 3, margin = margin(50, 0, 0, 0)))

