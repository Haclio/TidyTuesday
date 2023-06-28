setwd("./2023-W26")
library(tidyverse)
library(paletteer)
library(sf)
library(maps)
# install.packages("usmap")
library(usmap)
# install.packages("compare")
library(compare)
library(stringi)
# install.packages("tidycensus")
library(tidycensus)

#Greographical names counts by county
us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv') |>
    mutate(county_name = gsub("\\(city\\)", "city", county_name)) |>
    mutate(county_name = gsub("\\(CA\\)", "", county_name)) |>
    filter(!county_name == "U.S. Minor Outlying Islands") |>
    filter(!state_name == "American Samoa" & !state_name == "Alaska" & !state_name == "Hawaii" & !state_name == "United States Virgin Islands" & !state_name == "Puerto Rico" & !state_name == "Commonwealth of the Northern Mariana Islands" & !state_name == "Guam") |>
    mutate(county_name = gsub("Doña", "Dona", county_name)) |>
    mutate(county_name = gsub("City city", "City", county_name))

us_sum <- us_place_names |>
    count(state_name, county_name)

#Geographical data by county
us_counties <- get(data(fips_codes)) |>
    rename(county_name = "county") |>
    mutate(county_name = gsub(" County", "", county_name)) |>
    mutate(GEOID = paste0(state_code, county_code)) |>
    filter(!state_name == "American Samoa" & !state_name == "Alaska" & !state_name == "Hawaii" & !state_name == "United States Virgin Islands" & !state_name == "Puerto Rico" & !state_name == "Commonwealth of the Northern Mariana Islands" & !state_name == "Guam") |>
    mutate(county_name = ifelse(state == "LA", gsub("La Salle", "LaSalle", county_name), county_name))

us_counties <- us_counties|>
    mutate(county_name = gsub(" Borough| Census Area| Municipality| City and Borough| Municipio| Parish", "", county_name))

us_poly <- tigris::counties(class = "sf", cb = TRUE) |>
    rename(state_name = "STATE_NAME") |>
    rename(county_name = "NAME")
ct_geom <- st_read("Connecticut/Connecticut.shp") |>
    mutate(new_region = gsub("CT", "Connecticut", new_region)) |>
    arrange(new_region) |>
    mutate(GEOID = paste0("09", seq(110, 190, by = 10))) |>
    rename(county_name = "new_region") |>
    mutate(state_name = "Connecticut") |>
    select(county_name, GEOID, state_name) |>
    mutate(county_name = gsub("ern", "ern Connecticut", county_name)) |>
    mutate(county_name = gsub("Central", "Central Connecticut", county_name)) |>
    mutate(county_name = gsub("Capitol Region", "Capitol", county_name)) |>
    st_transform(crs = 4269)
us_poly <- bind_rows(us_poly, ct_geom)

us_states <- tigris::states(class = "sf", cb = TRUE) |>
    filter(!NAME == "American Samoa" & !NAME == "Alaska" & !NAME == "Hawaii" & !NAME == "United States Virgin Islands" & !NAME == "Puerto Rico" & !NAME == "Commonwealth of the Northern Mariana Islands" & !NAME == "Guam")


us_counties2 <- merge(us_poly, us_counties, by = c("state_name", "county_name", "GEOID"))

us_df <- merge(us_counties2, us_sum, by = c("state_name", "county_name"))

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

