setwd("./2023-W25")

library(tidyverse)
library(here)
library(withr)
library(sf)
library(ggrepel)
# devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

url <- "https://github.com/jonthegeek/apis/raw/main/data/data_ufo_reports_with_day_part.rds"
ufo_path <- withr::local_tempfile(fileext = ".rds")
download.file(url, ufo_path)

ufo_data_original <- readRDS(ufo_path)

# We need to make the csv small enough that github won't choke. We'll pull out
# some of the joined data back into separate tables.

ufo_sightings <- ufo_data_original |> 
  dplyr::select(
    reported_date_time:city,
    state, 
    country_code,
    shape:has_images,
    day_part
  ) |> 
  # This got normalized after the data was saved, re-normalize.
  dplyr::mutate(
    shape = tolower(shape)
  )

places <- ufo_data_original |>
  dplyr::select(
    city:country_code, 
    latitude:elevation_m
  ) |> 
  dplyr::distinct()

# We'll also provide the map of "day parts" in case anybody wants to do
# something with that.
url2 <- "https://github.com/jonthegeek/apis/raw/main/data/data_day_parts_map.rds"
day_parts_path <- withr::local_tempfile(fileext = ".rds")
download.file(url2, day_parts_path)

day_parts_map <- readRDS(day_parts_path)

readr::write_csv(
  ufo_sightings,
  here::here(
    "2023-W25",
    "ufo_sightings.csv"
  )
)

readr::write_csv(
  places,
  here::here(
    "2023-W25",
    "places.csv"
  )
)

readr::write_csv(
  day_parts_map,
  here::here(
    "2023-W25",
    "day_parts_map.csv"
  )
)

# ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
# places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
# day_parts_map <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')

places_fr <- places |> #Cleaning region names
    filter(country == "France") |>
    mutate(state = case_when(
                state == "Auvergne-Rhone-Alpes" ~ "Auvergne-Rhône-Alpes",
                state == "Brittany" ~ "Bretagne",
                state == "Corsica" ~ "Corse",
                state == "Provence-Alpes-Cote d'Azur" ~ "Provence-Alpes-Côte d'Azur",
                state == "Ile-de-France" ~ "Île-de-France",
                state == "Normandy" ~ "Normandie",
                state == "Centre" ~ "Centre-Val de Loire",
                TRUE ~ state
    ))


ufo_sightings_fr <- ufo_sightings |> #Cleaning region names
    filter(country_code == "FR") |>
    mutate(state = case_when(
                state == "Auvergne-Rhone-Alpes" ~ "Auvergne-Rhône-Alpes",
                state == "Brittany" ~ "Bretagne",
                state == "Corsica" ~ "Corse",
                state == "Provence-Alpes-Cote d'Azur" ~ "Provence-Alpes-Côte d'Azur",
                state == "Ile-de-France" ~ "Île-de-France",
                state == "Normandy" ~ "Normandie",
                state == "Centre" ~ "Centre-Val de Loire",
                TRUE ~ state
    ))

#Building the dfs with the appropriate informations
regions <- read_sf("regions-20180101-shp/")
places_fr <- places_fr |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
st_geometry(places_fr) = "geometry_cit"

ufo_fr <- merge(ufo_sightings_fr, places_fr)
ufo_fr2 <- ufo_fr |>
    reframe(n = n(), .by = state) |>
    add_row(state = "Bourgogne-Franche-Comté", n = 0)
regions <- merge(regions, ufo_fr2, by.x = "nom", by.y = "state")
regions <- regions |>
    mutate(label = paste(regions$nom, ":", regions$n))
st_geometry(regions) = "geometry_reg"
ufo_fr <- merge(as.data.frame(regions), ufo_fr, by.x = "nom", by.y = "state", all = TRUE)
ufo_fr <- merge(ufo_fr, ufo_fr2, all = TRUE)




ggplot() +
    geom_sf(data = regions, aes(geometry = geometry_reg, fill = n), color = "black") +
    geom_sf(data = ufo_fr, aes(geometry = geometry_cit), shape = 15, color = "red", size = 3) +
    geom_sf_label_repel(data = regions, aes(label = label), fontface = "bold", force = 100, seed = 9, fill = alpha(c("white"), 0.5)) +
    coord_sf(xlim = c(-5.5,10), ylim = c(41,51), default_crs = sf::st_crs(4326)) +
    scale_fill_gradient(name = "UFO observations \n per region", low = "#C4FFC0", high = "#084911", breaks = c(0, 7), labels = c(0, 7)) +
    ggtitle("Declared UFO sightings in \n mainland France, 1978-2022") +
    theme_void() +
    theme(legend.key.width = unit(1.5, "cm"), legend.position = "bottom",
    panel.background = element_rect(fill = "#fff6ec", color = NA), plot.background = element_rect(fill = "#fff6ec", color = NA),
    legend.background = element_rect(fill = "#fff6ec", color = NA), legend.text = element_text(color = "#071D26", size = 20, face = "bold"),
    legend.title = element_text(color = "#071D26", size = 24, face = "bold", margin = margin(0, 0, 50, 0), vjust = 3), legend.title.align = 0.5,
    plot.title = element_text(face = "bold", color = "#071D26", size = 30, hjust = 0.5, vjust = 3, margin = margin(50, 0, 0, 0)))