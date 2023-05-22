install.packages("janitor")
install.packages("treemapify")
library(sf)
library(janitor)
library(treemapify)
setwd("./2023-W21")

# tuesdata <- tidytuesdayR::tt_load(2023, week = 21)
# squirrel_data <- tuesdata$squirrel_data

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') |>
    clean_names()

centralpark <- read_sf(dsn = "Map", layer  = "CentralPark")

centralpark |>
ggplot() +
geom_sf() +
geom_point(data = squirrel_data, aes(x = X, y = Y))

ggplot(squirrel_data |> drop_na(highlight_fur_color)) +
  geom_bar(aes(x = primary_fur_color, fill = highlight_fur_color),  position = position_fill()) +
  coord_flip()

squir_sum <- squirrel_data |>
  summarize()
ggplot(squirrel_data |> drop_na(highlight_fur_color), aes(area = highlight_fur_color, label = highlight_fur_color, subgroup = primary_fur_color)) +
geom_treemap()

