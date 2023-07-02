setwd("./2023-W27")
library(tidyverse)
library(png)
library(jpeg)
library(grid)
library(ggtext)
library(patchwork)
library(htmltools)
library(cowplot)
library(glue)

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')
# no_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv')

mosquito <- historical_markers |>
                filter(grepl("mosquito", title, ignore.case = TRUE)) |>
                mutate(type = "Mosquito")

sandfly <- historical_markers |>
                filter(grepl("sandfly", title, ignore.case = TRUE)) |>
                mutate(type = "Sandfly")
sandflyimg <- readPNG("sandfly.png") # https://dic.academic.ru/pictures/wiki/files/80/Phlebotomus_pappatasi_bloodmeal_finished.jpg

bee <- historical_markers |>
                filter(grepl("beehive", title, ignore.case = TRUE) | grepl("Bee ", title) | grepl("Bee-Hive", title) | grepl("Beeville", title)) |>
                mutate(type = "Bee")

wasp <- historical_markers |>
                filter(grepl("wasp", title, ignore.case = TRUE)) |>
                mutate(type = "Wasp")
waspimg <- readPNG("waspimg.png") # https://upload.wikimedia.org/wikipedia/commons/f/f4/Wasp_March_2008-1.jpg

weevil <- historical_markers |>
                filter(grepl("weevil", title, ignore.case = TRUE)) |>
                mutate(type = "Weevil")
weevilimg <- readPNG("weevilimg.png") #https://www.flickr.com/photos/bareego/5584903730/

firebug <- historical_markers |>
                filter(grepl("firebug", title, ignore.case = TRUE)) |>
                mutate(type = "Firebug")

grasshopper <- historical_markers |>
                filter(grepl("grasshopper", title, ignore.case = TRUE)) |>
                mutate(type = "Grasshopper")

locust <- historical_markers |>
                filter(grepl("locust", title, ignore.case = TRUE)) |>
                mutate(type = "Locust")
locustimg <- readPNG("locust.png") #https://pxhere.com/en/photo/862090

louse <- historical_markers |>
                filter(grepl("louse", title, ignore.case = TRUE)) |>
                mutate(type = "Louse")

df <- rbind(bee, firebug, grasshopper, locust, louse, mosquito, sandfly, wasp, weevil)


# label <- c(
#   Weevil = "<img src='C:/Users/lloui/Documents/GitHub/TidyTuesday/2023-W27/weevilimg.png'> alt='blblbl'",
#   Wasp = glue("<img src='https://dic.academic.ru/pictures/wiki/files/80/Phlebotomus_pappatasi_bloodmeal_finished.jpg'/>")
# )

# img <- readPNG("https://via.placeholder.com/150/FFFF00/000000?Text=google.com")

label <- c(
  Wasp = readPNG("C:/Users/lloui/Documents/GitHub/TidyTuesday/2023-W27/weevilimg.png")
)


gg1 <- ggplot(df) +
    geom_bar(aes(x = fct_rev(fct_infreq(type)))) +
    coord_flip() +
    scale_x_discrete(name = NULL, labels = label) +
    theme_classic() +
    theme(axis.title = element_blank())
gg1

gg2 <- ggplot() +
    annotation_custom(rasterGrob(locustimg), x = , ymin = 0.75, ymax = 0.85) +
    annotation_custom(rasterGrob(sandflyimg), xmin = 0, xmax = 1, ymin = 0.2, ymax = 0.31) +
    annotation_custom(rasterGrob(waspimg), xmin = 0, xmax = 1, ymin = 0.08, ymax = 0.19) +
    annotation_custom(rasterGrob(weevilimg), xmin = 0, xmax = 1, ymin = -0.04, ymax = 0.07) +
    theme_minimal()

gg2 + gg1 +
    plot_layout(widths = c(0.1, 1))

# labels <- c(
#   Weevil = "<img src='locust.jpg' />",
#   Wasp = "<img src='waspimg.png' />"
# )
