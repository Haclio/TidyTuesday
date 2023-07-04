setwd("./2023-W27")
library(tidyverse)
library(png)
# install.packages("ggtext")
library(ggtext)
library(extrafont)
font_import()
loadfonts(device = "win")

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')
# no_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv')

mosquito <- historical_markers |>
                filter(grepl("mosquito", title, ignore.case = TRUE)) |>
                mutate(type = "Mosquito")
# Personal picture

sandfly <- historical_markers |>
                filter(grepl("sandfly", title, ignore.case = TRUE)) |>
                mutate(type = "Sandfly")
# https://dic.academic.ru/pictures/wiki/files/80/Phlebotomus_pappatasi_bloodmeal_finished.jpg

bee <- historical_markers |>
                filter(grepl("beehive", title, ignore.case = TRUE) | grepl("Bee ", title) | grepl("Bee-Hive", title) | grepl("Beeville", title)) |>
                mutate(type = "Bee")
# https://upload.wikimedia.org/wikipedia/commons/c/cd/Honeybee-27527-1.jpg

wasp <- historical_markers |>
                filter(grepl("wasp", title, ignore.case = TRUE)) |>
                mutate(type = "Wasp")
# https://upload.wikimedia.org/wikipedia/commons/f/f4/Wasp_March_2008-1.jpg

weevil <- historical_markers |>
                filter(grepl("weevil", title, ignore.case = TRUE)) |>
                mutate(type = "Weevil")
# https://www.flickr.com/photos/bareego/5584903730/

firebug <- historical_markers |>
                filter(grepl("firebug", title, ignore.case = TRUE)) |>
                mutate(type = "Firebug")
# https://upload.wikimedia.org/wikipedia/commons/6/6a/Pyrrhocoris_apterus_%28punaise_rouge%29.JPG

grasshopper <- historical_markers |>
                filter(grepl("grasshopper", title, ignore.case = TRUE)) |>
                mutate(type = "Grasshopper")
# https://upload.wikimedia.org/wikipedia/commons/3/37/Heupferd_fg01.jpg

locust <- historical_markers |>
                filter(grepl("locust", title, ignore.case = TRUE)) |>
                mutate(type = "Locust")
# https://pxhere.com/en/photo/862090

louse <- historical_markers |>
                filter(grepl("louse", title, ignore.case = TRUE)) |>
                mutate(type = "Louse")
# https://upload.wikimedia.org/wikipedia/commons/4/45/Male_human_head_louse.jpg

df <- rbind(bee, firebug, grasshopper, locust, louse, mosquito, sandfly, wasp, weevil)
df_sum <- df |>
  reframe(n = n(), .by = type)
fct_relevel(df_sum$type, c("Bee", "Locust", "Grasshopper", "Mosquito", "Firebug", "Louse", "Sandfly", "Wasp", "Weevil"))

label <- c(
  Weevil = "<img src='weevilimg.png' height = 65>",
  Wasp = "<img src='waspimg.png' height = 65>",
  Sandfly = "<img src='sandfly.png' height = 65>",
  Locust = "<img src='locust.png' height = 65>",
  Louse = "<img src='louse.png' height = 65>",
  Mosquito = "<img src='mosquito.png' height = 65>",
  Bee = "<img src='bee.png' height = 65>",
  Firebug = "<img src='firebug.png' height = 65>",
  Grasshopper = "<img src='grasshopper.png' height = 65>")

ggplot(df_sum, aes(x = reorder(fct_rev(type), n))) +
    geom_col(aes(y = n), width = 0.8, fill = "#d1941b") +
    geom_text(aes(y = n -0.5, label = n), color = "#10290d", size = 14, family = "Perpetua", fontface = "bold") +
    geom_text(aes(y = n + 0.5, label = type, hjust = 0), color = "grey60", size = 12, family = "serif", fontface = "italic") +
    coord_flip() +
    scale_x_discrete(name = NULL, labels = label) +
    scale_y_continuous(expand = expansion(mult = c(0.5,0.5))) +
    # ylim(expand = c(0.5, 0.5)) + #ERROR
    theme_void() +
    theme(axis.title = element_blank(), axis.text.y = element_markdown(size = 10, margin = margin(0, -30, 0, 0)),
    plot.background = element_rect(fill = "#10290d", color = NA), panel.background = element_rect(fill = "#10290d", color = NA),
    plot.margin = margin(1,1,1.5,1.2, "cm"))
