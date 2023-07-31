setwd("./2023/2023-W27")
library(tidyverse)
# install.packages("ggtext")
library(ggtext)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "pl", regular = "C:/Windows/Fonts/pala.ttf") #Palatino Linotype

cap <- paste0("<span style='font-family:fb;'>&#xf09b; </span> Haclio  |", #Caption
              "<span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Historical Markers Database | #TidyTuesday week 27")

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
search <- c(paste0("\"Bee\"", ", \"beehive\",", "\n \"bee-hive\"", ", \"Beeville\""), paste("\"Firebug\""), paste("\"Grasshopper\""), paste("\"Locust\""), paste("\"Louse\""), paste("\"Mosquito\""), paste("\"Sandfly\""), paste("\"Women Airforce \n Service Pilots (WASP)\""), paste("\"Weevil\""))
df_sum$search <- search
df_sum <- rbind(c("", NA, "Query :"), df_sum) |>
  mutate(n = as.numeric(n))


label <- c( #Array of pictures to use as axis labels
  Weevil = "<img src='weevilimg.png' height = 60>",
  Wasp = "<img src='waspimg.png' height = 60>",
  Sandfly = "<img src='sandfly.png' height = 60>",
  Locust = "<img src='locust.png' height = 60>",
  Louse = "<img src='louse.png' height = 60>",
  Mosquito = "<img src='mosquito.png' height = 60>",
  Bee = "<img src='bee.png' height = 60>",
  Firebug = "<img src='firebug.png' height = 60>",
  Grasshopper = "<img src='grasshopper.png' height = 60>")

ggplot(df_sum, aes(x = reorder(fct_rev(type), n))) + #fct_rev to order from max to min
    geom_col(aes(y = n), width = 0.8, fill = "#d1941b") +
    geom_text(aes(y = n - 0.25, label = n, hjust = 1), color = "#071306", size = 12, fontface = "bold") +
    geom_text(aes(y = n + 0.8, label = type, hjust = 0), color = "grey60", size = 12, family = "serif", fontface = "italic") +
    geom_text(aes(y = 20, label = search), color = "#b7cfb5", family = "serif", fontface = "italic", size = 5) +
    coord_flip() + #Rotate the graph
    scale_x_discrete(name = NULL, labels = label) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
    labs(title = "Historical (bug) markers", subtitle = "Bugs in historical markers names", caption = cap) +
    theme_void() +
    theme(axis.title = element_blank(), axis.text.y = element_markdown(size = 10, margin = margin(0, -30, 0, 0)), #element_markdown necessary for the pictures to show
    plot.background = element_rect(fill = "#071306", color = NA), panel.background = element_rect(fill = "#071306", color = NA),
    plot.margin = margin(1, 1, 1, 1.2, "cm"), plot.caption = element_markdown(color = "#b7cfb5", margin = margin(t = 10, b = 0), size = 12, vjust = -10), #element_markdown necessary for the logos to appear
    plot.title = element_text(hjust = 0.5, colour = "#f8c96c", family = "pl", size = 50, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, colour = "grey60", family = "pl", size = 25, face = "bold", vjust = 0.1))
