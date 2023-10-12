setwd("./2023/2023-W41")
library(tidyverse)
library(ggtext)
library(showtext)
library(usmap)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp", regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
                         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
                         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro
font_add(family = "gloom", regular = paste(fontsfolder, "Gloomy Things", "GloomyThings-3zBv3.ttf", sep = "/")) #Brand logos in fonts directory

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: the Shadowlands Haunted Places Index | #TidyTuesday week 41 2023"
sub <- "It's spooky season again ! Today's data comes from the **Shadowlands Haunted Places Index**,<br>
        which is 'A state by state, country by country index of haunted places'.<br>
        The map below shows reports of <span style = 'color: #0bd8bd;'>**ghosts**</span>,
        <span style = 'color: #913cd6;'>**witches**</span>, <span style = 'color: #c21b29;'>**demons**</span>, and descriptions mentioning <span style = 'color: #e48900;'>**Halloween**</span>."

#Data wrangling
haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')
haunted_t <- haunted_places |>
    relocate(c(longitude, latitude), .before = city) |>
    filter(!is.na(longitude) | !is.na(latitude)) |>
    filter(longitude < -66) |>
    filter(!row_number() %in% c(3028, 5134)) #Wrong coordinates
haunted_t <- usmap_transform(haunted_t, input_names = c("longitude", "latitude"), output_names = c("lon", "lat"))

plot_usmap(color = "grey70", fill = "grey10", alpha = 0.8) +
    geom_point(data = haunted_t |> filter(grepl("ghost", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#0bd8bd", size = 2, alpha = 0.2) +
    geom_point(data = haunted_t |> filter(grepl("ghost", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#41d6c2", size = 1, alpha = 0.2) +
    geom_point(data = haunted_t |> filter(grepl("witch", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#570f92", size = 2, alpha = 0.3) +
    geom_point(data = haunted_t |> filter(grepl("witch", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#a14dd1", size = 1, alpha = 0.3) +
    geom_point(data = haunted_t |> filter(grepl("demon", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#9e111d", size = 2, alpha = 0.3) +
    geom_point(data = haunted_t |> filter(grepl("demon", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#d31c34", size = 1, alpha = 0.3) +
    geom_point(data = haunted_t |> filter(grepl("halloween", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#af7113", size = 2, alpha = 0.4) +
    geom_point(data = haunted_t |> filter(grepl("halloween", description, ignore.case = TRUE)), aes(x = lon, y = lat), color = "#c99119", size = 1, alpha = 0.4) +
    labs(title = "Haunted places of the USA", subtitle = sub, caption = cap) + #Title, subtitle, caption
    theme_minimal() +
    theme(axis.line = element_blank(), #No y axis line
          axis.text = element_blank(), #Axis labels formatting
          axis.ticks = element_blank(), #No ticks
          axis.title = element_blank(), #No axis title
          panel.background = element_rect(fill = "grey5", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "grey5", color = NA), #Background color
          plot.caption = element_markdown(margin = margin(10, 0, -5, 0), size = 12, color = "grey90"), #Caption formatting
          plot.margin = unit(c(1,1,0.5,0.5),"cm"), #Margins
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 20, lineheight = 1.2, color = "grey90"), #Subtitle settings
          plot.title = element_markdown(size = 90, family = "gloom", margin = margin(b = 25), hjust = 0.5, color = "grey90"), #Title settings
          plot.title.position = "plot",
          text = element_text(family = "ssp")) #General font
