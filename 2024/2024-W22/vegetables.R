setwd("./2024/2024-W22")
library(tidyverse)
library(ggtext)
library(showtext)
library(ggforce)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp",
         regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: World Bank |  #TidyTuesday week 18 2024"
sub <- "<i>Average value for each group is displayed with a black line. Only the latest data for each country is shown.<br> &nbsp; Total number of countries: 111. Low/high income refers to the country's income bracket as per the World Bank.</i>"

#Data wrangling
harvest_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2020.csv')
harvest_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2021.csv')

tomato2020 <- harvest_2020 |>
    filter(vegetable == "tomatoes") |>
    summarize(meanmass = mean(weight), sdmass = sd(weight), .by = variety) |>
    mutate(variety = str_to_title(variety),
           year = 2020)

tomato2021 <- harvest_2021 |>
    filter(vegetable == "tomatoes") |>
    summarize(meanmass = mean(weight), sdmass = sd(weight), .by = variety) |>
    mutate(variety = gsub("volunteer", "volunteers", variety),
           variety = str_to_title(variety),
           year = 2021)

tomato <- rbind(tomato2020, tomato2021)

ggplot(data = tomato) +
    geom_circle(aes(x0 = variety, y0 = 0, r = meanmass / 2), color = "firebrick", fill = alpha("#e24d4d", 0.8)) +
    facet_wrap(year ~ ., nrow = 2) +
    coord_fixed()
