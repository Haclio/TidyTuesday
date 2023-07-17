setwd("./2023/2023-W29")
library(tidyverse)
library(showtext)
library(ggtext)
remotes::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
font_add(family = "fb", regular = paste(dirname(dirname(getwd())), "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos
font_add(family = "vd", regular = "C:/Windows/Fonts/verdana.ttf", bold = "C:/Windows/Fonts/verdanab.ttf") #Verdana
cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Simon Counch's {detectors} R package | #TidyTuesday week 29"

detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')
det_sum <- detectors |>
    summarize(n = n(), .by = c("kind", ".pred_class"))

det2 <- waffle_iron(det_sum, aes_d(group = kind))

gg <- ggplot(det_sum) +
geom_waffle(aes(x = kind, y = n, fill = kind)) +
theme_waffle()
gg
