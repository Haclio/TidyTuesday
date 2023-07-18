setwd("./2023/2023-W29")
library(tidyverse)
library(showtext)
library(ggtext)
remotes::install_github("liamgilbey/ggwaffle")
library(ggwaffle)
install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
font_add(family = "fb", regular = paste(dirname(dirname(getwd())), "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in parent (x2) directory
font_add(family = "vd", regular = "C:/Windows/Fonts/verdana.ttf", bold = "C:/Windows/Fonts/verdanab.ttf") #Verdana
cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Simon Counch's {detectors} R package | #TidyTuesday week 29"

detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')
det_sum <- detectors |>
    summarize(n = n(), .by = c(".pred_class", "native")) |>
    filter(!is.na(native)) |>
    group_by(.pred_class) |>
    mutate(pct = n/sum(n))
    summarize(.pred_class = .pred_class, native = native, pct = n/sum(n), .by = c("kind"))

det2 <- waffle_iron(detectors, rows = 60, aes_d(group = kind, native = native), sample_size = 0.5)

gg <- ggplot(det2) +
geom_waffle(aes(x = x, y = y, color = group), tile_shape = "circle", size = 1) +
coord_equal()
gg

gg2 <- ggplot(det_sum) +
geom_waffle(aes(values = bin, fill = native), na.rm = FALSE) +
facet_grid(. ~ .pred_class, scales = "free") +
theme(aspect.ratio = 1)
gg2

ggplot(det_sum) +
geom_col(aes(x = .pred_class, y = pct, fill = native))
