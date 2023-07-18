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
    mutate(pct = round(n/sum(n)*100)) |>
    arrange(.pred_class, native)

det2 <- waffle_iron(detectors, rows = 60, aes_d(group = kind), sample_size = 0.5)

gg2 <- ggplot(det_sum) +
geom_waffle(aes(values = pct, fill = native), na.rm = FALSE, flip = TRUE, radius = unit(15, "pt"), size = 0) +
facet_grid(. ~ .pred_class, scales = "free") +
theme_classic() +
theme(aspect.ratio = 1) +
labs(title = "The Earth is on <span style = 'color: darkorange1;'>**fire**</span>", subtitle = "Temperature deviations from the 1951-1980 monthly average", caption = cap) +
theme(plot.background = element_rect(fill = "#25251f"), panel.background = element_rect(fill = "#25251f"),
axis.text.x = element_text(size = 16, family = "vd", color = "grey50"), axis.title.x = element_blank(),
axis.text.y = element_text(size = 16, family = "vd", color = "grey50"), 
axis.title.y = element_text(size = 20, family = "vd", margin = margin(0, 10, 0, 0)),
text = element_text(color = "white", family = "vd"), panel.grid.major = element_line(linewidth = 0.1),
panel.grid.minor = element_blank(), legend.background = element_rect(fill = "#25251f"),
legend.position = "bottom",plot.margin = margin(1, 1, 1, 1.2, "cm"), legend.key.width = unit(1.5, "cm"), 
legend.text = element_text(size = 12), legend.title = element_text(size = 16),
plot.caption = element_markdown(color = "white", margin = margin(t = 30), size = 8, hjust = 1.1),
plot.title = element_markdown(size = 30, family = "vd", margin = margin(t = 20, b = 10)),
plot.subtitle = element_text(size = 16, family = "vd", margin = margin(t = 10, b = 20)))
gg2

ggplot(det_sum) +
geom_col(aes(x = .pred_class, y = pct, fill = native))
#333324