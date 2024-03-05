setwd("./2024/2024-W10")
library(tidyverse)
library(ggtext)
library(showtext)
# install.packages("treemapify")
library(treemapify)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp",
         regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: Baltimore Healthy Harbor initiative |  #TidyTuesday week 10 2024"
sub <- "According to his very own website, Mr. Trash Wheel is 'a semi-autonomous trash interceptor<br>
        that is placed at the end of a river, stream or other outfall'. Hydro- and solar-powered,<br>
        he and his family of four clean the waters of Baltimore by 'eating' the trash coming at them.<br>
        Below is a breakdown of the different kinds of trash each member of the family collects.<br>
        <span style='font-size: 8pt'><i>Percentages might not add up to 100 due to rounding.</i></span>"

#Data wrangling
trashwheel <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv") |>
    drop_na(Year)

trash <- trashwheel |>
    summarize(across(PlasticBottles:SportsBalls, ~sum(.x, na.rm = TRUE)), .by = "Name") |>
    mutate(total = rowSums(pick(where(is.numeric)))) |>
    mutate(across(PlasticBottles:SportsBalls, ~.x / total * 100)) |>
    pivot_longer(cols = -c(Name, total), names_to = "Type", values_to = "Proportion") |>
    mutate(Type = str_to_sentence(gsub("(\\B[A-Z])", " \\1", Type))) |>
    mutate(Name = case_when(Name == "Mister Trash Wheel" ~ "Mr. Trash Wheel",
                            Name == "Gwynnda Trash Wheel" ~ "Gwynnda the Good Wheel of the West",
                            .default =  as.character(Name))) |>
    mutate(Name = factor(Name, levels = c("Mr. Trash Wheel", "Professor Trash Wheel", "Captain Trash Wheel", "Gwynnda the Good Wheel of the West")))

gg <- ggplot(data = trash, aes(area = Proportion, fill = Type, subgroup = Name)) +
    geom_treemap(start = "topleft", color = NA, size = 0) +
    # geom_treemap_subgroup_border(color = "white") +
    geom_treemap_text(aes(label = format(round(Proportion, 1), nsmall = 1)), start = "topleft", place = "bottomright", fontface = "italic", size = 11, color = "grey20", family = "ssp") +
    # geom_treemap_text(aes(label = Type), start = "topleft") +
    facet_wrap(Name ~ ., labeller = labeller(Name = label_wrap_gen(25))) +
    labs(title = "The Baltimore Trash Wheel Family", subtitle = sub, caption = cap) +
    scale_fill_manual(values = c("#f4895f", "grey40", "#369acc", "#9656a2", "#de324c", "#f8e16f", "#95cf92")) +
    guides(fill = guide_legend(ncol = 7)) +
    coord_fixed() +
    theme_void() +
    theme(legend.key.size = unit(9, "pt"),
          legend.margin = margin(t = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 8, face = "bold"),
          legend.title = element_blank(),
          panel.background = element_rect(fill = NA, color = NA), #Background color
          panel.spacing.y = unit(1, "lines"),
          panel.spacing.x = unit(2, "lines"),
          plot.background = element_rect(fill = NA, color = NA),
          plot.caption = element_markdown(margin = margin(15, -70, -10, 0), size = 7, color = "grey20"),
          plot.margin = unit(c(1, 1, 0.5, 1), "cm"), #Margins
          plot.subtitle = element_markdown(size = 10, color = "grey20", hjust = 0.5, margin = margin(t = 20, b = 10), lineheight = 1.2),
          plot.title = element_markdown(size = 24, color = "grey20", hjust = 0.5, lineheight = 1.1),
          plot.title.position = "plot",
          strip.text = element_text(size = 13, margin = margin(b = 5), vjust = 0.1),
          text = element_text(family = "ssp"))

agg_png("trash.png", width = 7, height = 8, units = "in", res = 300)
gg
dev.off()