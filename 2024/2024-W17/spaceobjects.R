setwd("./2024/2024-W17")
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp",
         regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: Our World in Data |  #TidyTuesday week 17 2024"

#Data wrangling
outer_space_objects <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv")

spaceobjects <- outer_space_objects |>
    filter(Entity %in% european_countries) |>
    group_by(Entity) |>
    complete(Year = min(Year):2023) |>
    arrange(Year) |>
    mutate(cumsum = cumsum(coalesce(num_objects, 0))) |>
    # fill(cumsum) |>
    group_by(Year) |>
    mutate(rank = rank(-cumsum, ties.method = "first", na.last = TRUE)) |>
    group_by(Entity) |>
    filter(any(rank <= 10)) |>
    filter(Year >= 1993)

gg <- ggplot(spaceobjects) +
    geom_point(aes(x = Year, y = rank, color = Entity), size = 3) +
    geom_line(aes(x = Year, y = rank, color = Entity), linewidth = 7, alpha = 0.7, lineend = "round", show.legend = FALSE) +
    geom_point(aes(x = Year, y = rank), size = 0.8, color = "white", show.legend = TRUE) +
    geom_text(data = spaceobjects |> filter(Year == 2023 & rank <= 10), aes(x = 2024, y = rank, label = Entity, color = Entity), size = 5, fontface = "bold", hjust = 0, show.legend = FALSE) +
    ylab("Position") +
    labs(title = "<b>Ranking of European countries by total amount of objects launched<br>into outer space, since the 10th country's first launch in 1993.</b>", caption = cap) +
    scale_x_continuous(limits = c(1992.5, 2031), breaks = c(1993, seq(1995, 2020, 5), 2023), expand = c(0.01, 0), sec.axis = dup_axis()) +
    scale_y_continuous(limits = c(10, 1), breaks = seq(1:10), trans = "reverse", expand = c(0.03, 0)) +
    scale_color_manual(name = "", values = c(c25[c(1:7, 12, 16:17, 19, 22, 24)], "#d6a606", "grey50")) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
    theme_void() +
    theme(axis.line = element_blank(),
        axis.text.x = element_text(color = "grey20", size = 10, family = "ssp", margin = margin(t = 8, b = 8)),
        axis.text.y = element_text(color = "grey20", size = 20, family = "ssp", margin = margin(r = 8)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 24, angle = 90, margin = margin(r = 10)),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal",
        legend.key.spacing.y = unit(0, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 10, color = "grey20", family = "ssp", margin = margin(l = 0, r = 10)),
        plot.caption = element_markdown(margin = margin(15, 0, 0, 0), size = 8.5, color = "grey20", hjust = 1, family = "ssp"),
        plot.title = element_markdown(size = 20, color = "grey20", hjust = 0, lineheight = 1.1, family = "ssp", margin = margin(t = 25, b = 20, l = 15)),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#f1efff", color = NA),
        plot.margin = margin(r = 10, l = 10),
        text = element_text(family = "ssp", color = "grey20"))

agg_png("spaceobject.png", width = 10, height = 8, units = "in", res = 300)
gg
dev.off()
