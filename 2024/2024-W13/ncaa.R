setwd("./2024/2024-W13")
library(tidyverse)
library(ggtext)
library(showtext)
library(ggh4x)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp",
         regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: Nishaan Amin |  #TidyTuesday week 13 2024"
sub <- "Does past performance inform "

#Data wrangling
team_results <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/team-results.csv") |>
    select(TEAM, WINPERCENT)

public_picks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/public-picks.csv")
picks <- public_picks |>
    mutate(across(4:9, ~gsub("%", "", .x))) |>
    rename(c("Round 1" = "R64", "Round 2" = "R32", "Round 3" = "S16", "Quarter finals" = "E8", "Semi-finals" = "F4", Finals = "FINALS")) |>
    select(-TEAMNO) |>
    pivot_longer(-c(YEAR, TEAM), names_to = "Round", values_to = "Percent") |>
    mutate(Round = factor(Round, levels = c("Round 1", "Round 2", "Round 3", "Quarter finals", "Semi-finals", "Finals")),
           Percent = as.numeric(Percent))
picks <- left_join(picks, team_results) |>
    drop_na()

gg1 <- ggplot(picks, aes(x = Round, y = Percent, group = TEAM)) +
    geom_line(aes(color = WINPERCENT * 100), alpha = 0.8) +
    annotate(geom = "rect", xmin = 6, xmax = 6.5, ymin = 0, ymax = 100, fill = "grey20", color = "grey20") +
    annotate(geom = "segment", x = 6.05, xend = 6.5, y = 34.92, yend = 100, color = "grey80", linetype = "dashed") +
    annotate(geom = "segment", x = 6.05, xend = 6.5, y = 0, yend = 0, color = "grey80", linetype = "dashed") +
    ylab("Winning chance as predicted by the public (%)") +
    labs(title = "NCAA Men's March Madness public predictions", subtitle = sub, caption = cap) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
    scale_color_viridis_c(option = "inferno", name = "Total winning %\nof the team") +
    coord_cartesian(clip = "off") +
    theme(axis.line = element_blank(),
          axis.text.x = element_text(color = "grey80", size = 12, family = "ssp", margin = margin(t = 8)),
          axis.text.y = element_text(color = "grey80", size = 12, family = "ssp"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.background = element_rect(fill = NA),
          legend.position = c(0.82, 0.88),
          panel.grid.major.x = element_line(linetype = "longdash", linewidth = 0.2, color = alpha("grey80", 0.2)),
          panel.grid.major = element_line(color = alpha("grey80", 0.2)),
          panel.grid.minor = element_blank(),
          text = element_text(family = "ssp", color = "grey80"))

gg2 <- ggplot(picks |> filter(Round == "Finals"), aes(x = 1, y = reorder(TEAM, Percent), fill = WINPERCENT)) +
    geom_tile(color = NA, show.legend = FALSE) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_viridis_c(option = "inferno") +
    coord_cartesian(clip = "off") +
    theme_void()

gg <- gg1 + gg2 + plot_layout(widths = c(1, 0.05, 0.05)) &
    theme(panel.background = element_rect(fill = NA, color = NA),
          plot.background = element_rect(fill = "grey20", color = "grey20"))
gg

agg_png("ncaa.png", width = 8, height = 9, units = "in", res = 300)
gg
dev.off()