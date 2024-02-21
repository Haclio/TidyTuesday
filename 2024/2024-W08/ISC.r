setwd("./2024/2024-W08")
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
font_add(family = "sspbi",
         regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-BoldItalic.ttf", sep = "/"))

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: R Consortium |  #TidyTuesday week 08 2024"
sub <- "The R Consortium Infrastructure Steering Committee (ISC) awards grants to members of the R community<br>
        every year since 2016. These grants aim to fund projects that can broadly enhance the R ecosystem,<br>
        on both technical and social aspects alike. The plot below shows the grants awarded each year<br>
        (each rectangle represents a grant), and the amount funded..."

#Data wrangling
isc_grants <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv")
isc_sum <- isc_grants |> #Compute the cumulative sums for and coordinates for geom_segment
  mutate(funded = funded / 1000) |>
  group_by(year, group) |>
  mutate(cum_n = cumsum(funded)) |>
  mutate(ymin = dplyr::lag(cum_n, default = 0),
         ymax = cum_n) |>
  select(year, group, funded, cum_n, ymin, ymax)
isc_sum

transpseg <- isc_sum |> #Identify the local maxima to remove them from geom_segment
    summarize(max = max(cum_n))

isc_sum <- isc_sum |>  #Color column for geom_segment
    mutate(color = case_when(group == 1 & !(ymax %in% transpseg$max[!(transpseg$max == 35)]) ~ "goldenrod",
                             group == 1 & ymax %in% transpseg$max ~ NA,
                             group == 2 & !(ymax %in% transpseg$max) ~ "grey20",
                             group == 2 & ymax %in% transpseg$max ~ NA)) |>
    mutate(year = as.numeric(year))

gg <- ggplot(data = isc_sum) +
    geom_col(aes(x = as.numeric(year), y = ifelse(group == 1, -funded, funded), fill = as.factor(group)), position = "stack", show.legend = FALSE, width = 0.8) + #
    geom_segment(aes(x = year - 0.4, xend = year + 0.4, y = ifelse(group == 1, -ymax, ymax), yend = ifelse(group == 1, -ymax, ymax), color = color), linewidth = 0.5, show.legend = FALSE) +
    annotate(geom = "text", label = "...during the fall cycle...", x = 2016.5, y = 110, size = 4, color = "goldenrod3", family = "sspbi") +
    annotate(geom = "text", label = "...and the spring cycle.", x = 2022.5, y = -110, size = 4, color = "grey20", family = "sspbi") +
    labs(title = "Grant distribution history of the<br>R Consortium Infrastructure Steering Committee", subtitle = sub, caption = cap) +
    ylab("Funding (k€)") +
    scale_x_reverse(limits = c(2023.45, 2015.55), breaks = seq(2016, 2023, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-180, 180), breaks = seq(-150, 150, 50), labels = abs, expand = c(0, 0)) +
    scale_fill_manual(values = c("1" = "grey20", "2" = "goldenrod")) +
    scale_color_identity() + #Color taken from a column as text
    coord_flip() +
    theme_void() +
    theme(axis.text.x = element_text(colour = "grey20", size = 14, margin = margin(t = 10)),
          axis.text.y = element_text(colour = "grey20", size = 16),
          axis.title.x = element_text(colour = "grey20", size = 15, margin = margin(t = 10, b = 15)),
          panel.background = element_rect(fill = NA, color = NA), #Background color
          plot.background = element_rect(fill = alpha("goldenrod", 0.2), color = NA),
          plot.caption = element_markdown(margin = margin(10, -5, -5, 0), size = 9, color = "grey20"),
          panel.grid.major.x = element_line(linetype = "longdash", linewidth = 0.5, color = alpha("grey20", 0.15)),
          plot.margin = unit(c(1, 1, 0.5, 1), "cm"), #Margins
          plot.subtitle = element_markdown(size = 11, color = "grey20", hjust = 0.5, margin = margin(t = 20, b = 30), lineheight = 1.2),
          plot.title = element_markdown(size = 24, color = "grey20", hjust = 0.5, lineheight = 1.1),
          plot.title.position = "plot",
          text = element_text(family = "ssp"))
gg

agg_png("isc.png", width = 8, height = 8, units = "in", res = 300)
gg
dev.off()
