setwd("./2024/2024-W18")
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

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: World Bank |  #TidyTuesday week 18 2024"
sub <- "<i>Average value for each group is displayed with a black line. Only the latest data for each country is shown.<br> &nbsp; Total number of countries: 111. Low/high income refers to the country's income bracket as per the World Bank.</i>"

#Data wrangling
wwbi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_data.csv')
wwbi_series <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_series.csv')
wwbi_country <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-30/wwbi_country.csv')

data <- wwbi_data |>
    filter(str_detect(indicator_code, "BI.PWK.P..S.FE.Q..ZS")) |>
    slice_tail(n = 1, by = c(country_code, indicator_code))
data2 <- left_join(data, wwbi_country, by = "country_code")
data2 <- data2 |>
    select(short_name, indicator_code, value, income_group) |>
    drop_na(income_group) |>
    mutate(income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")),
           sector = ifelse(grepl("PUB", indicator_code), "Public sector", "Private sector"),
           income_quantile = substr(indicator_code, 16, 17))

ggplot(data2, aes(x = income_quantile, y = value * 100)) +
    geom_point(aes(color = income_quantile), size = 0.9) +
    stat_summary(geom = "point", fun = "mean", col = "black", shape = "-", size = 10) +
    facet_nested(. ~ sector + income_group, scales = "free_x", space = "free", nest_line = element_line(linetype = 1), solo_line = TRUE, resect = unit(20, "pt"), labeller = label_wrap_gen(width = 15), strip = strip_nested(text_x = list(element_text(size = 14, face = "bold", family = "ssp", margin = margin(b = 5)), element_text(size = 8, family = "ssp", vjust = 1, margin = margin(t = 5))), by_layer_x = TRUE)) + #Nested facets, with {ggh4x}
    ylab("Female employment share (%)") +
    labs(title = "<b>Global female employment share by country income bracket and employment sector</b>", subtitle = sub, caption = cap) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_color_manual(name = "Individual income quantile", values = c("#334e00", "#586d02", "#838d01", "#b1ad00", "#e4cd00")) +
    theme_void() +
    theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey20", size = 20, family = "ssp", margin = margin(r = 8)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18, angle = 90, margin = margin(r = 10)),
        legend.background = element_rect(fill = NA, color = NA),
        legend.direction = "horizontal",
        legend.key.spacing.y = unit(0, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 10, color = "grey20", family = "ssp", margin = margin(l = -2, r = -5)),
        panel.grid.major.y = element_line(color = "grey70"),
        panel.spacing.x = unit(c(rep(20, 3), 30, rep(20, 3)), "pt"),
        plot.caption = element_markdown(margin = margin(15, 0, 2, 0), size = 8.5, color = "grey20", hjust = 1, family = "ssp"),
        plot.subtitle = element_markdown(margin = margin(b = 10, l = 15), family = "ssp", hjust = 0, halign = 0, size = 10, lineheight = 1.2, color = "#586d02"), #Subtitle settings
        plot.title = element_markdown(size = 15, color = "grey20", hjust = 0, lineheight = 1.1, family = "ssp", margin = margin(t = 25, b = 15, l = 15)),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "grey98", color = NA),
        plot.margin = margin(r = 10, l = 10),
        text = element_text(family = "ssp", color = "grey20"))

agg_png("indicators.png", width = 9, height = 6, units = "in", res = 300)
gg
dev.off()
