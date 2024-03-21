setwd("./2024/2024-W12")
library(tidyverse)
library(ggtext)
library(ggh4x)
library(ggrepel)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp",
         regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: Anderson Evans / Rally |  #TidyTuesday week 12 2024"
sub <- "Since their creation in 1963 to this day, X-men, the mutant superheroes, are one of the most successful franchises of<br>
        Marvel Comics. These comics have become collector items and as such, are prone to speculation. Much like the baseball-<br>
        themed book (and later movie), Michael Lewis' <i>Moneyball</i>, the aim of this dataset was to 'gamify open and available<br>
        financial data' to get insights on what each mutant is worth. Older comics are always more sought after, but in a given<br>
        decade, it might not be the most popular characters who are worth more, but the rarest... yet not always."

#Data wrangling
mutant_moneyball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')
xmen <- mutant_moneyball |>
    select(c(Member, ends_with("Percent"), starts_with("PPI") & ends_with("Street"))) |>
    rename_with(~gsub("PPI", "", .x)) |>
    rename_with(~gsub("_Percent", "", .x)) |>
    rename_with(~gsub("oStreet", "Value", .x)) |>
    pivot_longer(-Member, names_to = c("Decade", ".value"), names_pattern = "(.0s)_(...)") |>
    mutate(Val = gsub("\\$|,", "", Val),
           App = gsub("\\%", "", App)) |>
    mutate(across(c(App, Val), ~as.numeric(.x))) |>
    mutate(Decade = case_when(Decade == "60s" ~ "1963 - 1969",
                              Decade == "70s" ~ "1970 - 1979",
                              Decade == "80s" ~ "1980 - 1989",
                              Decade == "90s" ~ "1990 - 1992")) |>
    mutate(label = case_when(Member == "ericMagnus" & Decade == "1963 - 1969" ~ "Magneto",
                             Member == "charlesXavier" & Decade == "1963 - 1969" ~ "Professor X",
                             Member == "warrenWorthington" & Decade == "1963 - 1969" ~ "Angel",
                             Member == "hankMcCoy" & Decade == "1963 - 1969" ~ "Beast",
                             Member == "bobbyDrake" & Decade == "1963 - 1969" ~ "Iceman",
                             Member == "scottSummers" & Decade == "1963 - 1969" ~ "Cyclops",
                             Member == "jeanGrey" & Decade == "1963 - 1969" ~ "Phoenix",
                             Member == "johnProudstar" & Decade == "1970 - 1979" ~ "Thunderbird",
                             Member == "shiroYoshida" & Decade == "1970 - 1979" ~ "Sunfire",
                             Member == "rachelSummers" & Decade == "1990 - 1992" ~ "Askani",
                             TRUE ~ NA)) |>
    mutate(color = case_when(Member == "ericMagnus" ~ "#a30202",
                             Member == "charlesXavier" ~ "black",
                             Member == "warrenWorthington" ~ "springgreen4",
                             Member == "hankMcCoy" ~ "#4e4ed8",
                             Member == "bobbyDrake" ~ "#8accd4",
                             Member == "scottSummers" ~ "red",
                             Member == "jeanGrey" ~ "#e7ce3c",
                             Member == "johnProudstar" ~ "#0abe04",
                             Member == "shiroYoshida" ~ "#da6b7e",
                             Member == "rachelSummers" ~ "#e0992f",
                             TRUE ~ "grey70")) |>
    filter(!App == 0)

df_scales  <- data.frame(Panel = c("1963 - 1969", "1970 - 1979", "1980 - 1989", "1990 - 1992"),
                         ymin = 0,
                         ymax = c(4000, 1000, 60, 30),
                         n = c(4, 5, 4, 4))
df_scales <- split(df_scales, df_scales$Panel)
scales <- lapply(df_scales, function(x) {
  scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n, expand = c(0, 0))
})

gg <- ggplot(xmen) +
    geom_point(aes(x = App, y = Val, color = color)) +
    geom_text_repel(aes(x = App, y = Val, label = label, color = color), force = 20, force_pull = 0.01, seed = 1, verbose = TRUE, xlim = c(NA, 104), nudge_y = ifelse(xmen$label %in% c("Iceman", "Phoenix"), 800, ifelse(xmen$label %in% c("Beast", "Cyclops"), -800, 0))) +
    facet_wrap(Decade ~ ., scales = "free_y", nrow = 4, strip.position = "right") +
    facetted_pos_scales(y = scales) +
    xlab("Proportion of Character Appearance (%)") +
    ylab("Average price per issue ($)") +
    labs(title = "The true (financial) worth of the X-men", subtitle = sub, caption = cap) +
    # expand_limits(y = 0) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    scale_color_identity() +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(axis.text.x = element_text(margin = margin(t = 5)),
          axis.text.y = element_text(margin = margin(r = 5), hjust = 1),
          axis.title.x = element_text(size = 20, margin = margin(t = 20)),
          axis.title.y = element_text(size = 20, angle = 90, margin = margin(r = 20)),
          panel.grid.major = element_line(linewidth = 0.5, color = alpha("grey80", 0.5)),
          panel.background = element_rect(fill = NA, color = NA), #Background color
          panel.spacing.y = unit(2.5, "lines"),
          plot.background = element_rect(fill = NA, color = NA),
          plot.caption = element_markdown(margin = margin(15, -35, -10, 0), size = 8.5, color = "grey20", hjust = 1),
          plot.margin = unit(c(1, 1, 0.5, 1), "cm"), #Margins
          plot.subtitle = element_markdown(size = 9.5, color = "grey20", hjust = 0.5, margin = margin(t = 20, b = 20), lineheight = 1.2),
          plot.title = element_markdown(size = 28, color = "grey20", hjust = 0.5, lineheight = 1.1),
          plot.title.position = "plot",
          strip.placement = "outside",
          strip.text = element_text(size = 16, margin = margin(l = 15), angle = -90),
          text = element_text(family = "ssp"))
# gg

agg_png("xmen.png", width = 7.5, height = 10, units = "in", res = 300)
gg
dev.off()
