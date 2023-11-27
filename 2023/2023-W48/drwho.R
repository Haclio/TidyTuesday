setwd("./2023/2023-W48")
library(tidyverse)
library(ggtext)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp", regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
                         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
                         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: MIT Election Data and Science Lab | #TidyTuesday week 45 2023"
sub <- "The United States House of Representatives is the lower chamber of the US Congress, the Senate being the upper chamber.<br>
        The purpose of both chambers is to pass federal legislation, better known as bills. The House also has dedicated powers,<br>
        such as originating revenue bills, or initiating impeachment proceedings.<br><br>
        Each state's plot below represents the affiliation of its representatives in each even-year election,<br>
        from 1976 to 2022, between <span style='color: #4fa7d3'>**Democrats**</span>, <span style='color: #ea5d56'>**Republicans**</span>, and <span style='color: #739627'>**other affiliations**</span>.<br>
        <span>*Note that the District of Columbia is shown here but its representative doesn't have any voting power in the House.*</span>"

#Data wrangling
drwho <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv') |>
    mutate(actor = case_when(season_number == 1 ~ "Christopher Eccleston",
                             season_number %in% 2:4 | story_number %in% c(167, 178, 188, 199:201, "202a", "202b") ~ "David Tennant",
                             season_number %in% 5:7 | story_number %in% 240:241 ~ "Matt Smith",
                             season_number %in% 8:10 ~ "Peter Capaldi",
                             season_number %in% 11:13 | story_number %in% 298:300 ~ "Jodie Whittaker")) |>
    mutate(actor = factor(actor, levels = c("Christopher Eccleston", "David Tennant", "Matt Smith", "Peter Capaldi", "Jodie Whittaker"))) |>
    mutate(season_number = ifelse(is.na(season_number), "Specials", season_number)) |>
    mutate(season_number = factor(season_number, levels = c(1:13, "Specials"))) |>
    filter(!is.na(episode_number))

ggplot(drwho) +
        geom_col(aes(x = episode_number, y = uk_viewers, color = actor), fill = "grey5", linewidth = 0.8, show.legend = FALSE) +
    facet_grid(. ~ actor + season_number, scales = "free_x", space = "free") +
    labs(title = "How many Whovians watched each Doctor Who ?", sub = sub, cap = cap) +
    scale_color_manual(values = c("Christopher Eccleston" = "grey60", "David Tennant" = "#bb5e1c", "Matt Smith" = "red3", "Peter Capaldi" = "royalblue2", "Jodie Whittaker" = "goldenrod3")) +
    theme_classic() +
    theme(axis.line = element_blank(), #No y axis line
          axis.text.x = element_blank(), #Axis labels formatting
          axis.ticks.x = element_blank(), #No ticks
        #   axis.title = element_blank(), #No axis title
          panel.background = element_rect(fill = "grey5", color = NA),
          plot.background = element_rect(fill = "grey5", color = NA), #Background color
          plot.caption = element_markdown(margin = margin(10, 0, -5, 0), size = 12, color = "grey90"), #Caption formatting
          plot.margin = unit(c(1,1,0.5,0.5),"cm"), #Margins
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 20, lineheight = 1.2, color = "grey90"), #Subtitle settings
          plot.title = element_markdown(size = 90, family = "gloom", margin = margin(b = 25), hjust = 0.5, color = "grey90"), #Title settings
          plot.title.position = "plot",
          text = element_text(family = "ssp")) #General font

ggsave(filename = "drwho.png", width = 3600, height = 1800, units = "px")
