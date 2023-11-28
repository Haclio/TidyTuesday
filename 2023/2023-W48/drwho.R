setwd("./2023/2023-W48")
library(tidyverse)
library(ggtext)
library(showtext)
library(ggh4x) #For nested facets and strips customization
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp", regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
                         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
                         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro
font_add(family = "futb", regular = paste(fontsfolder, "Futura_Book", "Futura Book.otf", sep = "/")) #Futura Book

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: Jonathan Kitt's {datardis} package  |  #TidyTuesday week 48 2023"
sub <- "Doctor Who is, according to *Guinness World Records*, the 'longest-running science-fiction television series in the world',<br>
       from its debut in 1963 to this day 60 years later, with a break from 1989 to 2005. Each Doctor's regeneration is portrayed by a different actor,<br>
       with five iterations so far since the 2005 revival.<br><br>
       The plot below represents each Doctor's audience throughout the seasons, relative to that season's average.<br>
       Special episodes are marked under 'S'. Episodes not tied to a season have been excluded for clarity."

#Data wrangling
drwho <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv") |>
    mutate(actor = case_when(season_number == 1 ~ "Christopher Eccleston", #Adding actor names
                             season_number %in% 2:4 | story_number %in% c(167, 178, 188, 199:201, "202a", "202b") ~ "David Tennant",
                             season_number %in% 5:7 | story_number %in% 240:241 ~ "Matt Smith",
                             season_number %in% 8:10 ~ "Peter Capaldi",
                             season_number %in% 11:13 | story_number %in% 298:300 ~ "Jodie Whittaker")) |>
    mutate(actor = factor(actor, levels = c("Christopher Eccleston", "David Tennant", "Matt Smith", "Peter Capaldi", "Jodie Whittaker"))) |> #Ordering factors
    mutate(season_number = ifelse(is.na(season_number), "S", season_number)) |> #Special case for special episodes
    mutate(season_number = factor(season_number, levels = c(1:13, "S"))) |> #Ordering factors
    filter(!is.na(episode_number)) |> #Remove out-of-season episodes
    group_by(actor, season_number) |>
    mutate(season_avg = mean(uk_viewers), #Calculate mean viewers, and total number of episodes in the season to draw the average
           min = 1,
           max = n())

test1 <- strip_nested(text_x = elem_list_text(colour = c("grey60", "#bb5e1c", "red3", "royalblue2", "goldenrod3", "grey60", rep("#bb5e1c", 4), rep("red3", 4), rep("royalblue2", 3), rep("goldenrod3", 4)))) #Facet strip customization

gg <- ggplot(drwho) +
    geom_segment(aes(x = episode_number, xend = episode_number, y = season_avg, yend = uk_viewers, color = actor, group = season_number), stat = "summary", show.legend = FALSE, lineend = "round") + #Vertical segments
    geom_segment(aes(x = min, xend = max, y = season_avg, yend = season_avg, group = actor, color = actor), show.legend = FALSE, lineend = "round") + #Horizontal segments
    facet_nested(. ~ actor + season_number, scales = "free_x", space = "free", nest_line = element_line(linetype = 1), solo_line = TRUE, labeller = label_wrap_gen(width = 21), strip = test1) + #Nested facets, with {ggh4x}
    ylab("UK viewers (millions)") + #Axis title
    labs(title = "How many Whovians for each Doctor Who ?", subtitle = sub, caption = cap) + #Title, subtitle, caption
    scale_x_discrete(expand = c(0, 1)) + #Discrete x axis with some padding added on each side
    scale_y_continuous(limits = c(0, 13.5), breaks = seq(0, 12.5, 2.5), expand = c(0, 0)) + #Continuous y axis
    scale_color_manual(values = c("Christopher Eccleston" = "grey60", "David Tennant" = "#bb5e1c", "Matt Smith" = "red3", "Peter Capaldi" = "royalblue2", "Jodie Whittaker" = "goldenrod3")) + #Segment colors
    coord_cartesian(clip = "off") + #Text clipping off
    theme_classic() +
    theme(axis.line = element_blank(), #No y axis line
          axis.text.x = element_blank(), #Axis labels formatting
          axis.text.y =  element_text(colour = "grey80", size = 10),
          axis.ticks.x = element_blank(), #No ticks
          axis.ticks.y = element_line(color = "grey80"),
          axis.title.x = element_blank(), #No axis title
          axis.title.y = element_text(color = "grey80", size = 12),
          ggh4x.facet.nestline = element_line(color = "grey80", linewidth = 1), #Nested facet line
          panel.background = element_rect(fill = "grey10", color = NA), #Background color
          plot.background = element_rect(fill = "grey10", color = NA), #Background color
          plot.caption = element_markdown(margin = margin(15, 0, -5, 0), size = 8, color = "grey90"), #Caption formatting
          panel.grid.major = element_blank(),
          plot.margin = unit(c(1,1,0.5,0.5), "cm"), #Margins
          plot.subtitle = element_markdown(margin = margin(b = 10), family = "ssp", hjust = 0.5, halign = 0.5, size = 12, lineheight = 1.2, color = "grey90"), #Subtitle settings
          plot.title = element_markdown(size = 30, family = "futb", margin = margin(b = 25), hjust = 0.5, color = "grey90", face = "bold"), #Title settings
          plot.title.position = "plot",
          strip.background = element_blank(),
          strip.clip = "off",
          strip.text.x = element_text(color = "white", size = 10, face = "bold"),
          text = element_text(family = "ssp")) #General font

agg_png("drwho.png", width = 12, height = 7, units = "in", res = 300)
gg
dev.off()
