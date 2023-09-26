setwd("./2023/2023-W39")
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

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: D. Menghani's {richmondway} package | #TidyTuesday week 39"
sub <- "Ted Lasso is a TV show that 'follows Ted Lasso, an American college football coach who is hired to coach an English soccer team<br>with the secret intention that his inexperience will lead it to failure, but whose folksy, optimistic leadership proves unexpectedly successful'.<br><br>Roy Kent is one of the main characters, and a man of few words, but many of them are f-ck : mad f-cks, sad f-cks, happy f-cks.<br>Below are shown counts of his f-cks per episode (in solid colors), compared to all characters' f-cks combined."

#Data wrangling
richmondway <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')
labs <- data.frame(Episode_order = c(7, 18, 29), y = c(28, 32, 40), lab = c("<span style = 'color: #4169E1;'>Season 1</span>", "<span style = 'color: #5D478B;'>Season 2</span>", "<span style = 'color: darkorange3;'>Season 3</span>"))

ggplot(richmondway, aes(x = Episode_order)) +
    annotate('text', x = 34.5, y = c(12, 22, 32, 42, 52), label = c('10', '20', '30', '40', '50'), color = "grey70") + #Y-axis values
    geom_hline(yintercept = seq(0, 50, by = 10), colour = "grey70", linewidth = 0.3) + #Y-axis grid lines
    geom_col(aes(y = F_count_total, fill = as.factor(Season)), alpha = 0.4, show.legend = FALSE) + #Everyone's f-cks
    geom_col(aes(y = F_count_RK, fill = as.factor(Season)), show.legend = FALSE) + #Roy's f-cks
    geom_richtext(data = labs, aes(x = Episode_order, y = y, label = lab), fill = NA, label.color = NA, size = 10, family = "ssp") + #Seasons text
    scale_fill_manual(values = c("#4169E1", "#5D478B", "darkorange3")) + #Colors
    scale_y_continuous(limits = c(0, 52), breaks = seq(0, 50, 10)) + #Y-axis range
    coord_polar() +
    theme_void() +
    labs(title = "Roy Kent : a man of his (F-)word", subtitle = sub, caption = cap) + #Title, subtitle, caption
    theme(panel.grid = element_blank(), #Radar lines
          panel.background = element_rect(fill = NA, color = NA), #No panel background color = same as plot background
          plot.background = element_rect(fill = NA, color = NA), #Sets the whole plot's background color
          plot.caption = element_markdown(margin = margin(-20, 0, 5, 0), family = "ssp", size = 12, halign = 0.5, hjust = 0.5), #Caption settings
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 15, lineheight=1.1, margin = margin(0, 0, -20, 0)), #Subtitle settings
          plot.title = element_text(size = 50, family = "ssp", margin = margin(t = 40, b = 25), hjust = 0.5), #Title settings
          plot.title.position = "plot", #Aligns the title and subtitle to the edge of the plot, not the panel, so that hjust = 0.5 makes them centered
          text = element_text(family = "ssp")) #Axis labels font


