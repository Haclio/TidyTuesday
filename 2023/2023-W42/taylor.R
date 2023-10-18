setwd("./2023/2023-W42")
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
font_add(family = "IM", regular = paste(fontsfolder, "IM_Fell_DW_Pica", "IMFellDWPica-Regular.ttf", sep = "/")) #Title font

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: W. Jake Thompson's {taylor} package | #TidyTuesday week 42 2023"
sub <- "Spotify's algorithm attributes scores to songs based on different metrics:<br>
        <span style = 'color: #009ACD;'>**acousticness**</span>, <span style = 'color: #9A32CD;'>**danceability**</span>,
        <span style = 'color: #CD2626;'>**energy**</span>, and <span style = 'color: #EEAD0E;'>**valence**</span>.<br>
        The plot below shows such metrics for Taylor Swift's albums."

#Data wrangling
taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
taylor <- taylor_album_songs |>
        filter(!is.na(artist)) |>
        group_by(album_name) |>
        summarize(across(c(danceability, energy, acousticness, valence), mean)) |>
        pivot_longer(cols = !album_name, names_to = "metric", values_to = "value") |>
        mutate(album_name = fct_relevel(album_name, c("Taylor Swift", "Speak Now", "1989", "reputation", "Lover",
                                                      "folklore", "evermore", "Fearless (Taylor's Version)",
                                                      "Red (Taylor's Version)", "Midnights")))

ann_text <- data.frame(#Y-axis replacement scale
  y = c(30, 55, 80, 105),
  lab = c("25", "50", "75", "100")
)

labels <- c( #Array of pictures to use as facet labels
  "reputation" = "<img src='reputation.jpg' width = '120'>",
  "Lover" = "<img src='lover.jpg' width = '120'>",
  "folklore" = "<img src='folklore.jpg' width = '120'>",
  "evermore" = "<img src='evermore.jpg' width = '120'>",
  "Midnights" = "<img src='midnights.jpg' width = '120'>",
  "Taylor Swift" = "<img src='ts.jpg' width = '120'>",
  "Speak Now" = "<img src='speaknow.jpg' width = '120'>",
  "Fearless (Taylor's Version)" = "<img src='fearless.jpg' width = '120'>",
  "1989" = "<img src='1989.jpg' width = '120'>",
  "Red (Taylor's Version)" = "<img src='red.jpg' width = '120'>")

ggplot() +
        geom_hline(yintercept = seq(0, 100, by = 25), colour = "grey80", linewidth = 0.3) + #Y-axis grid lines
        geom_text(data = ann_text, aes(x = 2.5, y = y + 5, label = lab), color = "grey80", size = 2.2) + #Y-axis values
        geom_col(data = taylor, aes(x = metric, y = value * 100, fill = metric), width = 0.2, show.legend = FALSE) +
        geom_text(data = taylor |> filter(metric == "valence"), aes(x = metric, y = value * 100 + 25, label = paste(as.character(round(value * 100)), "%")), color = "#d49c0d", fontface = "bold", size = 4) +
        geom_text(data = taylor |> filter(metric == "acousticness"), aes(x = metric, y = value * 100 + 25, label = paste(as.character(round(value * 100)), "%")), color = "#009ACD", fontface = "bold", size = 4) +
        geom_text(data = taylor |> filter(metric == "energy"), aes(x = metric, y = value * 100 + 25, label = paste(as.character(round(value * 100)), "%")), color = "#CD2626", fontface = "bold", size = 4) +
        geom_text(data = taylor |> filter(metric == "danceability"), aes(x = metric, y = value * 100 + 25, label = paste(as.character(round(value * 100)), "%")), color = "#9A32CD", fontface = "bold", size = 4) +
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) + #Y-axis range
        scale_x_discrete() + #Necessary to avoid the "Discrete value supplied to continuous scale" error when using numerical coordinates on a discrete scale
        scale_fill_manual(values = c("valence" = "#EEAD0E", "acousticness" = "#009ACD", "energy" = "#CD2626", "danceability" = "#9A32CD")) +
        coord_polar() +
        facet_wrap(album_name ~ ., nrow = 2, labeller = as_labeller(labels)) + #labeller necessary to display images
        theme_void() +
        labs(title = "What does Spotify think of Taylor Swift's albums?", subtitle = sub, caption = cap) + #Title, subtitle, caption
        theme(panel.grid = element_blank(), #Radar lines
          panel.background = element_rect(fill = "grey98", color = NA), #No panel background color = same as plot background
          panel.spacing.y = unit(20, "pt"),
          plot.background = element_rect(fill = "grey98", color = NA), #Sets the whole plot background color
          plot.caption = element_markdown(margin = margin(20, 0, 5, 0), family = "ssp", size = 10, halign = 0.5, hjust = 0.95), #Caption settings
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 15, lineheight = 1.1, margin = margin(0, 0, 25, 0)), #Subtitle settings
          plot.title = element_text(size = 30, family = "IM", margin = margin(t = 15, b = 10), hjust = 0.5), #Title settings
          plot.title.position = "plot", #Aligns the title and subtitle to the edge of the plot, not the panel, so that hjust = 0.5 makes them centered
          strip.text.x = element_markdown(), #Necessary to display images as strip text
          text = element_text(family = "ssp"))
