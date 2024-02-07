setwd("./2024/2024-W06")
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
font_add(family = "sspb", regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Black.ttf", sep = "/"))

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin  |  Bluesky : @louisnadalin.bsky.social  |  Source: 1 dataset, 100 visualizations project |  #TidyTuesday week 06 2024"
sub <- "The <span style='font-family:sspb; color: #315FD2'>1 dataset, 100 visualizations project</span> aimed to demonstrate how a very simple dataset
        can be plotted in a myriad of styles, offering to visualize a tiny 3x3 dataset in a hundred different ways.<br><br><br><br>
        The dataset used here comes from UNESCO, and sums up the number of World Heritage Sites in Norway, Denmark and Sweden, showing how it evolved between the years<br>
        <span style='font-family:sspb; color: white'>2004</span> and <span style='font-family:sspb; color: #315FD2'>2022</span>."

#Data wrangling
heritage <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv") |>
  mutate(country = factor(country, levels = c("Norway", "Denmark", "Sweden")))

gg1 <- ggplot(data = heritage) +
  labs(title = "1 dataset,<br>1<span style='color: #6a9bc1;'>00</span> visualization<span style='color: #6a9bc1;'>s</span>", subtitle = sub) +
  theme_void() +
  theme(plot.subtitle = element_textbox(margin = margin(t = 100, b = -100), family = "ssp", hjust = -0.25, halign = 0.5, vjust = -2, size = 25, lineheight = 1.2, color = "#161246", face = "bold", width = 1.5), #Subtitle settings
        plot.title = element_markdown(size = 70, family = "ssp", margin = margin(t = 0, b = -25), hjust = -0.25, color = "#161246", face = "bold", halign = 0.5), #Title settings
        plot.title.position = "plot",
        plot.margin = unit(c(1, 0, 0.5, 0.5), "cm"))

gg3 <- ggplot(data = heritage, aes(x = country)) +
       geom_col(aes(y = `2022`), fill = "#315FD2", color = NA, width = 0.7) +
       geom_col(aes(y = `2004`), fill = "white", color = NA, width = 0.5) +
       geom_text(aes(y = `2022` + 0.6, label = `2022`), color = "#315FD2", size = 14, fontface = "bold") +
       geom_text(aes(y = `2004` + 0.6, label = `2004`), color = "white", size = 14, fontface = "bold") +
       scale_y_continuous(limits = c(0, 16), expand = c(0, 0.1)) + #Continuous y axis
       labs(caption = cap) +
       theme_void() +
       theme(axis.text.x = element_text(colour = "#161246", size = 25, face = "bold"), #Axis labels formatting
             plot.margin = unit(c(2, 1, 0.5, 0), "cm"), #Margins
             text = element_text(family = "ssp"),
             plot.caption = element_markdown(margin = margin(10, -5, -5, 0), size = 14, color = "#161246"))

gg <- free(gg1) + gg3 & 
            plot_layout(widths = c(0.6, 0.4)) &
            plot_annotation(theme = theme(panel.background = element_rect(color = NA), #Background color
                                          plot.background = element_rect(fill = "#94dfff", color = NA))) #Caption formatting #Background color

agg_png("heritage.png", width = 13, height = 12, units = "in", res = 300)
gg
dev.off()
