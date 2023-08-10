setwd("./2023/2023-W32")
library(tidyverse)
library(ggtext)
library(showtext)
library(DescTools) #For SortMixed()
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp", regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
                         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
                         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro               

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Hot Ones Wikipedia page | #TidyTuesday week 32"
sub <- "Hot Ones is an American talk show where celebrities are interviewed while eating<br>increasingly spicy chicken wings. Each season, ten hot sauces are used,<br>which are represented below on the Scoville scale, a measurement of spiciness.<br>Several peppers and their Scoville scale ratings are shown for reference."

#Data wrangling
sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv') |>
    mutate(season = paste("Season", season)) #Adding 'season' prefix
sauces <- sauces |>  
    mutate(season = factor(season, levels = unique(SortMixed(sauces$season)))) |> #factors = list of factors sorted by the numerical component in the string
    mutate(group = as.factor(rep(1:21, each = 10))) #For ordering

ggplot(sauces) +
geom_point(aes(y = reorder(group, desc(group)), x = scoville), color = "red", shape = 20, size = 5) + #Points layer
scale_x_continuous(trans = "log", limits = c(100, 2000000), breaks = c(100, 1000, 10000, 100000, 1000000, 2000000)) + #Log x scale with custom limits and breaks
scale_y_discrete(labels = c("Season 21", rep("", 19), "Season 1")) + #Y axis labels, reverse order because of the geom_point call
expand_limits(y = 25) + #Extend y limit to 25 to use coordinates for annotations
annotate("segment", x = 150, y = 22, xend = 100, yend = 22, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) + #Arrow
annotate("text", x = 125, y = 24.5, label = "Bellpepper\n0 u", size = 6, family = "ssp") + #Text annotation
annotate("segment", x = 2500, y = 22.5, xend = 2500, yend = 21.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
annotate("text", x = 2500, y = 24.5, label = "Jalapeño\n2500 u", size = 6, family = "ssp") +
annotate("segment", x = 25000, y = 22.5, xend = 25000, yend = 21.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
annotate("text", x = 25000, y = 24.5, label = "Cayenne\n25000 u", size = 6, family = "ssp") +
annotate("segment", x = 100000, y = 22.5, xend = 100000, yend = 21.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
annotate("text", x = 100000, y = 24.5, label = "Habanero\n100000 u", size = 6, family = "ssp") +
annotate("segment", x = 1500000, y = 22.5, xend = 1500000, yend = 21.5, arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
annotate("text", x = 1500000, y = 24.5, label = "Carolina Reaper\n1500000 u", size = 6, family = "ssp") +
labs(title = "Too <span style = 'color: red;'>**hot**</span> to handle ?", subtitle = sub, caption = cap) + #Title, subtitle, caption
xlab("Scoville heat units") + #X axis title
coord_cartesian(clip = "off") + #Turns off clipping so text isn't clipped if out of panel bounds
theme(axis.line.y = element_blank(), #No y axis line
      axis.text.x = element_text(size = 16, family = "ssp", face = "bold"), #Axis labels formatting
      axis.text.y = element_text(size = 12, family = "ssp", face = "bold"), #Axis labels formatting
      axis.ticks.y = element_blank(), #No y ticks
      axis.title.x = element_text(size = 20, margin = margin(t = 10)), #Axis title formatting
      axis.title.y = element_blank(), #No y axis title
      panel.grid.major.y = element_line(linewidth = 0.5, color = "black"), #Gridlines as scatterplot support
      panel.grid.major.x = element_blank(), #No x gridlines : they appear above y lines and cut them
      panel.background = element_blank(), #Panel background color = plot background color
      plot.background = element_rect(fill = "#FFF9F6"), #Background color
      plot.caption = element_markdown(margin = margin(30, 0, 0, 0), family = "ssp", size = 12), #Caption formatting
      plot.margin = unit(c(1,1,0.5,1),"cm"), #Margins
      plot.subtitle = element_textbox(family = "ssp", hjust = 0.5, halign = 0.5, size = 20, margin = margin(b = 35), lineheight = 1.2), #Subtitle settings
      plot.title = element_markdown(size = 50, family = "ssp", margin = margin(t = 20, b = 25), hjust = 0.5), #Title settings
      plot.title.position = "plot", #Aligns the title and subtitle to the edge of the plot, not the panel, so that hjust = 0.5 makes them centered,
      text = element_text(family = "ssp")) #Axes font