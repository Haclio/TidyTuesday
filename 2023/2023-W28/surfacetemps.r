setwd("./2023/2023-W28")
library(tidyverse)
library(showtext)
library(ggtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
font_add(family = "fb", regular = paste(dirname(dirname(getwd())), "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos
font_add(family = "vd", regular = "C:/Windows/Fonts/verdana.ttf", bold = "C:/Windows/Fonts/verdanab.ttf") #Verdana
cap <- paste0("<span style='font-family:fb;'>&#xf09b; </span> Haclio  |", 
              "<span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: NASA GISS Surface Temperature Analysis | #TidyTuesday week 28") #Caption

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") #as.Date won't work otherwise

global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv') 
temps <- global_temps |>
    select(!c("J-D", "D-N", "DJF", "MAM", "JJA", "SON")) |>
    pivot_longer(cols = !"Year", names_to = "Month", values_to = "Temp") |>
    mutate(Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) |>
    mutate(date = as.Date(paste("01", Month, Year), format = "%d %b %Y")) |>
    arrange(date)

break_vec <- c(seq(from = as.Date("01-01-1880"), to = as.Date("01-01-2024"), by = "20 years")) #Axis labels

gg <- ggplot(temps) +
geom_hline(aes(yintercept = 0), color = "yellow") +
geom_line(aes(x = date, y = Temp, color = Temp), linewidth = 0.6) +
ylab("Temperature deviation (°C)") +
scale_color_gradient2(name = "Temperature (°C)", high = "red", mid = "white", low = "darkblue") +
scale_x_date(breaks = "20 years", date_labels = "%Y", expand = c(0, 0)) +
labs(title = "The Earth is on <span style = 'color: darkorange1;'>**fire**</span>", subtitle = "Temperature deviations from the 1951-1980 monthly average", caption = cap) +
theme(plot.background = element_rect(fill = "grey10"), panel.background = element_rect(fill = "grey10"),
axis.text.x = element_text(size = 16, family = "vd", color = "grey50"), axis.title.x = element_blank(),
axis.text.y = element_text(size = 16, family = "vd", color = "grey50"), 
axis.title.y = element_text(size = 20, family = "vd", margin = margin(0, 10, 0, 0)),
text = element_text(color = "white", family = "vd"), panel.grid.major = element_line(linewidth = 0.1),
panel.grid.minor = element_blank(), legend.background = element_rect(fill = "grey10"),
legend.position = "bottom",plot.margin = margin(1, 1, 1, 1.2, "cm"), legend.key.width = unit(1.5, "cm"), 
legend.text = element_text(size = 12), legend.title = element_text(size = 16),
plot.caption = element_markdown(color = "white", margin = margin(t = 30), size = 8, hjust = 1.1),
plot.title = element_markdown(size = 30, family = "vd", margin = margin(t = 20, b = 10)),
plot.subtitle = element_text(size = 16, family = "vd", margin = margin(t = 10, b = 20)))
gg

