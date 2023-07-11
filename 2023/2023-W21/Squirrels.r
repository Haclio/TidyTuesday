install.packages("janitor")
install.packages("treemapify")
install.packages("ggh4x")
library(tidyverse)
library(openxlsx)
library(treemapify) #To make the treemap
library(janitor) #Cleaning names
library(ggh4x) #To switch facet strip position
setwd("./2023/2023-W21")

# tuesdata <- tidytuesdayR::tt_load(2023, week = 21)
# squirrel_data <- tuesdata$squirrel_data

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') |>
    clean_names()

squir_sum <- squirrel_data |>
  drop_na(primary_fur_color) |> #Drop the NAs
  reframe(nb = n(), .by  = c("primary_fur_color", "highlight_fur_color")) |> # = summarize()
  replace_na(list(highlight_fur_color = "None")) |> #Replace NAs by "None"
  mutate(cols = case_when(highlight_fur_color == "Black" ~ "black", #Hard-coding color values for the plot, there must be a better way to do this
                          highlight_fur_color == "Gray" ~ "grey40",
                          highlight_fur_color == "Cinnamon" ~ "#ca6e04",
                          highlight_fur_color == "White" ~ "white",
                          highlight_fur_color == "Gray, White" ~ "grey50",
                          highlight_fur_color == "Gray, Black" ~ "grey20",
                          highlight_fur_color == "Black, White" ~ "grey30",
                          highlight_fur_color == "Cinnamon, White" ~ "#ec9634",
                          highlight_fur_color == "Black, Cinnamon" ~ "#8f4d02",
                          highlight_fur_color == "Black, Cinnamon, White" ~ "#ca6e04",
                          highlight_fur_color == "None" & primary_fur_color == "Black" ~ "black",
                          highlight_fur_color == "None" & primary_fur_color == "Gray" ~ "grey40",
                          highlight_fur_color == "None" & primary_fur_color == "Cinnamon" ~ "#ca6e04"))

ggplot(squir_sum) +
geom_treemap(aes(area = nb, fill = I(cols)), show.legend = FALSE) + #I() necessary here to take the columns values as colors
geom_treemap_text(aes(area = nb, label = highlight_fur_color), place = "center", color = ifelse(squir_sum$highlight_fur_color == "White", "black", "white"), reflow = TRUE) + #reflow for wrapping text
facet_grid2(primary_fur_color ~ ., switch = "both", strip = strip_themed(background_y = list(element_rect(fill = "black"), element_rect(fill = "#ca6e04"), element_rect(fill = "grey40")))) + #switch = facet strip position
xlab("Highlights fur color") +
ylab("Primary fur color") +
theme(axis.title = element_text(size = 30), strip.text.y = element_text(size = 20, color = "white"), text = element_text(family = "Verdana"), panel.border = element_rect(color = "grey20", fill = NA), strip.background = element_rect(color = "grey20", fill = NA))

write.xlsx(squirrel_data, "Squirrels.xlsx")
write.xlsx(squir_sum, "Squirrels_summ.xlsx")
