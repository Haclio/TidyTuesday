setwd("./2023/2023-W40")
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

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: Grants.gov website | #TidyTuesday week 40 2023"
sub <- "Ted Lasso is a TV show that 'follows Ted Lasso, an American college football coach who is hired to coach an English soccer team<br>with the secret intention that his inexperience will lead it to failure, but whose folksy, optimistic leadership proves unexpectedly successful'.<br><br>Roy Kent is one of the main characters, and a man of few words, but many of them are f-ck : mad f-cks, sad f-cks, happy f-cks.<br>Below are shown counts of his f-cks per episode (in solid colors), compared to all characters' f-cks combined."

#Data wrangling
# grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')
grant_opportunity_details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')
sum <- grant_opportunity_details |>
    group_by(opportunity_category) |>
    reframe(across(starts_with("category"), ~sum(. == "TRUE"))) |>
    select(-last_col()) |>
    pivot_longer(!opportunity_category, names_to = "category", values_to = "number") |>
    mutate(category = gsub("([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2", category, perl=TRUE)) |>
    mutate(category = gsub("Category_", "", category)) |>
    mutate(category = gsub("_", " ", category))

ggplot(sum |> filter(number > 0)) +
    geom_col(aes(x = number, y = as.factor(category), fill = opportunity_category), show.legend = FALSE, width = 0.3) +
    facet_grid(. ~ opportunity_category) +
    coord_trans(x = "pseudo_log") +
    scale_x_continuous(limits = c(0, 1100), breaks = c(0, 1, 10, 100, 1000)) +
    scale_y_discrete(limits = rev) +
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())
