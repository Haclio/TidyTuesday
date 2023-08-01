setwd("./2023/2023-W31")
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

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Wikipedia | #TidyTuesday week 31"
sub <- "Dates on which a version of the current state name was <span style = 'color: #038001;'>first attested</span>, <br/>and when the state officially became <span style = 'color: #3caca8;'>part of the United States of America</span>."

#Data wrangling
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv') |> #Import data
    select(state, postal_abbreviation, admission) #Only keep useful columns

state_name_etymology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv') |> #Import data
    distinct(state, .keep_all=TRUE) |> #Only keep one row per state
    select(state, date_named) #Only keep useful columns

state <- merge(states, state_name_etymology) |> #Merge dataframes
    mutate(region = case_when( #Adding a 'region' column
                          postal_abbreviation== "CT" | postal_abbreviation== "ME" | postal_abbreviation== "MA" | postal_abbreviation== "NH"| postal_abbreviation== "RI"| postal_abbreviation== "VT" | postal_abbreviation== "NJ"| postal_abbreviation== "NY"| postal_abbreviation== "PA" ~ "Northeastern states",
                          postal_abbreviation== "IL" | postal_abbreviation== "IN" | postal_abbreviation== "MI" | postal_abbreviation== "OH"| postal_abbreviation== "WI"| postal_abbreviation== "IA" | postal_abbreviation== "KS" | postal_abbreviation== "MN" | postal_abbreviation== "MO" | postal_abbreviation== "NE" | postal_abbreviation== "ND"| postal_abbreviation== "SD" ~ "Midwestern states",
                          postal_abbreviation== "DE" | postal_abbreviation== "FL" | postal_abbreviation== "GA" | postal_abbreviation== "MD"| postal_abbreviation== "NC"| postal_abbreviation== "SC"| postal_abbreviation== "VA" | postal_abbreviation== "DC" | postal_abbreviation== "WV" | postal_abbreviation== "AL" | postal_abbreviation== "KY" | postal_abbreviation== "MS"| postal_abbreviation== "TN" | postal_abbreviation== "AR" | postal_abbreviation== "LA" | postal_abbreviation== "OK" | postal_abbreviation== "TX" ~ "Southern states",
                          postal_abbreviation== "WA" | postal_abbreviation== "AZ" | postal_abbreviation== "CO" | postal_abbreviation== "ID"| postal_abbreviation== "MT"| postal_abbreviation== "NV" | postal_abbreviation== "NM" | postal_abbreviation== "UT" | postal_abbreviation== "WY" | postal_abbreviation== "AK" | postal_abbreviation== "CA"| postal_abbreviation== "HI" | postal_abbreviation== "OR" ~ "Western states"
)) |>
    mutate(across(admission:date_named, \(x) as.Date(x, format = "%Y-%m-%d"))) |> #new across() synta, changes columns to data type
    mutate(region = factor(region, levels = c("Midwestern states", "Northeastern states", "Western states", "Southern states"))) #Makes the plot more in-line with relative position of regions

ggplot(state, aes(y = reorder(state, date_named))) + 
    geom_linerange(aes(xmin = date_named, xmax = admission), linewidth = 5.5, color = "#269867", alpha = 0.8) + #Line between points, first so it ends up behind the points
    geom_point(aes(x = admission), size = 5, color = "mediumturquoise") +  #First set of points
    geom_point(aes(x = date_named), size = 5, color = "#035f01") + #Second set of points
    scale_y_discrete(limits=rev) + #From top to bottom, alphabetical ordering of the states
    scale_x_date(limits = c(as.Date("1500-01-01"), as.Date("1975-01-01")), date_break = "100 years", date_labels = "%Y", expand = c(0, 0)) + #Set limits and formatting of the x axis
    facet_wrap(. ~ region, ncol = 2, nrow = 2, scales = "free") + #Wrapping facets in a 2x2 grid
    labs(title = "What's in a name?", subtitle = sub, caption = cap) + #Title, subtitle, caption
    theme_classic() + #Remove most of the theme elements
    theme(axis.line = element_blank(), #No axis lines
          axis.text = element_text(size = 14), #Size of axis labels
          axis.ticks = element_blank(), #No axis ticks
          axis.title = element_blank(), #No axis titles
          panel.background = element_rect(fill = NA, color = NA), #No panel background color = same as plot background
          panel.grid.major.x = element_line(linewidth = 0.5, color = "grey95"), #Solid x-axis grid lines
          panel.grid.major.y = element_line(linewidth = 1, linetype = "12", color = "grey97"), #Dashed y-axis grid-line, linetype = "12" = 1 dash, 2 spaces
          panel.spacing = unit(1, "cm"), #Spacing between facets
          plot.background = element_rect(fill = "#26988510", color = NA), #Sets the whole plot's background color
          plot.caption = element_markdown(margin = margin(20, 0, 0.5, 0), family = "ssp", size = 12), #Caption settings
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 20, margin = margin(b = 25)), #Subtitle settings
          plot.title = element_text(size = 50, family = "ssp", margin = margin(t = 20, b = 25), hjust = 0.5), #Title settings
          plot.title.position = "plot", #Aligns the title and subtitle to the edge of the plot, not the panel, so that hjust = 0.5 makes them centered
          strip.background = element_rect(fill = NA, color = NA), #Remove facet strips
          strip.text = element_text(size = 20, margin = margin(b = 20)), #Facet title text settings
          text = element_text(family = "ssp")) #Axis labels font


