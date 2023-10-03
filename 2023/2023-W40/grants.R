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
sub <- "According to grants.gov, 'A grant is a way the government funds your ideas and projects<br>
        to provide public services and stimulate the economy. Grants support critical recovery<br>
        initiatives, innovative research, and many other programs.'<br><br>
        Grants can fall under four main types depending on how they're attributed.<br>
        Below is shown the distribution of the 2000 grants we had data on this week,<br>
        by their type and category. Note that some grants fall under several categories."

#Data wrangling
# grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')
grant_opportunity_details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')
sum <- grant_opportunity_details |>
    group_by(opportunity_category) |>
    reframe(across(starts_with("category"), ~sum(. == "TRUE"))) |>
    select(-last_col()) |>
    pivot_longer(!opportunity_category, names_to = "category", values_to = "number") |>
    mutate(category = gsub("([[:alpha:]])([[:alpha:]]+)", "\\U\\1\\L\\2", category, perl=TRUE)) |> #Sentence case
    mutate(category = gsub("Category_", "", category)) |>
    mutate(category = gsub("_", " ", category)) |>
    mutate(category = case_when(category == "Business" ~ "Business and Commerce",
                                category == "Disaster" ~ "Disaster Prevention and Relief",
                                category == "Employment" ~ "Employment, Labor and Training",
                                category == "Food" ~ "Food and Nutrition",
                                category == "Income Security" ~ "Income Security and Social Services",
                                category == "Info" ~ "Information and Statistics",
                                category == "Iija" ~ "Infrastructure Investment and Jobs Act",
                                category == "Law" ~ "Law, Justice and Legal Services",
                                category == "Opportunity Zone" ~ "Opportunity Zone Benefits",
                                category == "Science" ~ "Science, Technology and other R&D",
                                TRUE ~ category)) |>
    mutate(category = fct_relevel(category, "Other", after = Inf)) |>
    mutate(opportunity_category = case_when(opportunity_category == "Continuation" ~ "Continuation\ngrants",
                                            opportunity_category == "Discretionary" ~ "Discretionary\ngrants",
                                            opportunity_category == "Earmark" ~ "Earmarks",
                                            opportunity_category == "Mandatory" ~ "Mandatory\ngrants",
                                            TRUE ~ "Miscellaneous\ngrants"))

ggplot(sum |> filter(number > 0)) +
    geom_col(aes(x = number, y = as.factor(category), fill = opportunity_category), show.legend = FALSE, width = 0.3) +
    facet_grid(. ~ opportunity_category) +
    coord_trans(x = "pseudo_log") + #Pseudolog necessary as log won't display any values
    scale_x_continuous(limits = c(0, 1100), breaks = c(1, 10, 100, 1000)) +
    scale_y_discrete(limits = rev) + #When using horizontal labels, they're sorted in reverse way, this fixes it
    scale_fill_manual(values = c25) +
    xlab("Number of grants") +
    ylab("Grant type") +
    labs(title = "US Government<br>Grant Opportunities", subtitle = sub, caption = cap) + #Title, subtitle, caption
    theme_minimal() +
    theme(axis.line.y = element_blank(), #No y axis line
          axis.text.x = element_text(size = 14), #Axis labels formatting
          axis.text.y = element_text(size = 11, face = "bold"), #Axis labels formatting
          axis.ticks.y = element_blank(), #No y ticks
          axis.title.x = element_text(color = "grey60", size = 14, margin = margin(5, 0, 0, 0), hjust = 1), #Axis title formatting
          axis.title.y = element_blank(), #No y axis title
          panel.background = element_rect(fill = "#ffefe6", color = NA), #Panel background color = plot background color
          plot.background = element_rect(fill = "#FFF9F6"), #Background color
          plot.caption = element_markdown(margin = margin(10, 0, -5, 0), size = 12), #Caption formatting
          panel.grid.major.x = element_line(color = "grey80"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.spacing = unit(15, "pt"),
          plot.margin = unit(c(1,1,0.5,0.5),"cm"), #Margins
          plot.subtitle = element_textbox(, hjust = 0.98, halign = 0.5, size = 10, margin = margin(t = -80, b = 20), lineheight = 1.2), #Subtitle settings
          plot.title = element_markdown(size = 42, family = "ssp", margin = margin(t = 5, b = 0, l = 10), hjust = 0), #Title settings
          plot.title.position = "plot", #Aligns the title and subtitle to the edge of the plot, not the panel, so that hjust = 0.5 makes them centere
          strip.background = element_rect(fill = "#ffefe6", color = NA), #Remove facet strips
          strip.text = element_text(size = 16, margin = margin(t = 5, b = 10)), #Facet title text settings
          text = element_text(family = "ssp")) #General font
