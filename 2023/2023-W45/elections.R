setwd("./2023/2023-W45")
library(tidyverse)
library(ggtext)
library(showtext)
library(sf)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "ssp", regular = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Regular.ttf", sep = "/"),
                         bold = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Bold.ttf", sep = "/"),
                         italic = paste(fontsfolder, "Source_Sans_Pro", "SourceSansPro-Italic.ttf", sep = "/")) #Source Sans Pro

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: Grants.gov website | #TidyTuesday week 45 2023"
sub <- "According to grants.gov, 'A grant is a way the government funds your ideas and projects<br>
        to provide public services and stimulate the economy. Grants support critical recovery<br>
        initiatives, innovative research, and many other programs.'<br><br>
        Grants can fall under four main types depending on how they're attributed.<br>
        Below is shown the distribution of the 2000 grants we had data on this week,<br>
        by their type and category. Note that some grants fall under several categories."

#Data wrangling
house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')
house2 <- house |>
    filter(stage == "GEN" & special == FALSE) |>
    # filter(!state_po == "DC") |>
    mutate(party = as.factor(party)) |>
    mutate(party = fct_other(party, keep = c("DEMOCRAT", "REPUBLICAN"))) |>
    mutate(pct = candidatevotes / totalvotes * 100) |>
    slice_max(pct, by = c("year", "state", "district")) |>
    summarize(n = length(candidate), .by = c(year, state_po, party)) |>
    group_by(year, state_po) |>
    mutate(pct = n / sum(n) * 100) |>
    rename(id = state_po)

us <- read_sf("us_states_hexgrid.geojson")

us_map <- fortify(us, region="iso3166_2") |>
    st_transform(crs="+init=epsg:3857")

centers <- cbind.data.frame(data.frame(st_centroid(us_map), id=us$iso3166_2)) |>
    select(geometry, id)
house3 <- merge(house2, centers)
house3 <- cbind(house3, data.frame(st_coordinates(st_cast(house3$geometry))))
house3 <- bind_rows(house3, data.frame(year = c(seq(1976, 2018, 2), 2022), id = "DC", X = -8993986.841, Y = 4447891.376, pct = 0))

us_map1 <- ggplot() +
    geom_sf(data = us_map, color = "grey80", linewidth = 0.8, fill = "#052639") +
    geom_sf_text(data = us_map, aes(label = iso3166_2, geometry = geometry), color = "grey90", size = 4, vjust = -1.8, family = "ssp", fontface = "bold") +
    labs(title = "Haunted places of the USA", subtitle = sub, caption = cap) + #Title, subtitle, caption
    theme_void() +
    theme(plot.background = element_rect(fill = "grey25", color = NA), #Background color
          plot.caption = element_markdown(margin = margin(10, 0, -5, 0), size = 12, color = "grey90"), #Caption formatting
          plot.margin = unit(c(1,1,0.5,0.5),"cm"), #Margins
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 12, lineheight = 1.2, color = "grey90"), #Subtitle settings
          plot.title = element_markdown(size = 30, margin = margin(b = 25), hjust = 0.5, color = "grey90"), #Title settings
          plot.title.position = "plot")
us_map1

for (i in unique(house3$id)) { 
    state_data <- house3[house3$id == i, ]

    p <- ggplot() +
    geom_col(data = state_data, aes(x = year, y = pct, fill = party), show.legend = FALSE, width = 2.5, color = NA) +
    scale_x_discrete(drop=FALSE) +
    scale_fill_manual(values = c("DEMOCRAT" = "#4fa7d3", "REPUBLICAN" = "#ea5d56", "Other" = "grey40")) +
    theme_void()

    g <- ggplotGrob(p)

    Xmin <- mean(state_data[state_data$id == i, ]$X) - 250000
    Xmax <- mean(state_data[state_data$id == i, ]$X) + 250000
    Ymin <- mean(state_data[state_data$id == i, ]$Y) - 190000
    Ymax <- mean(state_data[state_data$id == i, ]$Y) + 170000
    us_map1 <- us_map1 +
    annotation_custom(grob = g,  xmin = Xmin, xmax = Xmax, ymin = Ymin, ymax = Ymax)
}
us_map1

us_map1 + annotation_custom(grob = g, xmin = Xmin, xmax = Xmax, ymin = Ymin, ymax = Ymax)
"#393905"