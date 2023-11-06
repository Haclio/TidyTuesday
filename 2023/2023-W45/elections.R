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

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Bluesky : @louisnadalin.bsky.social | Source: MIT Election Data and Science Lab | #TidyTuesday week 45 2023"
sub <- "The United States House of Representatives is the lower chamber of the US Congress, the Senate being the upper chamber.<br>
        The purpose of both chambers is to pass federal legislation, better known as bills. The House also has dedicated powers,<br>
        such as originating revenue bills, or initiating impeachment proceedings.<br><br>
        Each state's plot below represents the affiliation of its representatives in each even-year election,<br>
        from 1976 to 2022, between <span style='color: #4fa7d3'>**Democrats**</span>, <span style='color: #ea5d56'>**Republicans**</span>, and <span style='color: #739627'>**other affiliations**</span>.<br>
        <span>*Note that the District of Columbia is shown here but its representative doesn't have any voting power in the House.*</span>"

#Data wrangling
house <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-07/house.csv')
house2 <- house |>
    filter(stage == "GEN" & special == FALSE) |> #Only keep regular general elections
    filter(!state_po == "DC") |> #Remove DC, only one data point
    mutate(party = as.factor(party)) |>
    mutate(party = fct_other(party, keep = c("DEMOCRAT", "REPUBLICAN"))) |> #Collapses non-D, non-R levels into 'other'
    mutate(pct = candidatevotes / totalvotes * 100) |>
    slice_max(pct, by = c("year", "state", "district")) |> #Keep the maximum value per district = the winner of the election
    summarize(n = length(candidate), .by = c(year, state_po, party)) |>
    group_by(year, state_po) |>
    mutate(pct = n / sum(n) * 100) |> #% of D/R/misc candidates
    rename(id = state_po)

us <- read_sf("us_states_hexgrid.geojson") #Hexagon US base map

us_map <- fortify(us, region = "iso3166_2") |> #isoxxx = state name abbreviations
    st_transform(crs="+init=epsg:3857") #To make the hexagons into the right shape

centers <- cbind.data.frame(data.frame(st_centroid(us_map), id=us$iso3166_2)) |>
    select(geometry, id) #Centroid calculations for plot locations
house3 <- merge(house2, centers)
house3 <- cbind(house3, data.frame(st_coordinates(st_cast(house3$geometry)))) #centroids geometry as numbers to calculate easily

us_map1 <- ggplot() + #Base hexagon map with title, subtitle and caption
    geom_sf(data = us_map, color = "grey80", linewidth = 0.8, fill = ifelse(us_map$iso3166_2 == "DC", "grey55", "#052639")) +
    geom_sf_text(data = us_map, aes(label = iso3166_2, geometry = geometry), color = "grey90", size = 3.5, vjust = -2.4, family = "ssp", fontface = "bold") +
    labs(title = "US House of Representatives distribution<br>1976 to 2022", subtitle = sub, caption = cap) + #Title, subtitle, caption
    theme_void() +
    theme(plot.background = element_rect(fill = "grey98", color = NA), #Background color
          plot.caption = element_markdown(margin = margin(10, -15, -5, 0), size = 10, color = "grey20"), #Caption formatting
          plot.margin = unit(c(1,1,0.5,0.5),"cm"), #Margins
          plot.subtitle = element_markdown(family = "ssp", hjust = 0.5, halign = 0.5, size = 12, lineheight = 1.2, color = "grey20"), #Subtitle settings
          plot.title = element_markdown(size = 28, margin = margin(b = 10), hjust = 0.5, color = "grey20", lineheight = 1.2), #Title settings
          plot.title.position = "plot")
us_map1

for (i in unique(house3$id)) { #Loop over the states to create each subplot, then add it to the main plot with calculated coordinates
    state_data <- house3[house3$id == i, ]

    p <- ggplot() +
    geom_rect(data = house3, aes(xmin = 1974, xmax = 2024, ymin = -2, ymax = 102), color = ifelse(house3$id == "DC", NA, "white"), fill = NA, linewidth = 3) +
    geom_col(data = state_data, aes(x = year, y = pct, fill = party), show.legend = FALSE, width = 2.5, color = NA) +
    scale_x_discrete(expand = c(0, 0), drop=FALSE) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c("DEMOCRAT" = "#5abced", "REPUBLICAN" = "#e45f58", "Other" = "olivedrab3")) +
    theme_void()

    g <- ggplotGrob(p)

    Xmin <- mean(state_data[state_data$id == i, ]$X) - 240000
    Xmax <- mean(state_data[state_data$id == i, ]$X) + 240000
    Ymin <- mean(state_data[state_data$id == i, ]$Y) - 180000
    Ymax <- mean(state_data[state_data$id == i, ]$Y) + 160000
    us_map1 <- us_map1 +
    annotation_custom(grob = g,  xmin = Xmin, xmax = Xmax, ymin = Ymin, ymax = Ymax)
}
us_map1

ggsave(filename = "house.png", width = 2052, height = 1800, units = "px", scale = 1.45)
