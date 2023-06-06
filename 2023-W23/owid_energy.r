library(tidyverse)
library(stringr)
library(sf)
setwd("./2023-W23")
mytheme <- theme_set(theme_classic() + theme(legend.background = element_rect(fill = "#fff6ec"), panel.background = element_rect(fill = "#fff6ec"),
plot.background = element_rect(fill = "#fff6ec"), text = element_text(family = "Verdana"), legend.position = "bottom", axis.title.x = element_blank(), axis.text = element_text(size = 20),
    panel.grid.major = element_line(color = "black", linetype = "dashed"), axis.line = element_blank(),
    legend.text = element_text(size = 16, family = "Verdana"), legend.title = element_text(face = "bold", size = 16, family = "Verdana"),
    axis.title.y = element_text(size = 20)))

owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')
owid_energy_fr <- owid_energy |>    
    filter(country == "France")
owid_energy_fr_prod <- owid_energy_fr |>
    select(year, ends_with("share_elec")) |>
    select(-c(fossil_share_elec, renewables_share_elec, other_renewables_share_elec, low_carbon_share_elec)) |>
    pivot_longer(cols = !year, names_to = "Energy_type", values_to = "share") |>
    mutate(Energy_type = str_to_title(gsub("_share_elec", "", Energy_type)))

ggplot(owid_energy_fr_prod) +
    geom_area(aes(x = year, y = share, fill = Energy_type), size = 1.5, alpha = 0.9) +
    geom_hline(aes(yintercept = 0)) + #Used to replace the axes, quite hacky
    geom_vline(aes(xintercept = 1985)) + #Used to replace the axes, quite hacky
    scale_fill_manual(name = "Energy type", values = c("green3", "black", "grey50", "dodgerblue4", "orange", "red2", "yellow2", "cadetblue2")) +
    ylab("Share (%)") +
    ggtitle("Distribution of energy produced by France by type, 1985-2022") +
    scale_x_continuous(limits = c(1985, 2022), breaks = seq(1985, 2022, 5), expand = c(0.01, 0.01)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0.01, 0.01))

euro_countries <- codelist |>
    filter(continent == "Europe") |>
    select(3, 6)

owid_energy_euro <- subset(owid_energy, country %in% euro_countries$country.name.en) |>
    filter(year == "2022") |>
    select(c(country, electricity_generation, electricity_demand)) |>
    pivot_longer(cols = !country, names_to = "Type", values_to = "Value")

ggplot(owid_energy_euro) +
    geom_bar(aes(x = country, y = Value, fill = Type), position = "dodge", stat = "identity")
