setwd("./2023/2023-W30")
library(tidyverse)
library(ggtext)
# library(patchwork)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
font_add(family = "fb", regular = paste(dirname(dirname(getwd())), "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in parent (x2) directory
font_add(family = "vd", regular = "C:/Windows/Fonts/verdana.ttf", bold = "C:/Windows/Fonts/verdanab.ttf") #Verdana
font_add(family = "gil", regular = "C:/Windows/Fonts/gil_____.ttf", bold = "C:/Windows/Fonts/gilb____.ttf") #Gill Sans

sub <- "James Lind's 1757 scurvy experiment is thought to be the first controlled clinical trial recorded in history. Lind was the ship's surgeon on board the HMS Salisbury, 
and had a number of scurvy-affected seamen at his disposal. Many remedies had been described and advocated for, with no more than anecdotal evidence. On May 20, 1747, Lind decided 
to try the 6 therapies on the Salisbury in a comparative study in 12 affected seamen. He selected 12 with roughly similar severity, with notable skin and mouth sores, 
weakness of the knees, and significant lassitude, making them unfit for duty. They each received the standard shipboard diet of gruel and mutton broth, supplemented with occasional biscuits and puddings. Each treatment was a dietary supplement (including citrus fruits) or a medicinal."

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Peter Higgins' {medicaldata} R package | #TidyTuesday week 30"

scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv') |>
    mutate(across(.cols = ends_with("d6"), .fns = ~str_extract(., "(?<=_)[a-zA-Z0-9]+"))) |>
    mutate(across(.cols = ends_with("d6"), .fns = ~str_to_title(.))) |>
    rename_with(~str_to_sentence(.), everything()) |>
    rename_with(~str_replace_all(., "_d6", ""), everything()) |>
    mutate(Treatment = str_replace_all(Treatment, "_", " ")) |>
    mutate(Frequency = ifelse(str_detect(Dosing_regimen_for_scurvy, "three"), "Three times a day", "Daily")) |>
    mutate(Regimen = str_replace_all(Dosing_regimen_for_scurvy, " per day|, three times a day|, three times daily| daily| three times a day", "")) |>
    mutate(Regimen = str_to_sentence(case_when(Study_id == "3" | Study_id == "4" | Study_id == "11" | Study_id == "12" ~ paste0(Regimen, " (", Treatment, ")"),
                               Study_id == "7" | Study_id == "8" ~ paste("A", Regimen, "of", Treatment),
                               Study_id == "9" | Study_id == "10" ~ Regimen,
                               TRUE ~ paste(Regimen, "of", Treatment)))) |>
    mutate(Regimen = gsub("peru", "Peru", Regimen)) |>
    select(-Dosing_regimen_for_scurvy, -Treatment)
    
scurvy2 <- scurvy |>
    pivot_longer(cols = !c(Study_id, Frequency, Regimen), names_to = "Parameter", values_to = "Value") |>
    mutate(Patient = rep(1:2, each = 5, times = 6)) |>
    mutate(Parameter = str_replace_all(Parameter, "_", " ")) |>
    mutate(Value = factor(Value, levels = c("None", "Mild", "Moderate", "Severe", "Yes", "No"))) |>
    mutate(Parameter = ifelse(Parameter == "Fit for duty", "Fit for duty?", Parameter)) |>
    mutate(Regimen = ifelse(Regimen == "Two lemons and an orange", "Two lemons\nand an orange", Regimen)) |>
    mutate(Parameter = factor(Parameter, levels = c("Fit for duty?", "Weakness of the knees", "Skin sores", "Lassitude", "Gum rot"))) |>
    mutate(Regimen = factor(Regimen, levels = c("25 drops of elixir of vitriol (dilute sulfuric acid)", 
    "A nutmeg-sized paste of garlic, mustard seed, horseradish, balsam of Peru, and gum myrrh (purgative mixture)",
    "Two spoonfuls of vinegar", "1 quart of cider", "A half pint of sea water", "Two lemons\nand an orange")))


ggplot(scurvy2, aes(x = Patient, y = Parameter)) +
    geom_tile(data = scurvy2, aes(fill = Value, height = 0.80, width = 0.80)) +
    geom_point(data = scurvy2 |> filter(Parameter == "Fit for duty?" & Value == "Yes"), shape = "\u2714", size = 14, color = "darkorange3") +
    geom_point(data = scurvy2 |> filter(Parameter == "Fit for duty?" & Value == "No"), shape = "\u2716", size = 20, color = "black") +
    geom_text(aes(label = ifelse(Patient == "1", Frequency, NA), y = 6, x = 1.5), family = "gil", size = 4, hjust = 0.5, check_overlap = TRUE) +
    expand_limits(y = 6.5) +
    coord_fixed() +
    facet_grid(. ~ Regimen) +
    scale_y_discrete(labels = c("<span style = 'font-size:18pt;'>**Fit for duty?**</span>", "Weakness of the knees", "Skin sores", "Lassitude", "Gum rot", "<span style = 'color: darkorange3;'>**Frequency**</span>"), limits = c("Fit for duty?", "Weakness of the knees", "Skin sores", "Lassitude", "Gum rot", "Frequency")) +
    scale_fill_manual("Severity of the symptom at day 6", values = c("orange","chocolate3", "#703901", "black", NA, NA), na.value = NA, breaks = c("None", "Mild", "Moderate", "Severe")) + 
    labs(title = "The first controlled clinical trial ?", subtitle = sub, caption = cap) +
    theme_classic() +
    theme(panel.spacing.x = unit(40, "pt"), axis.title = element_blank(), legend.position = "bottom",
        axis.ticks = element_blank(), axis.text.x = element_blank(), plot.background = element_rect(fill = "#f5e4c9", color = NA), 
        panel.background = element_rect(fill = "#f5e4c9", color = NA), axis.line = element_blank(), 
        plot.caption = element_markdown(margin = margin(20, 0, 0.5, 0)),
        legend.background = element_rect(fill = "#f5e4c9", color = NA), strip.background = element_rect(fill = "#f5e4c9", color = NA),
        axis.text.y = element_markdown(family = "gil", size = 14, color = "black"),
        plot.title = element_text(size = 30, family = "gil", margin = margin(t = 20, b = 30), hjust = 0.05),
        plot.subtitle = element_textbox_simple(family = "vd", margin = margin(r = 50, l = -100, b = 20), hjust = 0, halign = 0),
        plot.margin = margin(20, 20, 20, 20), strip.text = element_text(family = "gil", size = 10),
        legend.text = element_text(size = 14), legend.title = element_text(size = 18, margin = margin(0, 10, 0, 0)),
        axis.text.x.top = element_markdown(face = "bold"))

# gg2 <- ggplot(data = scurvy2 |> filter(Parameter == "Fit for duty"), aes(x = Patient, y = Parameter)) +
#     geom_tile(aes(fill = Value, height = 0.9, width = 0.9)) +
#     geom_point(data = scurvy2 |> filter(Parameter == "Fit for duty" & Value == "Yes"), shape = "\u2714", size = 14, color = "orange") +
#     geom_point(data = scurvy2 |> filter(Parameter == "Fit for duty" & Value == "No"), shape = "\u2716", size = 20, color = "orange") +
#     coord_fixed() +
#     facet_wrap(. ~ Regimen, scales = "free") +
#     theme_classic() +
#     theme(strip.text = element_blank())

# gg1 / gg2 + plot_layout(heights = c(1, 1)) +
# plot_annotation(caption = cap) & 
# theme(panel.spacing.x = unit(30, "pt"), axis.title = element_blank(),
# axis.ticks = element_blank(), axis.text.x = element_blank(), plot.background = element_rect(fill = "#FFDBA2", color = NA), 
# panel.background = element_rect(fill = "#FFDBA2", color = NA), axis.line = element_blank(), plot.caption = element_markdown(),
# legend.background = element_rect(fill = "#FFDBA2", color = NA), strip.background = element_rect(fill = "#FFDBA2", color = NA))

