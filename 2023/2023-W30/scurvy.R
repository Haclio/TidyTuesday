setwd("./2023/2023-W30")
library(tidyverse)
library(ggtext)
# library(patchwork)
library(showtext)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
fontsfolder <- paste(dirname(dirname(getwd())), "Fonts", sep = "/")
font_add(family = "fb", regular = paste(fontsfolder, "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in fonts directory
font_add(family = "vd", regular = "C:/Windows/Fonts/verdana.ttf", bold = "C:/Windows/Fonts/verdanab.ttf") #Verdana
font_add(family = "gil", regular = "C:/Windows/Fonts/gil_____.ttf", bold = "C:/Windows/Fonts/gilb____.ttf") #Gill Sans

sub <- "James Lind's 1747 scurvy experiment is thought to be the first controlled clinical trial recorded in history. Lind was the ship's surgeon on board the HMS Salisbury, 
and had a number of scurvy-affected seamen at his disposal. Many remedies had been described and advocated for, with no more than anecdotal evidence. On May 20, 1747, Lind decided 
to try the 6 therapies on the Salisbury in a comparative study in 12 affected seamen. He selected 12 with roughly similar severity, with notable skin and mouth sores, 
weakness of the knees, and significant lassitude, making them unfit for duty. They each received the standard shipboard diet of gruel and mutton broth, supplemented with occasional biscuits and puddings. Each treatment was a dietary supplement (including citrus fruits) or a medicinal. It is now known that scurvy is caused by vitamin C deficit, which can be found in citrus fruits."

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
    mutate(Regimen = ifelse(Regimen == "Two lemons and an orange", "Two lemons and an orange", Regimen)) |>
    mutate(Parameter = factor(Parameter, levels = c("Fit for duty?", "Weakness of the knees", "Skin sores", "Lassitude", "Gum rot"))) |>
    mutate(Regimen = factor(Regimen, levels = c("25 drops of elixir of vitriol (dilute sulfuric acid)", 
    "A nutmeg-sized paste of garlic, mustard seed, horseradish, balsam of Peru, and gum myrrh (purgative mixture)",
    "Two spoonfuls of vinegar", "1 quart of cider", "A half pint of sea water", "Two lemons and an orange")))


ggplot(scurvy2, aes(x = Patient, y = Parameter)) +
    geom_tile(data = scurvy2, aes(fill = Value, height = 0.90, width = 0.90)) +
    geom_point(data = scurvy2 |> filter(Parameter == "Fit for duty?" & Value == "Yes"), shape = "\u2714", size = 13, color = "darkorange3") +
    geom_point(data = scurvy2 |> filter(Parameter == "Fit for duty?" & Value == "No"), shape = "\u2716", size = 20, color = "black") +
    geom_text(aes(label = ifelse(Patient == "1", Frequency, NA), y = 6, x = 1.5), family = "gil", size = 3, hjust = 0.5, check_overlap = TRUE, color = "darkorange4") +
    expand_limits(y = 6.5, x = c(-0.2, 1.5)) +
    coord_fixed() +
    facet_grid(. ~ Regimen, labeller = label_wrap_gen(width = 23)) +
    scale_y_discrete(labels = c("<span style = 'font-size:18pt;'>**Fit for duty?**</span>", "Weakness of the knees", "Skin sores", "Lassitude", "Gum rot", "<span style = 'font-size:12pt;color: darkorange4;'>**Frequency**</span>"), limits = c("Fit for duty?", "Weakness of the knees", "Skin sores", "Lassitude", "Gum rot", "Frequency")) +
    scale_fill_manual("Severity of the symptom at day 6", values = c("orange","chocolate3", "#703901", "black", NA, NA), na.value = NA, breaks = c("None", "Mild", "Moderate", "Severe")) + 
    labs(title = "The first controlled clinical trial in history?", subtitle = sub, caption = cap) +
    theme_classic() +
    theme(axis.title = element_blank(), 
        legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        plot.background = element_rect(fill = "#f5e4c9", color = NA), 
        panel.background = element_rect(fill = "#f5e4c9", color = NA), 
        axis.line = element_blank(), 
        plot.caption = element_markdown(margin = margin(20, 0, 0.5, 0), family = "gil"),
        legend.background = element_rect(fill = "#f5e4c9", color = NA), 
        strip.background = element_rect(fill = "#f5e4c9", color = NA),
        axis.text.y = element_markdown(family = "gil", size = 14, color = "black"),
        plot.title = element_text(size = 40, family = "gil", margin = margin(t = 0, b = 25), hjust = -0.1),
        plot.subtitle = element_textbox_simple(family = "gil", margin = margin(r = 30, l = -100, b = 20), hjust = 0.5, halign = 0.5, size = 11),
        plot.margin = margin(30, 50, 20, 50), 
        strip.text = element_text(family = "gil", size = 11, margin = margin(l = 30, b = 3), vjust = 0),
        legend.text = element_text(size = 14, family = "gil"), 
        legend.title = element_text(size = 18, margin = margin(0, 10, 0, 0), family = "gil"))
