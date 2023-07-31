setwd("./2023/2023-W29")
library(tidyverse)
library(showtext)
library(ggtext)
# remotes::install_github("liamgilbey/ggwaffle")
# library(ggwaffle)
# install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
# install.packages("ggh4x")
# library(ggh4x)
library(patchwork)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
font_add(family = "fb", regular = paste(dirname(dirname(getwd())), "Fonts", "Font Awesome 6 Brands-Regular-400.otf", sep = "/")) #Brand logos in parent (x2) directory
font_add(family = "vd", regular = "C:/Windows/Fonts/verdana.ttf", bold = "C:/Windows/Fonts/verdanab.ttf") #Verdana
cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Simon Counch's {detectors} R package | #TidyTuesday week 29"

detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')
det_sum <- detectors |>
    summarize(n = n(), .by = c(".pred_class", "native")) |>
    filter(!is.na(native)) |>
    group_by(native) |>
    mutate(pct = round(n/sum(n)*100)) |>
    mutate(native = factor(native, levels = c("Yes", "No"))) |>
    arrange(native, .pred_class)

det_bydet <- detectors |>
    filter(!is.na(native) & native == "No") |>
    summarize(n = n(), .by = c(".pred_class", "detector")) |>
    group_by(detector) |>
    mutate(pct = round(n/sum(n)*100)) |>
    arrange(detector, .pred_class) |>
    mutate(lab = ifelse(.pred_class == "AI", paste(detector, ":", pct, "%"), NA)) |>
    group_by(detector) |>
    fill(lab, .direction = "down")

toplabs <- data.frame(native = c("Yes", "No"), lab = c("Out of 1831 texts written by native English writers, <br><span style = 'color: #daaa32;'>**97%**</span> were classified as <span style = 'color: #daaa32;'>**written by a human**</span>...", "Out of 637 texts written by non-native English writers, <br><span style = 'color: #daaa32;'>**only 39%**</span> were classified as <span style = 'color: #daaa32;'>**written by a human**</span>..."))
bottomlabs <- data.frame(native = c("Yes", "No"), lab = c("...while only <span style = 'color: grey70;'>**3%**</span> were classified as <span style = 'color: grey70;'>**AI-generated**</span>.", "...while <span style = 'color: grey70;'>**61%**</span> were classified as <span style = 'color: grey70;'>**AI-generated**</span>."))

gg1 <- ggplot(det_sum) +
geom_waffle(aes(values = pct, fill = .pred_class), na.rm = FALSE, flip = TRUE, radius = unit(10, "pt"), size = 0, show.legend = FALSE) +
facet_grid(. ~ rev(native), scales = "free") +
theme_void() +
ylim(-4, 14) +
xlim(-4, 14) +
theme(aspect.ratio = 1) +
labs(title = "GPT detectors disproportionately classify \nnon-native English writing as AI-generated") +
geom_richtext(data = toplabs, aes(x = -3, y = 12, label = lab), color = "white", hjust = 0, size = 3.8, family = "vd", label.color = NA, fill = NA) +
geom_richtext(data = bottomlabs, aes(x = -3, y = -1, label = lab), color = "white", hjust = 0, size = 3.8, family = "vd", label.color = NA, fill = NA) +
scale_fill_manual(values = c("grey50", "#CD950C")) +
theme(plot.background = element_rect(fill = "#25251f", color = NA), panel.background = element_rect(fill = "#25251f", color = NA),
text = element_text(color = "white", family = "vd"), plot.margin = margin(-1, -1.1, -50, -1.2, "cm"),
strip.text.x = element_blank(), plot.title = element_text(size = 30, family = "vd", margin = margin(t = 20, b = 10)))

ggCGHO <- ggplot(det_bydet |> filter(detector == "GPTZero" | detector == "HFOpenAI" | detector == "OriginalityAI" | detector == "Crossplag")) +
geom_waffle(aes(values = pct, fill = .pred_class), na.rm = FALSE, flip = TRUE, radius = unit(6, "pt"), size = 0, show.legend = FALSE) +
theme_void() +
theme(aspect.ratio = 0.8, strip.text.x = element_text(size = 12, color = "white", family = "vd", face = "bold", vjust = 2, hjust = 0.55, margin = margin(10,0,10,0, "pt"))) +
xlim(-1, 11) +
coord_fixed() +
scale_fill_manual(values = c("grey50", "#CD950C")) +
facet_wrap(. ~ lab, nrow = 1)


ggQSZ <- ggplot(det_bydet |> filter(detector == "Quil" | detector == "Sapling" | detector == "ZeroGPT")) +
geom_waffle(aes(values = pct, fill = .pred_class), na.rm = FALSE, flip = TRUE, radius = unit(6, "pt"), size = 0, show.legend = FALSE) +
theme(aspect.ratio = 1) + 
coord_fixed() +
xlim(-1, 11) +
scale_fill_manual(values = c("grey50", "#CD950C")) +
facet_wrap(. ~ lab, nrow = 1)

emptyplot <- ggplot()
gg2 <- (emptyplot + ggQSZ + emptyplot + plot_layout(widths = c(0.04, 0.92, 0.04))) &
 theme_void() & 
 theme(strip.text.x = element_text(size = 12, color = "white", family = "vd", face = "bold", vjust = 2, hjust = 0.55,
 margin = margin(10,0,10,0, "pt")))

gg3 <- ggCGHO / gg2 +
plot_annotation(title = "Some detectors do better than others :", cap = "Attribution of non-native English writing as AI-generated") &
theme(plot.background = element_rect(fill = "#25251f", color = NA), panel.background = element_rect(fill = "#25251f", color = NA), plot.margin = margin(0.2, 0, 0.2, 0, "cm"), 
plot.title = element_text(size = 20, family = "vd", margin = margin(t = -50, b = 10), color = "white"),
plot.caption = element_markdown(color = "white", size = 14, hjust = 0.5, face = "bold", family = "vd"))

gg1 / wrap_elements(gg3) + plot_layout(heights = c(1, 1.2)) +
plot_annotation(caption = cap) &
theme(plot.background = element_rect(fill = "#25251f", color = NA), panel.background = element_rect(fill = "#25251f", color = NA),
text = element_text(color = "white", family = "vd"), plot.margin = margin(1, 1.1, 1, 1.2, "cm"), 
plot.caption = element_markdown(color = "white", margin = margin(t = 30), size = 10, hjust = 1))
