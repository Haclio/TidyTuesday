setwd("./2023/2023-W33")
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

cap <- "<span style='font-family:fb;'>&#xf09b; </span> Haclio  |  <span style='font-family:fb;'> &#xf099; </span>@LouisNadalin | Source: Vincent Arel-Bundock's {Rdatasets} package | #TidyTuesday week 33"
sub <- ""

spam <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv')
spam_money <- spam |>
    filter(!yesno == "n") |>
    select(crl.tot, dollar, bang, money, n000)

spam_money2 <- spam |>
    filter(!dollar == 0, !money == 0, !n000 == 0) |>
    select(dollar, money, n000)

ggplot() +
# geom_freqpoly(data = spam_money, aes(crl.tot, after_stat(density)), binwidth = 50, color = "red") +
geom_freqpoly(data = spam_money2, aes(dollar), binwidth = 0.1, color = "blue") +
geom_freqpoly(data = spam_money2, aes(money), binwidth = 0.1, color = "green") +
geom_freqpoly(data = spam_money2, aes(n000), binwidth = 0.1, color = "orange")
