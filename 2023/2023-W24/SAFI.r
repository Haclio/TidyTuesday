setwd("./2023/2023-W24")
install.packages("paletteer")
library(paletteer)
library(ggtext)

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')

months <- rownames_to_column(safi_data, "rn") %>% #Creates a wide format table with months and the corresponding food values
        separate_rows(months_lack_food) %>% 
        group_by(rn, months_lack_food) %>% 
        tally %>% 
        spread(months_lack_food, n, fill=0) %>%
        ungroup() %>% 
        select(-none) |>
        mutate(rn = as.numeric(rn)) |>
        select(rn, Jan, Feb, Mar, Apr, May, June, July, Aug, Sept, Oct, Nov, Dec) |>
        arrange(rn)

safi_data  <- cbind(safi_data, months)
months  <- safi_data |> 
            select(-c(1, 3:15)) |>
            pivot_longer(cols = !village, names_to = "month", values_to = "lacking_food") |>
            summarize(mn = mean(lacking_food)*100, .by = c(village, month)) |>
            mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))) |>
            mutate(month = str_sub(month, 1, 3)) |>
            mutate(month = match(month, month.abb)) |>
            mutate(month = ifelse(nchar(month) == 1, paste("0", month, sep = ""), month)) |>
            mutate(village = factor(village, levels = c("Chirodzo", "God", "Ruaca")))

ggplot(months, aes(x = month, y = factor(village, levels = c("Ruaca", "God", "Chirodzo")), fill = mn)) +
    geom_raster(fill = "#071D26") +
    geom_tile(aes(height = 0.95, width = 0.95), color = "#071D26", lwd = 1.5) +
    coord_fixed() +
    scale_fill_paletteer_c(limits = c(0, 100), name = "Perceived lack of food (mean %)", palette = "grDevices::Lajolla") +
    scale_y_discrete(expand=c(0,0)) +
    scale_x_discrete("Month", expand=c(0,0)) +
    labs(caption = "Twitter : @LouisNadalin | Github : Haclio | Dataset : SAFI survey | #Tidytuesday week 24") +
    ggtitle("Declared lack of food in Chirodzo, God and Ruaca \n (villages in Tanzania & Mozambique)") +
    theme(panel.background = element_rect(fill = "#071D26"), plot.background = element_rect(fill = "#071D26"), plot.margin = unit(c(0.5, 0.5, 3, 0.5), "cm"),
    text = element_text(color = "white", family = "Century Schoolbook"), axis.text = element_text(color = "white", size = 20, family = "Century Schoolbook"),
    legend.background = element_blank(), legend.margin = margin(20, 0, 0, 0), legend.position = "bottom", axis.title.y = element_blank(),
    axis.line = element_blank(), axis.ticks = element_blank(), plot.caption = element_text(size = 16, vjust = -25),
    plot.title = element_text(face = "bold", color = "white", size = 30, family = "Century Schoolbook", hjust = 0.5, vjust = 5, margin = margin(25,0,50,0)),
    legend.text = element_text(size = 16), legend.title = element_text(size = 20), legend.key.size = unit(40, "pt"),
    axis.title.x = element_text(size = 20, margin = margin(20, 0, 0, 0)))


