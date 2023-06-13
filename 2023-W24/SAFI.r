setwd("./2023-W24")
install.packages("ggthemes")
library(ggthemes)
install.packages("paletteer")
library(paletteer)


safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')
months <- rownames_to_column(safi_data, "rn") %>%
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
            mutate(village = factor(village, levels = c("Chirodzo", "God", "Ruaca")))

ggplot(months, aes(x = month, y = factor(village, levels = c("Ruaca", "God", "Chirodzo")), fill = mn)) +
    geom_tile(aes(height = 0.95, width = 0.95), color = "#071D26", lwd = 1.5) +
    coord_fixed() +
    scale_fill_paletteer_c(name = "Perceived lack \n of food (mean %)", palette = "grDevices::Lajolla") +
    scale_y_discrete(expand=c(0,0)) +
    scale_x_discrete(expand=c(0,0)) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "#071D26"), plot.background = element_rect(fill = "#071D26"),
    text = element_text(color = "white"), axis.text = element_text(color = "white", size = 20),
    legend.background = element_rect(fill = "#071D26"), legend.position = "bottom", axis.title = element_blank(),
    axis.line = element_blank())
 