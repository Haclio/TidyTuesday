install.packages("ggtext")
library(ggtext) #To add colors to specific words in the title
library(tidyverse)
setwd("./2023-W21")

centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv') |>
    mutate(ageround = floor(age)) |> #Rounding down the age
    mutate(death_date = as.Date(ifelse(is.na(death_date), as.Date("2023-05-31"), death_date))) |> #Replacing the NA death date of people still alive with a value for the plot
    mutate(across(c(birth_date, death_date), as.Date, format = "%Y-%m-%d")) |> #Changing data type to date
    group_by(gender) |> #Grouping for arrange() below
    arrange(desc(birth_date), .by_group = TRUE) |> #Grouping by birth date, with respect of the gender
    mutate(num = rep(1:100)) #Adding an arbitrary number for ordering

cent_sum <- centenarians |>
    reframe(n = n(), .by = c(ageround, gender)) |> #Summarizing by age and gender
    complete(ageround = 111:122, gender, fill = list(n = 0)) #Filling the missing combinations

#Age distribution plot
ggplot(data = cent_sum) +
geom_bar(aes(x = ageround, y = n, fill = gender), width = 0.6, position = position_dodge(), stat = "identity") + #Dodged bar chart
geom_text(aes(x = ageround, y = n, label = ifelse(n == 0, NA, n), group = gender), position = position_dodge(width = 0.6), vjust = -0.5) + #Labels, with no text if label = 0
ggtitle("Distribution of the 100 oldest men and women recorded \n by age") + #Title with a line skip
xlab("Age (rounded down)") +
ylab("Count") +
scale_y_continuous(limits = c(0, 45), expand = c(0, 0)) +
scale_x_continuous(breaks = c(111:122), expand = c(0.02, 0)) +
scale_fill_manual("Gender", values = c("red3", "steelblue4"), labels = c("Women", "Men")) + #Legend title, colors and keys
theme_classic() +
theme(axis.text.y = element_text(size = 20), axis.text.x = element_text(size = 14), 
        panel.background = element_rect(fill = "#fff6ec"), plot.background = element_rect(fill = "#fff6ec"),
        strip.background = element_blank(), strip.text.x = element_blank(),
        legend.background = element_rect(fill = "#fff6ec"), legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 16), legend.title = element_text(face = "bold", size = 16),
        axis.title = element_text(size = 16))

#Dumbbell birth/death dates plot
ggplot(centenarians, aes(x = birth_date, y = num)) +
    geom_segment(aes(xend = death_date, yend = num)) + #Line between the points, has to be put before for it to appear under the points
    geom_point(aes(color = gender), show.legend = FALSE) + #Birth date points
    geom_point(aes(x = death_date, color = ifelse(centenarians$death_date == "2023-05-31", NA, gender)), show.legend = FALSE) + #Death date points, people with a date = 31/05/2023 are in yellow
    ggtitle("Birth and death dates of the 100 oldest men and women  \n recorded. <span style='color: orange2;'>Yellow means the person is still alive.<span>") + #Title styled with ggtext, requires "element_markdown" in theme()
    scale_color_manual("Gender", values = c("red3", "steelblue4"), na.value = "orange2") + #Setting colors and the NA color
    scale_x_date(limits = c(as.Date("1850-01-01"), as.Date("2030-01-01")), date_breaks = "25 years", date_labels = ("%Y"), expand = c(0, 0)) + #Setting the scale
    facet_grid(gender ~ ., scales = "free", labeller = as_labeller(c("female" = "Women", "male" = "Men"))) + #Setting the facets + titles
    theme_classic() + #Removes grid lines and panel background
    theme(axis.text.y = element_blank(), panel.grid.major.x = element_line(linewidth = 2), #Removes y labels and sets major grid lines
            axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.line.y = element_blank(), #Removes y axis, ticks and title
            axis.text.x = element_text(size = 20), plot.title = element_markdown(size = 20), #Sets size of the x labels and plot title, element_markdown() to use ggtext
            strip.text = element_text(size = 20, face = "bold"), strip.background = element_rect(fill = "#fff6ec"), #Sets facet strip text formatting and background color
            panel.background = element_rect(fill = "#fff6ec"), plot.background = element_rect(fill = "#fff6ec"), #Sets background and panel color
            axis.title.x = element_blank()) #Removes x axis title

