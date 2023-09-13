setwd("./2023/2023-W37")
library(tidyverse)
library(plotly)

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv') |>
    filter(country_iso3 == "FRA")

#Formatting for Plotly's sunburst chart
cat <- all_countries$Category
subc <- all_countries$Subcategory
val <- format(round(all_countries$hoursPerDayCombined, 2), nsmall = 2)

df <- data.frame(labels = subc, parents = cat, values = val)
sums <- aggregate(as.numeric(df$values), by=list(Category=df$parents), FUN=sum)
colnames(sums) <- c("labels", "values")
df2 <- merge(sums, df, all = TRUE) |>
    mutate(hour = as.numeric(sub("\\..*$", "", df2$val))) |> #Everything before a dot
    mutate(min = paste(0, sub("^[^.]*\\.", "", df2$val), sep = ".")) |> #Everything after a dot
    mutate(time = ifelse(hour == "0", paste(round(as.numeric(min) * 60), "min"), paste(hour, "hr", round(as.numeric(min) * 60), "min")))

fig <- plot_ly(
  labels = df2$labels,
  parents = df2$parents,
  values = df2$values,
  type = 'sunburst', branchvalues = "total", insidetextorientation = 'radial', hovertext = paste(df2$time), hoverinfo = "text") |>
  layout(title= list(text = "<b>How do French people spend their 24 hours each day?</b>", font = list(size = 25), y = 2, yanchor = "bottom", yref = "container", margin = list(t = 100, b = 50), automargin = FALSE), margin = list(t = 150, b = 50, pad = list(t = 0)),
  annotations = list(text = "Source: The Human Chronome Project | #TidyTuesday week 37<br>Github: Haclio | Twitter: @LouisNadalin | Bluesky: @louisnadalin.bsky.social", font = list(size = 10), x = 0.5, y = -0.05, showarrow = FALSE, align = "center"))
fig <- fig |>
    add_annotations(text = "The Human Chronome project aims to gain understanding and quantify how us, <br>humans, occupy ourselves in the Anthropocene", font = list(size = 16), x = 0.5, y = 1.075, showarrow = FALSE, align = "center")

htmlwidgets::saveWidget(
                widget = fig, #the plotly object
                file = "interactiveplot.html" #the path & file name
                # selfcontained = TRUE #creates a single html file
                )
