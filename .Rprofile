options(repos = c(CRAN = "https://cran.rstudio.com/"), ggpattern_use_R4.1_features = TRUE)
library(utils)
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidytuesdayR, jsonlite, sf, installr, tidyverse, ggrepel, readxl, openxlsx, emmeans, multcomp, ggforce, rcompanion, RVAideMemoire, extrafont, patchwork, ragg, lemon, DescTools, tidylog, showtext, reshape2, here, languageserver, httpgd, ggpattern)

# install.Rtools()

#Vectors
c25 <- c("dodgerblue2", "#E31A1C", # red
                      "green4",
                      "#6A3D9A", # purple
                      "#FF7F00", # orange
                      "purple", "black",
                      "skyblue2", "#FB9A99", # lt pink
                      "palegreen2",
                      "#CAB2D6", # lt purple
                      "#FDBF6F", # lt orange
                      "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "lightblue4", "steelblue4",
                      "darkturquoise", "green1", "yellow4", "yellow3",
                      "darkorange4", "brown", "darkred")

european_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                        "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia",
                        "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland",
                        "France", "Georgia", "Germany", "Greece", "Hungary",
                        "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo",
                        "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
                        "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
                        "Norway", "Poland", "Portugal", "Romania", "Russia",
                        "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain",
                        "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom")



#VSCode settings
if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}

if (interactive() && Sys.getenv("RSTUDIO") == "") {
    Sys.setenv(TERM_PROGRAM = "vscode")
    if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
        httpgd::hgd(silent = TRUE)
         .vsc.browser(httpgd::hgd_url(history = TRUE), viewer = FALSE)
         })
    }
    source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))
}