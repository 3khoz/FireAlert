setwd("c:/Users/Michelle1/Documents/GitHub/FireAlert")
library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
render("AlertaIncendios_27022018.Rmd")
