required.packages <- 
  c("shiny" = TRUE, "shinyFiles" = TRUE, "shinydashboard" = TRUE, "DT" = TRUE, 
    "data.table" = TRUE,  "tidyverse" = TRUE, "dtplyr" = TRUE, "devtools" = FALSE,
    "leaflet" = TRUE, "leaflet.minicharts" = TRUE, "plotly" = TRUE, 
    "openxlsx" = FALSE, "scales" = FALSE, "fishmethods" = FALSE, "viridis" = FALSE,
    "mapview" = FALSE, "DBI" = FALSE, "duckdb" = TRUE, "bsplus" = TRUE) ## TRUE means that the package should be loaded. FALSE that the functions are used without loading the package (by refering ::)

new.packages <- names(required.packages)[!(names(required.packages) %in% installed.packages()[,"Package"])]
if (length(new.packages) > 0) install.packages(new.packages)

sapply(names(required.packages)[required.packages], require, character.only = TRUE)

#### RstoxData
# (not loaded, use pointer when using the functions)

if (!"RstoxData" %in% installed.packages()[,"Package"]) {
  install.packages("RstoxData", repo="https://stoxproject.github.io/repo")
}
