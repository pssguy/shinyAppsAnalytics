
## check for packages needed for people running off github
packages <- c("markdown", "shiny", "shinydashboard", "stringr", "dplyr", "tidyr", "DT", "readr", "plotly")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# rsconnect is not on CRAN

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/rsconnect")

library(markdown)
library(shiny)
library(shinydashboard)

library(stringr)
library(dplyr) 

library(tidyr)
library(DT)
library(readr)
library(plotly)
library(rsconnect)




