# Wrangling
library(glue)
library(tidyverse)
library(magrittr)
library(lubridate)

# Mapping
library(leaflet)
library(sf)

# Aesthetics
library(gfonts)
library(scales)

# Visualization
library(plotly)
# install.packages("ggchicklet", repos = "https://cinc.rud.is")
# library(ggchicklet)

# Interactivity
library(shiny)
library(argonR)
library(argonDash)
library(bs4Dash)
library(shinyWidgets)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinycustomloader)

# setwd("/Users/nicolesullivan/Documents/Professional/VandegriftHuting_consulting/nuestro-iowa/app")

# # Only needs to be done once to load fonts into www folder

# setup_font(
#   id = "karla",
#   output_dir = "www/"
# )

# Set-up
# 1. Place `eliminating_disparities.xlsx` for the year of choice in ETL/data
# 2. Set year and Census key in ETL/run_ETL.R (done once per acs_yr)
# 3. Run ETL/run_ETL.R (done once per acs_yr)
# 4. Run app

#-------------------------------------------
# Set year of extracts to pull for app use
#-------------------------------------------

acs_yr <<- 2019

#-------------------------------------------

# If pulling a new year, change acs_yr in ETL/run_ETL.R and re-run script
# New extracts will be saved in data folder and available for pulling into app

# # Aesthetics, data, constant UI elements
# for (i in list.files("R/elements", pattern = "[.]R$")) {
#   source(glue("R/elements/{i}", local = TRUE))
# }
# 
# # # UI
# for (i in list.files("R/ui", pattern = "[.]R$")) {
#   source(glue("R/ui/{i}", local = TRUE))
# }
# 
# # # Server
# for (i in list.files("R/server", pattern = "[.]R$")) {
#   source(glue("R/server/{i}", local = TRUE))
# }

#--------------
# Front-end
#--------------

ui <- argonDashPage(
  title = "Argon Dashboard Nuestro IA",
  author = "Latinx Iowa Project",
  description = "Argon Dash Nuestro IA",
  sidebar = argonSidebar,
  gfonts::use_font("karla", "www/css/karla.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  body = argonDashBody(
    argonTabItems(
      tab_1_alt_a,
      tab_2_alt_a,
      tab_3,
      tab_4
    )
  ),
  footer = argonFooter
)

#--------------
# Back-end
#--------------

server <- function(input, output, session) {
  
  nu_county_server(input, output, session)
  
  nu_comm_server(input, output, ssession)
  
  nu_future_server(input, output, session)
  
  
}

# App
shiny::shinyApp(
  ui = ui,
  server = server
)
