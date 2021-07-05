library(tidyverse)
library(glue)

# Run create_data_extracts.R once to create data extracts for a certain year 
# (2019 already is run and in the data folder)
# This script gets run automatically by the app to pull in data

#-------------------------------------------
# Set year of extracts to pull for app use
#-------------------------------------------

acs_yr <- 2019

#-------------------------------------------

# setwd("Documents/Professional/VandegriftHuting_consulting/nuestro-iowa/app/")
setwd("app")

# Get Hispanic-related variables
# hispanic_vars <- read_csv(glue("data/acs5_{acs_yr}_hispanic_variables.csv"))

#-------------
# State data
#-------------

ia_state <- read_csv(glue("data/ia_state_{acs_yr-10}_{acs_yr}.csv"))

#--------------
# County data
#--------------

# Data is included in shapefile
ia_counties_tidy <- st_read(glue("data/ia_counties_{acs_yr}.shp"))

#-------------
# Tract data
#-------------

ia_metro_tracts_tidy <- st_read(glue("data/ia_metro_tracts_data_{acs_yr}.shp"))
