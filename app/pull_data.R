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

# Get state data
ia_state <- read_csv(glue("data/ia_state_{acs_yr-10}_{acs_yr}.csv"))

# Get transformed county dataset for plots
ia_data_labeled <- read_csv(glue("data/ia_county_{acs_yr}_tidied.csv")) %>%
  mutate(GEOID = as.character(GEOID))

# Create list of county names for dropdown selection on tab 2
county_names <- ia_data_labeled %>%
  distinct(county_name)

# Pull shapefile for mapping data
ia_shp <- st_read("data/ia_counties_map.shp")

# # Join shapefile to data
ia_shp_data <- ia_shp %>%
  left_join(ia_data_labeled, by = "GEOID")
  