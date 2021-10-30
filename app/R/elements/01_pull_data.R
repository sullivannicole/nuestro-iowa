library(tidyverse)
library(glue)
library(sf)

# Run ETL/run_ETL.R once to create data extracts for a certain year
# (2019 already is run and in the ETL/data folder)

# setwd("ETL/data")
# setwd("app/ETL/data")

# Get Hispanic-related variables
# hispanic_vars <- read_csv(glue("data/acs5_{acs_yr}_hispanic_variables.csv"))

#-------------
# State data
#-------------

ia_state <- read_csv(glue("ETL/data/ia_state_{acs_yr-10}_{acs_yr}.csv"))

#--------------
# County data
#--------------

ia_counties_tidy <- read_csv(glue("ETL/data/ia_counties_{acs_yr}.csv"))

# Over time - done separately for a small subset of variables
# Many variables pulled for 2019 don't exist in years prior and so temporal pull error'ed out
# So these two are done in separate pulls
ia_counties_temporal_tidy <- read_csv(glue("ETL/data/ia_counties_{acs_yr-10}_{acs_yr}.csv"))

# Spatial data for aesthetic purposes
ia_county_shp <- st_read("ETL/data/ia_county_shp.shp")

# HS grad rates by county
# Comes from a different data source (IA Dept. of Education)
hs_grad_rates <- read_csv("ETL/data/ia_county_hs_rates_2010_2020.csv")

#-------------
# Zip code data
#-------------

ia_metro_zips <- read_csv(glue("ETL/data/ia_metro_zips_{acs_yr}.csv"))

ia_metro_zips_shp <- sf::st_read(glue("ETL/data/ia_metro_zips_{acs_yr}.shp"))

#-------------
# Tract data
#-------------

ia_metro_tidy <- st_read(glue("ETL/data/ia_metro_tracts_{acs_yr}.shp"))

#-----------------
# Disparity data
#-----------------

disparities <- read_csv(glue("ETL/data/ia_disparities_{acs_yr}.csv"))

#-------------------------------
# Nuestro Future data
#-------------------------------

current_edu <- data.frame(level = c("lt_hs", "hs", "assoc", "bach"),
                          n_latinx = c(10064, 13738, 1292, 3361))

current_hhs <- data.frame(level = c("lt_hs", "hs", "assoc", "bach"),
                          n_latinx = c(4327, 5961, 531, 1875),
                          n_homeowners = c(1914, 2724, 335, 1147),
                          n_undoc = c(2248, 1148, 29, 85),
                          prob_of_home_lt_hs = 0.4255,
                          prob_of_home_hs = 0.5119, 
                          prob_of_home_assoc = 0.602, 
                          prob_of_home_bach = 0.6447)

race_ethnicity_vctr <- c("Latinx", "White")
