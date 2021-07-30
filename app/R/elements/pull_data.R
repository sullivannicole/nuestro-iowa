library(tidyverse)
library(glue)

# Run ETL/run_ETL.R once to create data extracts for a certain year
# (2019 already is run and in the ETL/data folder)

setwd("Documents/Professional/VandegriftHuting_consulting/nuestro-iowa/app/ETL/data")
# setwd("app/ETL/data")

# Get Hispanic-related variables
# hispanic_vars <- read_csv(glue("data/acs5_{acs_yr}_hispanic_variables.csv"))

#-------------
# State data
#-------------

ia_state <- read_csv(glue("ia_state_{acs_yr-10}_{acs_yr}.csv"))

#--------------
# County data
#--------------

ia_counties_tidy <- read_csv(glue("ia_counties_{acs_yr}.csv"))

# Over time - done separately for a small subset of variables
# Many variables pulled for 2019 don't exist in years prior and so temporal pull error'ed out
# So these two are done in separate pulls
ia_counties_temporal_tidy <- read_csv(glue("ia_counties_{acs_yr}_{acs_yr-10}.csv"))

# Spatial data for aesthetic purposes
ia_county_shp <- st_read("ia_county_shp.shp")

#-------------
# Tract data
#-------------

ia_metro_tracts_tidy <- st_read(glue("data/ia_metro_tracts_data_{acs_yr}.shp"))

#-----------------
# Disparity data
#-----------------

disparities <- read_csv(glue("ia_disparities_{acs_yr}.csv"))
