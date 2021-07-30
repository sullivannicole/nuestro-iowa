library(keyring) # For Census key
library(tidyverse)
library(tidycensus)

#----------------------------------
# Set ACS year, Census key and WD
# to pull Census data
#----------------------------------

acs_yr <- 2019
# census_api_key(key_get("census_api"), install = TRUE) #only needs to be done once per machine
setwd("/Users/nicolesullivan/Documents/Professional/VandegriftHuting_consulting/nuestro-iowa/app/ETL/data")

#-----------------------------------

source("ETL.R")

