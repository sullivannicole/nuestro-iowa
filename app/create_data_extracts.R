#----------------------------------
# Set ACS year to pull Census data
#----------------------------------

acs_yr <- 2019

#-----------------------------------

library(keyring) # For Census key
library(tidyverse)
library(tidycensus)
library(glue)
library(DescTools)
library(sf)

# Variables and descriptions
# To extract variable descriptions for a new year, uncomment lines below and change year:
# census_api_key(key_get("census_api"), install = TRUE) #only needs to be done once per machine
acs_vars <- load_variables(acs_yr, "acs5", cache = TRUE)

# Some state stats from Census data
ia_state <- map_df(c(acs_yr-10):acs_yr, function(yr) get_acs(geography = "state", variables = c("B01001I_001", "B01001_001"), state = "IA", year = yr) %>% mutate(year = yr) )
write_csv(ia_state, glue("data/ia_state_{acs_yr-10}_{acs_yr}.csv"))

# County stats from Census data
# Note that all Hispanic-related variables are imported for flexibility
# If new plots or tables need to be added, simply add the variable to the list in line 48
ia <- get_acs(geography = "county",
              variables = c("B03003", "B06004I", "B01002I", "B08105I", "B17020I", "B03001"),
              state = "IA",
              year = acs_yr)

# Select Latinx variables needed for dashboard
# Rejoin data with descriptions & create percentages
ia_data_labeled <- ia %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>%
  mutate(variable2 = variable,
         GEOID = as.character(GEOID)) %>%
  separate(variable2, into = c("variable_group", "variable_index"), sep = "_") %>%
  filter(variable_group %in% c("B03003", "B06004I", "B01002I", "B08105I", "B17020I", "B03001")) %>%
  group_by(NAME, variable_group) %>%
  mutate(denom = ifelse(variable_index == "001", estimate, NA),
         denom = max(denom, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = estimate/denom,
         percent = estimate/denom*100,
         county_name = str_replace_all(NAME, " County, Iowa", ""),
         label = str_replace_all(label, "Estimate!!|Total:|!!", "")) %>%
  select(-NAME)

write_csv(ia_data_labeled, glue("data/ia_county_{acs_yr}_tidied.csv"))

# Pull shapefile for mapping data and write to data folder
ia_shp <- tigris::counties(state = "Iowa")
st_write(ia_shp, dsn = "data/ia_counties_map.shp")



