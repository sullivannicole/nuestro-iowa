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

options(tigris_use_cache = TRUE)

# Variables and descriptions
# To extract variable descriptions for a new year, uncomment lines below and change year:
# census_api_key(key_get("census_api"), install = TRUE) #only needs to be done once per machine

# Some state stats from Census data
ia_state <- map_df(c(acs_yr-10):acs_yr, function(yr) get_acs(geography = "state", variables = c("B01001I_001", "B01001_001"), state = "IA", year = yr) %>% mutate(year = yr) )
write_csv(ia_state, glue("data/ia_state_{acs_yr-10}_{acs_yr}.csv"))

vars_list <- ia %>%
  distinct(variable) %>%
  filter(substr(variable, 1, 7) %in%  c("B03003_", "B06004I", "B01002I", "B08105I", "B17020I", "B03001_"))

# County stats from Census data
# If new plots or tables need to be added, simply add the variable to the list in line 48
ia_counties <- get_acs(geography = "county",
              variables = vars_list$variable,
              state = "IA",
              year = acs_yr,
              geometry = TRUE)

# Select Latinx variables needed for dashboard
# Rejoin data with descriptions & create percentages
ia_data_labeled <- ia_counties %>% 
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
         label = str_replace_all(label, "Estimate!!|Total:|!!", ""))

st_write(ia_data_labeled, glue("data/ia_counties_{acs_yr}.shp"))


# -----------------
# Tract-level data
#------------------

ia_metro_tracts <- tigris::tracts(state = "Iowa", 
                                  county = c("Warren", "Dallas", "Jasper", "Guthrie", "Polk", "Marshall", "Madison"))

ia_metro_tracts_df <- ia_metro_tracts %>% tibble() %>% distinct(GEOID)


ia_metro_tract_data <- map_df(c(acs_yr-10):acs_yr, function(yr) {
  
  get_acs(geography = "tract", 
          variables = c(glue("B03001_00{1:3}"), "B01001I_001", "B01001_001"), 
          state = "IA", 
          year = yr,
          geometry = TRUE) %>% 
    mutate(year = yr)
  })

ia_metro_labeled <- ia_metro_tract_data %>%
  left_join(acs_vars, by = c("variable" = "name")) %>%
  inner_join(ia_metro_tracts_df, by = "GEOID") %>% # filter to metro tracts
  mutate(variable2 = variable,
         GEOID = as.character(GEOID)) %>%
  separate(variable2, into = c("variable_group", "variable_index"), sep = "_") %>%
  group_by(GEOID, year, variable_group) %>%
  mutate(denom = ifelse(variable_index == "001", estimate, NA),
         denom = max(denom, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = estimate/denom,
         percent = estimate/denom*100,
         county_name = str_replace_all(NAME, " County, Iowa", ""),
         label = str_replace_all(label, "Estimate!!|Total:|!!", ""))

st_write(ia_metro_labeled, glue("data/ia_metro_tracts_{acs_yr}.shp"))


