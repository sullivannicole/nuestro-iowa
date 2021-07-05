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
library(tigris)

# options(tigris_use_cache = FALSE)

# Variables and descriptions
# census_api_key(key_get("census_api"), install = TRUE) #only needs to be done once per machine
# Pull all available variables
acs_vars <- load_variables(acs_yr, "acs5", cache = TRUE)

#--------
# State
#--------

get_state_data <- function(year) {
  
  get_acs(geography = "state", 
          variables = c("B01001I_001", "B01001_001", "B01002I_001"), 
          state = "IA", 
          year = year) %>% 
    mutate(year = year)
  
}

ia_state <- map_df(c(acs_yr-10):acs_yr, get_state_data)

write_csv(ia_state, glue("data/ia_state_{acs_yr-10}_{acs_yr}.csv"))

vars_list <- acs_vars %>%
  distinct(name) %>%
  filter(substr(name, 1, 7) %in% c("B01001_", "B03003_", "B06004I", "B01002I", 
                                   "B08105I", "B17020I", "B03001_", 
                                   "C27001I", "B01001I"))

# County stats from Census data
# If new plots or tables need to be added, simply add the variable to the list in line 48
ia_counties <- get_acs(geography = "county",
                       variables = vars_list$name,
                       state = "IA",
                       year = acs_yr,
                       geometry = FALSE)

# Select Latinx variables needed for dashboard
# Rejoin data with descriptions & create percentages
ia_counties_tidy <- ia_counties %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>%
  mutate(variable2 = variable,
         GEOID = as.character(GEOID)) %>%
  separate(variable2, into = c("variable_group", "variable_index"), sep = "_") %>%
  group_by(NAME, variable_group) %>%
  mutate(denom = ifelse(variable_index == "001", estimate, NA),
         denom = max(denom, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = estimate/denom,
         percent = estimate/denom*100,
         county_name = str_replace_all(NAME, " County, Iowa", ""),
         label = str_replace_all(label, "Estimate!!|Total:|!!", ""))

# st_write(ia_counties_tidy, glue("data/ia_counties_{acs_yr}.shp"))

# ia_county_shp <- counties("Iowa")
# st_write(ia_county_shp, "data/ia_county_shp.shp")

# county_shp <- st_read('tl_2020_us_county/tl_2020_us_county.shp')
# ia_county_shp <- county_shp %>% filter(STATEFP == 19)
# st_write(ia_county_shp, "data/ia_county_shp.shp")
ia_county_shp <- st_read("data/ia_county_shp.shp")

#-------------------------
# State & county combined
#-------------------------

state_compare <- ia_state %>% 
  filter(year == acs_yr) %>%
  select(NAME, estimate, variable) %>%
  mutate(NAME = paste0("State of ", NAME))

county_compare <- ia_counties_tidy %>% 
  tibble() %>%
  select(NAME, estimate, variable) %>%
  mutate(NAME = str_replace_all(NAME, " County, Iowa", ""))

state_county <- rbind(state_compare, county_compare)

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

ia_metro_tidy <- ia_metro_tract_data %>%
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
         label = str_replace_all(label, "Estimate!!|Total:|!!", "")) %>%
  separate(county_name, into = c("tract_desc", "county_name"), sep = ", ") %>%
  mutate(tract_and_county = paste(tract_desc, county_name, sep = ", "))

st_write(ia_metro_tidy, glue("data/ia_metro_tracts_{acs_yr}.shp"))

latinx_businesses <- tibble(Industry = c("Construction", "Other services", "Health care/social services", "Administrative support",
                                         "Retail trade", "Professional, scientific, technical", "Food services", "Transport & warehousing",
                                         "Arts & entertainment", "Real estate"),
                            Businesses = c(366, 216, 209, 153, 134, 124, 91, 73, 46, 40))

