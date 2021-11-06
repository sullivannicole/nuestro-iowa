#----------------------------------------------------
# Description: Extract, transform and load all data:
# ACS, spatial, and disparity data
# Do not run this script directly
# Set settings in run_ETL.R and run run_ETL.R
#----------------------------------------------------
# Pull data
library(tidycensus)
library(RSocrata) # State of Iowa data
library(readxl)
library(httr)
library(keyring)

# Data wrangling
library(glue)
library(DescTools)
library(lubridate)
library(tidyverse)

# Spatial tools
library(sf)
library(tigris)
library(janitor)

# options(tigris_use_cache = FALSE)

# Variables and descriptions - pull all available variables
acs_vars <- load_variables(acs_yr, "acs5", cache = TRUE)

#--------
# State
#--------

get_state_data <- function(year) {
  
  get_acs(geography = "state", 
          variables = c("B01001I_001", "B01001_001", "B01002I_001", "B19013I_001"), 
          state = "IA", 
          survey = "acs1",
          year = year) %>% 
    mutate(year = year)
  
}

ia_state <- map_df(c(acs_yr-10):acs_yr, get_state_data)

write_csv(ia_state, glue("ia_state_{acs_yr-10}_{acs_yr}.csv"))

# If new plots or tables need to be added, simply add the variable to the list below
vars_list <- acs_vars %>%
  distinct(name) %>%
  filter(substr(name, 1, 7) %in% c(# Demographics & hh-level
                                   "B01001_", "B01001I", "B01002I", "B01002_", "B12002I", "B11001I", "B10051I", "B16006_",
                                   
                                   # Latinx birth/migration
                                   "B03003_", "B06004I", "B03001_",
                                   
                                   # Employment, income, poverty status
                                   "B20005I", "B19013I", "B19013_", "B17020I", 
                                   
                                  # Means of transportation
                                  "B08105I",
                                   
                                   # Homeownership
                                   "B25003I",
                                   
                                   # Education
                                   "B14007I", "C15010I", "C15002I", "B28009I",
                                  
                                  # Health insurance
                                  "C27001I"))

# County stats from Census data
ia_counties <- get_acs(geography = "county",
                       variables = vars_list$name,
                       state = "IA",
                       survey = "acs5",
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
         label = str_replace_all(label, "Estimate!!|Total:|!!", "")) %>%
  group_by(county_name, variable_group) %>%
  
  # Convert MOE's to %s using the method outlined in the ACS handbook
  # https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
  mutate(denom_moe = ifelse(variable_index == "001", moe, NA)) %>%
  tidyr::fill(denom_moe, .direction = "down") %>%
  ungroup() %>%
  mutate(moe_pc = ifelse(percent == 0, NA, 100*(1/denom)*sqrt(moe^2 - (percent/100)^2*denom_moe^2)))

write_csv(ia_counties_tidy, glue("ia_counties_{acs_yr}.csv"))
# st_write(ia_counties_tidy, glue("data/ia_counties_{acs_yr}.shp"))

ia_county_shp <- counties("Iowa")
st_write(ia_county_shp, "ia_county_shp.shp")

#------------------------
# County data over time
#------------------------

# Some variables selected above were not disaggregated by race/ethnicity prior to 2017
# So pull only all races, white alone, and Hispanic
race_suffixes <- c('_', 'A', 'I')
temporal_vars <- c(  
  # Homeownership
  "B25003",
  
  # Income
  "B19013",
  
  # Employment
  "B20005",
  
  # Poverty
  # "B17020",
  
  # Educational attainment
  "C15002"
)

temp_vars_list <- map(temporal_vars, function(x) glue("{x}{race_suffixes}")) %>% unlist()

temporal_vars <- acs_vars %>%
  distinct(name) %>%
  filter(substr(name, 1, 7) %in% temp_vars_list)

get_county_data <- function(year) {
  
  get_acs(geography = "county",
          variables = temporal_vars$name,
          state = "IA",
          survey = "acs5",
          year = year,
          geometry = FALSE) %>%
    mutate(year = year)
  
}

ia_counties_temporal <- map_df(c(acs_yr-10):acs_yr, get_county_data)

# Getting unhelpful error message when I try to pull poverty over time
# looks to be due to how tidycensus pulls vars
# so pull separately

poverty_vars <- acs_vars %>%
  filter(substr(name, 1, 7) %in% c("B17020_", "B17020A", "B17020I"))

get_county_poverty <- function(year) {
  
  get_acs(geography = "county",
          variables = poverty_vars$name,
          state = "IA",
          survey = "acs5",
          year = year,
          geometry = FALSE) %>%
    mutate(year = year)
  
}

ia_counties_pov_temporal <- map_df(c(acs_yr-6):acs_yr, get_county_poverty)

# Select Latinx variables needed for dashboard
# Rejoin data with descriptions & create percentages
ia_counties_temporal_tidy <- ia_counties_temporal %>%
  rbind(ia_counties_pov_temporal) %>%
  left_join(acs_vars, by = c("variable" = "name")) %>%
  mutate(variable2 = variable,
         GEOID = as.character(GEOID)) %>%
  separate(variable2, into = c("variable_group", "variable_index"), sep = "_") %>%
  group_by(NAME, variable_group, year) %>%
  mutate(denom = ifelse(variable_index == "001", estimate, NA),
         denom = max(denom, na.rm = T)) %>%
  ungroup() %>%
  mutate(prop = estimate/denom,
         percent = estimate/denom*100,
         county_name = str_replace_all(NAME, " County, Iowa", ""),
         label = str_replace_all(label, "Estimate!!|Total:|!!", ""))

write_csv(ia_counties_temporal_tidy, glue("ia_counties_{acs_yr-10}_{acs_yr}.csv"))

#-------------------------
# Pull from data.iowa.gov
#-------------------------

library(RSocrata)
library(keyring)

# key_set("data.iowa_api_secret")
# key_set("data.iowa_password")
# key_set("data.iowa_app_token")

# Code pulling HS graduation rates
ia_hs_grads <- read.socrata(
  "https://data.iowa.gov/resource/xc4x-jnyq.json",
  app_token = key_get("data.iowa_app_token"),
  email     = "nicasull@gmail.com",
  password  = key_get("data.iowa_password")
)

ia_school_districts <- read.socrata(
  "https://data.iowa.gov/resource/xnu4-jzps.json",
  app_token = key_get("data.iowa_app_token"),
  email     = "nicasull@gmail.com",
  password  = key_get("data.iowa_password")
  
)

county_df <- counties("Iowa")

county_fips <- county_df %>%
  select(COUNTYFP, NAME) %>%
  st_set_geometry(NULL) %>%
  arrange(NAME) %>%
  mutate(county = row_number())

# 2010-2020 grad rates
docs <-
  c("distGRFRL%20and%20race_ethnicity.xls",
    "Iowa%20Public%20School%20Class%20of%202011%20-%204yr%20Cohort%20Graduation%20Data%20by%20LEA%20and%20Subgroup.xls",
    "Class%202012%204%20Year%20GR%20District%20Subgroup.xls",
    "Iowa%20Public%20High%20School-Class%20of%202013-4%20Year%20Graduation%20Data%20by%20District%20and%20Subgroup.xls",
    "Iowa%20Public%20High%20School%20Class%20of%202014-4%20Year%20Graduation%20Data%20by%20District%20and%20Subgroup.xls",
    "Iowa%20Public%20High%20School%2C%20Class%20of%202015%2C%204%20Year%20Graduation%20Data%20by%20District%20and%20Subgroup_0.xlsx",
    "2016GraduationRatesbyDistrictbySubgroup.xlsx",
    "gr4_district-16-17.xlsx",
    "Graduation%20Rates%20by%20district.xlsx",
    "gr4bydistrict2019.xlsx",
    "gr4bydistrict2020.xlsx")

grad_rates <- map2(docs, c(2010:2020), function(x, y) {
  
  # Get extension
  ext <- str_replace(x, "^.+\\.", "")
  
  GET(glue("https://educateiowa.gov/sites/files/ed/documents/{x}"), 
      write_disk(tf <- tempfile(fileext = ext)))
  
  # Create column names
  tf_head <- read_excel(tf) %>%
    rename(first_col = 1) %>%
    mutate(row_ind = ifelse(first_col %like% "%ounty%", 1, NA)) %>%
    fill(row_ind, .direction = "up") %>%
    filter(!is.na(row_ind)) %>%
    select(-row_ind)
  
  col_names <- tf_head %>%
    mutate(id = row_number()) %>%
    slice((n()-1):n()) %>%
    gather(everything(), key = "col", value = "value", -id) %>%
    mutate(race_ethnicity = ifelse(value %in% c("White",
                                                "Hispanic", 
                                                "American Indian", 
                                                "African American", 
                                                "Two or More Races",
                                                "Asian"), value, NA)) %>%
    fill(race_ethnicity, .direction = "down") %>%
    mutate(value = paste(value, race_ethnicity, sep = "_")) %>%
    filter(id == max(id))
  
  read_excel(tf, col_names = col_names$value, skip = nrow(tf_head)) %>%
    clean_names() %>%
    select(contains("county"), contains("aea"), contains("district"), contains("hispanic"), contains("white")) %>%
    mutate(year = y)
  
  
})

hs_grad_rates <- grad_rates %>%
  bind_rows() %>%
  filter(!(county_na %in% c("county", "County"))) %>%
  mutate(county = as.numeric(county_na)) %>%
  left_join(county_fips, by = "county") %>%
  rename(Latinx = rate_hispanic,
         White = rate_white) %>%
  select(NAME, year, Latinx, White) %>%
  gather(Latinx, White, key = "race_ethnicity", value = "rate") %>%
  mutate(rate = as.numeric(ifelse(rate %like% "%\\*%", NA, rate))) %>%
  group_by(NAME, race_ethnicity, year) %>%
  summarize(average_rate = mean(rate, na.rm = T)) %>%
  ungroup() %>%
  mutate(year = lubridate::ymd(year, truncated = 2L),
         text = paste(race_ethnicity, ": ", round(average_rate, 1), "%"))

write_csv(hs_grad_rates, "ia_county_hs_rates_2010_2020.csv")

#-------------------------
# State & county combined
#-------------------------

# state_compare <- ia_state %>% 
#   filter(year == acs_yr) %>%
#   select(NAME, estimate, variable) %>%
#   mutate(NAME = paste0("State of ", NAME))
# 
# county_compare <- ia_counties_tidy %>% 
#   tibble() %>%
#   select(NAME, estimate, variable) %>%
#   mutate(NAME = str_replace_all(NAME, " County, Iowa", ""))
# 
# state_county <- rbind(state_compare, county_compare)

# -----------------
# Zip code data
#------------------

library(zipcodeR)

metro_zips <- metro_zips <- purrr::map_df(c( "Dallas", "Jasper", "Marshall", "Polk", "Warren"), 
                                          function(i) zipcodeR::search_county(i, "IA"))

write_csv(metro_zips %>% select(zipcode, county), glue("ETL/data/ia_metro_zips_{acs_yr}.csv"))

metro_zips_shp <- tigris::zctas(cb = FALSE, starts_with = unique(substr(metro_zips$zipcode, 1, 3)))

sf::st_write(metro_zips_shp, glue("data/ia_metro_zips_{acs_yr}.shp"))

# -----------------
# Tract-level data
#------------------

ia_metro_tracts <- tigris::tracts(state = "Iowa", 
                                  county = c("Warren", "Dallas", "Jasper", "Guthrie", "Polk", "Marshall", "Madison"))

ia_metro_tracts_df <- ia_metro_tracts %>% tibble() %>% distinct(GEOID)


ia_metro_tract_data <- purrr::map_df(c(acs_yr-10):acs_yr, function(yr) {
  
  get_acs(geography = "tract", 
          variables = c(glue("B03001_00{1:3}"), "B01001I_001", "B01001_001"), 
          state = "IA", 
          year = yr,
          survey = "acs5",
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

# latinx_businesses <- tibble(Industry = c("Construction", "Other services", "Health care/social services", "Administrative support",
#                                          "Retail trade", "Professional, scientific, technical", "Food services", "Transport & warehousing",
#                                          "Arts & entertainment", "Real estate"),
#                             Businesses = c(366, 216, 209, 153, 134, 124, 91, 73, 46, 40))


#-------------------------------
# Eliminating disparities data
#-------------------------------

disparities <- read_excel("eliminating_disparities.xlsx") %>%
  clean_names() %>%
  select(-c(x7, x14, x15, x18, year_8, year_11, latinx_dollars_earned_9, dollars_earned_eliminate_disparities_10)) %>%
  rename(year = year_1,
         latinx_dollars_earned = latinx_dollars_earned_5,
         dollars_earned_eliminate_disparities = dollars_earned_eliminate_disparities_6)

write_csv(disparities, glue("ia_disparities_{acs_yr}.csv"))
