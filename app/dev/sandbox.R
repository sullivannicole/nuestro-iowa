library(DescTools)
library(readxl)
library(janitor)
library(lubridate)

# This script is used to explore ACS variables and
# add relevant variables to the create_data_extracts.R script

# Pull all available variables
acs_vars <- load_variables(acs_yr, "acs5", cache = TRUE)

# Hispanic variables only
hispanic_vars <- acs_vars %>%
  separate(name, into = c("variable_group", "variable_index"), sep = "_") %>%
  distinct(variable_group, concept) %>%
  filter(concept %like any% str_to_upper('%hispanic%')) %>%
  filter(!concept %like any% str_to_upper('%white alone%'))

hispanic_vars_pull <- acs_vars %>%
  distinct(variable_group, concept) %>%
  filter(concept %like any% str_to_upper('%hispanic%')) %>%
  filter(!concept %like any% str_to_upper('%white alone%'))

# Pull Hispanic data
ia <- get_acs(geography = "county",
              variables = hispanic_vars$name,
              state = "IA",
              year = acs_yr)

# Look at concepts
look_at_vars <- ia %>%
  # tibble() %>%
  # select(-geometry) %>%
  left_join(acs_vars %>% distinct(name, label, concept), by = c("variable" = "name")) %>%
  distinct(variable, label, concept) %>%
  mutate(variable2 = variable) %>%
  separate(variable2, into = c("variable_group", "variable_index"), sep = "_")

metro_county_list <- c("Warren", "Dallas", "Jasper", "Polk", "Guthrie", "Marshall", "Madison")

# New plots

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

library(readxl)
library(httr)

# 2010-2020 graad rates
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

grad_rates_df <- grad_rates %>%
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
  mutate(year = lubridate::ymd(year, truncated = 2L))

grad_rates_df %>%
  filter(NAME == "Madison") %>%
  ggplot(aes(year, average_rate, color = race_ethnicity, group = race_ethnicity)) +
  geom_line()

county_df <- counties("Iowa")

county_fips <- county_df %>%
  select(COUNTYFP, NAME) %>%
  st_set_geometry(NULL) %>%
  arrange(NAME) %>%
  mutate(county = row_number())

# Code pulling businesses & predicting race by surname
# Realized registered agent is likely NOT the owner
# and therefore the classification isn't useful

# ia_businesses <- read.socrata(
#   "https://data.iowa.gov/resource/ez5t-3qay.json",
#   app_token = key_get("data.iowa_app_token"),
#   email     = "nicasull@gmail.com",
#   password  = key_get("data.iowa_password")
# )
# 
# ia_businesses_unnested <- ia_businesses %>%
#   unnest(ra_location.coordinates) %>%
#   group_by(corp_number) %>%
#   mutate(ra_location_coords = paste(ra_location.coordinates, collapse = ", ")) %>%
#   ungroup() %>%
#   select(-ra_location.coordinates) %>%
#   unique() %>%
#   unnest(ho_location.coordinates) %>%
#   group_by(corp_number) %>%
#   mutate(ho_location_coords = paste(ho_location.coordinates, collapse = ", ")) %>%
#   select(-ho_location.coordinates) %>%
#   unique()
# 
# write_csv(ia_businesses_unnested, glue("app/data/ia_businesses_{Sys.Date()}.csv"))
# 
# ia_businesses_unnested <- read_csv("app/data/ia_businesses_2021-06-04.csv")
# 
# library(stringr)
# 
# ia_businesses_surname <- ia_businesses_unnested %>%
#   mutate(ra = trimws(str_replace_all(registered_agent, "I|II|III|IV|INC|JR.", "")),
#         surname = word(ra,-1))
# 
# library(wru)
# 
# # tigris::blocks("ia", county = "Polk")
# 
# test_df <- data.frame(surname = c("Sanchez", "Sullivan", "Perez"),
#                       state = rep("ia", 3),
#                       county = rep("153", 3),
#                       tract = rep("011021", 3),
#                       block = c("1012", "2009", "1038"))
# 
# census_ia <- get_census_data(key = key_get("census_api"), 
#                              state = "IA",
#                              age = FALSE, 
#                              sex = FALSE)
# 
# saveRDS(census_ia, "app/data/wru_census_ia.RDS")
# 
# test_preds <- predict_race(
#   test_df,
#   census.surname = TRUE,
#   surname.only = FALSE,
#   surname.year = 2010,
#   census.geo = "block",
#   census.data = census_ia,
#   # census.key = key_get("census_api"),
#   age = FALSE,
#   sex = FALSE,
#   retry = 0
# )

# Explore data on disabled Hispanic Iowans

ia_blind <- read.socrata(
  "https://data.iowa.gov/resource/twt2-zx5z.json",
  app_token = key_get("data.iowa_app_token"),
  email     = "nicasull@gmail.com",
  password  = key_get("data.iowa_password")
)

ia_blind_tidied <- ia_blind %>%
  filter(hispanic == "Hispanic") %>%
  group_by(federal_fiscal_year, gender) %>%
  count()

ia_blind_professions <- ia_blind %>%
  filter(hispanic == "Hispanic")

# Pandemic relief
ia_pandemic_relief <- read.socrata(
  "https://data.iowa.gov/resource/bqr9-xnsz.json",
  app_token = key_get("data.iowa_app_token"),
  email     = "nicasull@gmail.com",
  password  = key_get("data.iowa_password")
)

ia_relief_latinx <- ia_pandemic_relief %>%
  filter(raceethnicity %in% c("Hispanic", "Puerto Rican"))

ia_relief_latinx_summary <- ia_relief_latinx %>%
  mutate(jobsreported = as.numeric(jobsreported)) %>%
  group_by(gender) %>%
  summarize(jobs_saved = sum(jobsreported, na.rm = T))

ia_shp %>%
  mutate(input_status = ifelse(NAME == "Dallas", "1", "0")) %>%
  ggplot(aes(fill = input_status)) +
  geom_sf(color = "white") +
  scale_fill_manual(values = c("lightgrey", "#5E72E4")) +
  # labs(title = glue("{input$county_choice}\n\n")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#5E72E4"),
        legend.position = "none")

#-------------------------
# Eliminating disparities
#-------------------------

disparities <- read_excel("data/eliminating_disparities.xlsx") %>%
  clean_names() %>%
  select(-c(x7, x14, x15, x18, year_8, year_11, latinx_dollars_earned_9, dollars_earned_eliminate_disparities_10)) %>%
  rename(year = year_1,
         latinx_dollars_earned = latinx_dollars_earned_5,
         dollars_earned_eliminate_disparities = dollars_earned_eliminate_disparities_6)

income_df <- disparities %>%
  filter(year >= 2021) %>%
  select(year, per_capita_income, per_capita_income_eliminating_disparities) %>%
  mutate(per_capita_income_eliminating_disparities = per_capita_income + 
           (per_capita_income_eliminating_disparities-per_capita_income)*0.9)

ggplot(income_df, aes(year, per_capita_income_eliminating_disparities)) +
  geom_line(aes(y = per_capita_income), color = hex_pink, size = 1.2) +
  geom_line(aes(y = per_capita_income_eliminating_disparities), color = hex_green, size = 1.2) +
  geom_ribbon(aes(ymin = per_capita_income, ymax = per_capita_income_eliminating_disparities), alpha = 0.2, fill = hex_green) +
  theme(legend.position = "bottom") +
  labs(y = "Per capita income (USD)\n") +
  ylim(10000, 75000) +
  # scale_x_continuous(limits = c(2021, 2050), expand = c(0, 0)) +
  project_ggtheme

# Interpolate the measured points with a spline to produce a nice curve:
income_disparities <- disparities %>%
  filter(year >= 2021) %>%
  select(year, per_capita_income, per_capita_income_eliminating_disparities) %>%
  mutate(per_capita_income_eliminating_disparities = per_capita_income_eliminating_disparities*0.25) %>%
  gather(2:3, key = "future", value = "per_capita_income") %>%
  filter(future == "per_capita_income") %>%
  mutate(date = ymd(year, truncated = 2L))

spline_df   <- spline(income_disparities$year, 
                      income_disparities$per_capita_income, 
                      n = 200, method = "nat") %>%
  as.data.frame() %>%
  rename(per_capita_income = y) %>%
  mutate(date = format(date_decimal(x), "%m-%d-%Y"),
         date = as_date(mdy(date)))

# A data frame to produce a gradient effect over the filled area:
grad_df <- data.frame(yintercept = seq(0, 8, length.out = 200), 
                      alpha = seq(0.3, 0, length.out = 200))

ggplot(income_disparities, aes(date, per_capita_income)) +
  geom_hline(aes(yintercept = 2), alpha = 0.02) +
  geom_area(data = spline_df, fill = "#80C020", alpha = 0.35) + 
  geom_hline(data = grad_df, aes(yintercept = yintercept, alpha = alpha),
             size = 2.5, colour = "white") +
  geom_line(data = spline_df, colour = "#80C020", size = 1) +
  geom_point(shape = 21, size = 2, color = "#80C020", fill = "white", stroke = 1.6) +
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_blank(),
        axis.line.x        = element_line(),
        text               = element_text(size = 15),
        plot.margin        = margin(unit(c(20, 20, 20, 20), "pt")),
        axis.ticks         = element_blank(),
        axis.text.y        = element_text(margin = margin(0,15,0,0, unit = "pt"))) +
  scale_alpha_identity() +
  scale_y_continuous(expand = c(0, 0))
