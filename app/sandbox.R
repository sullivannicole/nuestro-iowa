# Pull all available variables
acs_vars <- load_variables(acs_yr, "acs5", cache = TRUE)

# Hispanic variables only
hispanic_vars <- acs_vars %>%
  distinct(name, concept) %>%
  filter(concept %like any% str_to_upper('%hispanic%')) %>%
  filter(!concept %like any% str_to_upper('%white alone%')) %>%
  distinct(name)

# Pull Hispanic data
ia <- get_acs(geography = "county",
              variables = hispanic_vars$name,
              state = "IA",
              year = acs_yr)

# Look at concepts
look_at_vars <- ia_data_labeled <- ia %>% 
  left_join(v19, by = c("variable" = "name")) %>%
  mutate(variable2 = variable,
         GEOID = as.character(GEOID)) %>%
  separate(variable2, into = c("variable_group", "variable_index"), sep = "_")
