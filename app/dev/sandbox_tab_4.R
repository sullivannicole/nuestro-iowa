library(tidyverse)
library(lubridate)
library(plotly)

#----------------------
# Starting data
#----------------------

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
                          # prob_of_home_given_edu_inc_citizen = c(0.5123, 0.598, 0.682, 0.722))

total_undoc <- 8252

#----------------------
# Inputs
#----------------------

# # Education strategy
# Set the max of these as the max of each group
hs_max <- current_edu$n_latinx[current_edu$level == "lt_hs"]
hs_to_assoc_max <- current_edu$n_latinx[current_edu$level == "hs"]/2
assoc_to_bach_max <- current_edu$n_latinx[current_edu$level == "assoc"]
hs_to_bach_max <- current_edu$n_latinx[current_edu$level == "hs"]/2

hs_grads <- 50
hs_to_assoc <- 50
assoc_to_bach <- 50
hs_to_bach <- 1000

undoc_to_citizen <- "Yes"

#----------------------
# Outputs
#----------------------

edu_df <- current_edu %>%
  mutate(edu_after_invest = case_when(level == "lt_hs" ~ n_latinx - hs_grads,
                                      level == "hs" ~ n_latinx + hs_grads - hs_to_assoc - hs_to_bach,
                                      level == "assoc" ~ n_latinx + hs_to_assoc - assoc_to_bach,
                                      level == "bach" ~ n_latinx + assoc_to_bach + hs_to_bach),
         change_in_n = n_latinx - edu_after_invest,
         pc_attain = edu_after_invest/sum(edu_after_invest)*100)

# Attainment
hs_increase <- hs_grads * 2644
associates_increase <- hs_to_assoc * 6093
bachelors_increase <- (assoc_to_bach * 14807) + (hs_to_bach * 20900)
undoc_increase <- if (undoc_to_citizen == "Yes") total_undoc * 5659 else 0

# Income (total and per capita)
total_increase <- hs_increase + associates_increase + bachelors_increase + undoc_increase
total_dollars_earned <- 996866128 + total_increase
per_capita_income <- total_dollars_earned/51618
total_investment_value <- ((total_increase * (1.02)^30 - total_increase)/1.9662316753) * 30 + (total_increase * 30)
total_residents_affected <- if (undoc_to_citizen == "Yes") total_undoc + hs_grads + hs_to_assoc + assoc_to_bach + hs_to_bach else hs_grads + hs_to_assoc + assoc_to_bach + hs_to_bach
per_capita_income <- total_dollars_earned/51618
per_person_value_30_yrs <- total_investment_value/total_residents_affected

# Homeownership
homeowner_df <- current_hhs %>%
  mutate(edu_after_invest = case_when(level == "lt_hs" ~ n_latinx - hs_grads,
                                      level == "hs" ~ n_latinx + hs_grads - hs_to_assoc - hs_to_bach,
                                      level == "assoc" ~ n_latinx + hs_to_assoc - assoc_to_bach,
                                      level == "bach" ~ n_latinx + assoc_to_bach + hs_to_bach),
         change_in_edu = n_latinx - edu_after_invest,
         prob_of_home_bach_lt_hs =  prob_of_home_bach - prob_of_home_lt_hs,
         change_in_home_given_edu = case_when(level == "lt_hs" ~ 0,
                                              level == "hs" ~ (prob_of_home_hs - prob_of_home_lt_hs) * hs_grads,
                                              level == "assoc" ~ (prob_of_home_assoc - prob_of_home_lt_hs) * hs_to_assoc,
                                              level == "bach" ~ (prob_of_home_bach_lt_hs * assoc_to_bach) + (prob_of_home_bach_lt_hs * hs_to_bach) ),
         current_homeowners = sum(n_homeowners),
         current_renters = sum(n_latinx - n_homeowners),
         addtl_homeowners = sum(change_in_home_given_edu),
         homeowners_with_investments = current_homeowners + addtl_homeowners,
         renters_with_investments = sum(n_latinx) - homeowners_with_investments,
         homeowners_base = homeowners_with_investments/(homeowners_with_investments + renters_with_investments),
         homeownership_rate = ifelse(undoc_to_citizen == "Yes" , (homeowners_base + 0.087) * 100, homeowners_base * 100)) # 0.087 = undoc boost

ho_rate <- unique(homeowner_df$homeownership_rate)

#----------------------
# Plots
#----------------------

race_ethnicity_vctr <- c("Latinx", "White")

test <- expand_grid(race_ethnicity = race_ethnicity_vctr,
            edu_level = factor(c("< HS", "HS", "Associate's", "Bachelor's"), levels = c("< HS", "HS", "Associate's", "Bachelor's"))) %>%
  mutate(attainment = c(edu_df$pc_attain, 4.8, 46.5, 11, 37.7))

# Educational attainment bar
expand_grid(race_ethnicity = race_ethnicity_vctr,
           edu_level = factor(c("< HS", "HS", "Associate's", "Bachelor's"), levels = c("< HS", "HS", "Associate's", "Bachelor's"))) %>%
  mutate(attainment = c(edu_df$pc_attain, 4.8, 46.5, 11, 37.7),
         text = paste0(edu_level, attainment)) %>%
  ggplot(aes(race_ethnicity, attainment, fill = edu_level, text = text)) +
  geom_bar(stat = "identity", width = 0.15) +
  scale_fill_manual(values = c(hex_pink, hex_purple, hex_blue_lt, hex_green)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Educational Attainment",
       x = "",
       y = "",
       fill = "") +
  project_ggtheme_y

# Homeownership bar

plot_ho <- data.frame(race_ethnicity = race_ethnicity_vctr,
           tenure = c(rep("Homeowners", 2), rep("Renters", 2)),
           pc = c(ho_rate, 73.7, 100 - ho_rate, 26.3)) %>%
  ggplot(aes(race_ethnicity, pc, fill = tenure)) +
  geom_bar(stat = "identity", width = 0.1) +
  scale_fill_manual(values = c(hex_blue_lt, hex_blue_dk)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Homeownership rates",
       x = "",
       y = "",
       fill = "") +
  project_ggtheme_y

ggplotly(plot_ho)

# Per capita income bar
plot_per_cap_income <- data.frame(race_ethnicity = race_ethnicity_vctr,
           income = c(per_capita_income, 39812),
           text) %>%
  ggplot(aes(race_ethnicity, income/1000, text = text)) +
  geom_bar(stat = "identity", width = 0.1, fill = hex_purple) +
  labs(title = "Per Capita Income (in $1K)",
       x = "",
       y = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  project_ggtheme_y

ggplotly(plot_per_cap_income)

# Total addtl dollars per year line graph
yrs_out <- 30
total_per_year <- c(total_increase, total_increase * cumprod(rep(1.02, yrs_out)))
total_yrly_df <- data.frame(year = seq.Date(ymd("2021-01-01"), ymd("2021-01-01") + years(yrs_out), by = "years"),
                            total_addtl = total_per_year) %>%
  mutate(text = paste(year, "hello"))

# Interpolate the measured points with a spline to produce a nice curve:
spline_df   <- as.data.frame(spline(year(total_yrly_df$year), total_yrly_df$total_addtl, n = 200, method = "nat")) %>%
  mutate(year = date_decimal(x)) %>%
  slice(-c(1, 200)) %>%
  rename(total_addtl = y)

# # A data frame to produce a gradient effect over the filled area:
# grad_df <- data.frame(yint = seq(0, max(total_yrly_df$total_addtl/1000000), length.out = 200), 
#                       alpha = seq(0.3, 0, length.out = 200))


plot_addtl_dollars <- 
  ggplot(total_yrly_df, aes(year, total_addtl/1000000, group = 1, text = text)) +
  geom_line(color =  hex_purple, size = 1.2) +
  geom_ribbon(aes(ymin = 0, ymax = total_addtl/1000000), alpha = 0.2, fill = hex_purple) +
  labs(title = "Additional Earnings (in $M)",
       y = "",
       x = "") 
  # scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

ggplotly(plot_addtl_dollars, tooltip = "text")

  
  
  
  

