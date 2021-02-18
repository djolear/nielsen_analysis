total_calories <-
  niel_df %>% 
  group_by(household_code) %>% 
  summarise(
    total_calories = sum(calories, na.rm = TRUE),
    num_na = sum(is.na(calories))
  )

total_calories <-
  total_calories %>% 
  left_join(
    panelists %>% mutate(household_code = Household_Cd)
  )

total_calories <- total_calories %>% mutate(zip = Panelist_ZipCd)

total_calories <- bind_zip_code_census_data_function(total_calories, year)

summary(lmer(total_calories ~ num_na + median_income + median_monthly_housing_cost + inc_mid + Household_Size + (1|zip), total_calories))
