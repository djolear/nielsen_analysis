
lm0 <-
  lm(
    yes_scale ~
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale +
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Male_Head_Employment +
      Female_Head_Employment +
      Marital_Status +
      Household_Size_scale + 
      quarter + 
      year,
    data = nielsen_dollar_budget %>% filter(!is.na(med_inc_niel_avg_scale))
  )


lm1 <-
  lm(
    yes_scale ~
      med_inc_niel_avg_scale +
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale +
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Male_Head_Employment +
      Female_Head_Employment +
      Marital_Status +
      Household_Size_scale + 
      quarter + 
      year,
    data = nielsen_dollar_budget %>% filter(!is.na(med_inc_niel_avg_scale))
  )

summary(lm1)

tidy_lm1 <- tidy(lm1)


lm2 <-
  lm(
    yes_scale ~
      median_income_county_scale +
      income_scale +
      Male_Head_Education_scale +
      Female_Head_Education_scale +
      Male_Head_Age_scale +
      Female_Head_Age_scale +
      Male_Head_Employment +
      Female_Head_Employment +
      median_monthly_housing_cost_county_scale +
      land_area_2010_scale +
      total_pop_county_scale +
      Race +
      Male_Head_Employment +
      Female_Head_Employment +
      Marital_Status +
      Household_Size_scale + 
      quarter + 
      year,
    data = nielsen_dollar_budget %>% filter(!is.na(median_income_county_scale))
  )

summary(lm2)

