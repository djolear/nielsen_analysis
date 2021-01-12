lm1 <- 
  lm(
    fresh_produce ~ 
      income + 
      #med_earn_census_cty_female * Female_Head_Education + 
      med_earn_census_cty_male * Male_Head_Education + 
      median_monthly_housing_cost_county +
      land_area_2010 +
      total_pop_county +
      household_size +
      as.factor(Household_Composition) +
      Race +
      Male_Head_Age +
      Female_Head_Age, 
    data = df_wide_2013
  )

lm1 <- lm.beta::lm.beta(lm1)

summary(lm1)
