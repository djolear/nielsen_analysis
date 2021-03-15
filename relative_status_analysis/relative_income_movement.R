library(tidyverse)
library(lme4)

df1 <-
  read_csv("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_2017.csv")

df2 <-
  read_csv("D:/data/nielsen/qfahpd_health_calories_imputed_sc_by_household_quarterly/qh_calories_imputed_sc_by_household_quarterly_wide_secondary_2016.csv")

df <-
  df1 %>% 
  left_join(
    df2,
    by = c("household_code", "quarter")
  )

movers <-
  df %>% 
  filter(fips_code.x != fips_code.y) 


movers_master <-
  bind_rows(
    movers_master,
    movers
  )

#movers_master <- movers
# 
# movers <-
#   df %>%
#   filter(zip.x != zip.y)
# 

# lm1 <-
#   lmer(
#     yes_scale.x ~
#       scale(I(median_income_county_scale.x - median_income_county_scale.y)) +
#       #median_income_county_scale.x +
#       median_income_county_scale.y +
#       median_monthly_housing_cost_county_scale.x +
#       median_monthly_housing_cost_county_scale.y +
#       yes_scale.y +
#       income_scale.x + 
#       income_scale.y +
#       (1|household_code) +
#       (1|quarter),
#     data =
#       df
#   )
# 
# 
# summary(lm1)

lm1 <-
  lmer(
    yes_scale.x ~
      scale(I(median_income_county_scale.x - median_income_county_scale.y)) +
      #median_income_county_scale.x +
      median_income_county_scale.y +
      median_monthly_housing_cost_county_scale.x +
      median_monthly_housing_cost_county_scale.y +
      yes_scale.y +
      income_scale.x +
      income_scale.y +
      Male_Head_Education_scale.y +
      Female_Head_Education_scale.y +
      Male_Head_Education_scale.x +
      Female_Head_Education_scale.x +

      Male_Head_Age_scale.y +
      Female_Head_Age_scale.y +

      land_area_2010_scale.y +
      total_pop_county_scale.y +
      land_area_2010_scale.x +
      total_pop_county_scale.x +

      Race.y +

      Male_Head_Employment.y +
      Female_Head_Employment.y +
      Marital_Status.y +
      Household_Size_scale.y +

      Male_Head_Employment.x +
      Female_Head_Employment.x +
      Marital_Status.x +
      Household_Size_scale.x +

      year.x +
      year.y +
      
      (1|household_code) +
      (1|quarter),
    data =
      movers_master
  )


summary(lm1)

lm1 <-
  lmer(
    yes_scale.x ~
      yes_scale.y +
      
      scale(I(median_income_county.x - median_income_county.y)) +
      median_income_county_scale.y +
      
      scale(I(median_monthly_housing_cost_county.x - median_monthly_housing_cost_county.y)) +
      median_monthly_housing_cost_county_scale.y +
      
      scale(I(income.x - income.y)) +
      income_scale.y +
      
      scale(I(Male_Head_Education.x - Male_Head_Education.y)) +
      Male_Head_Education_scale.y +
      
      scale(I(Female_Head_Education.x - Female_Head_Education.y)) +
      Female_Head_Education_scale.y +
      
      scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
      Female_Head_Age_scale.y +
      
      scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
      Female_Head_Age_scale.y +
      
      scale(I(total_pop_county.x - total_pop_county.y)) +
      total_pop_county_scale.y +
      
      scale(I(land_area_2010.x - land_area_2010.y)) +
      land_area_2010_scale.y +
      
      scale(I(Household_Size.x - Household_Size.y)) +
      Household_Size_scale.y +
      
      Race.y +
      
      Male_Head_Employment.y +
      Female_Head_Employment.y +
      Marital_Status.y +
      
      Male_Head_Employment.x +
      Female_Head_Employment.x +
      Marital_Status.x +

      year.x +
      year.y +
      
      (1|fips_code.x) +
      (1|fips_code.y) +
      (1|household_code) +
      (1|quarter),
    data =
      movers
  )


summary(lm1)


lm1 <-
  lmer(
    yes_scale.x ~
      yes_scale.y +
      
      scale(I(median_income_county_scale.x - median_income_county_scale.y)) +
      median_income_county_scale.y +
      
      scale(I(median_monthly_housing_cost_county_scale.x - median_monthly_housing_cost_county_scale.y)) +
      median_monthly_housing_cost_county_scale.y +
      
      scale(I(income_scale.x - income_scale.y)) +
      income_scale.y +
      
      scale(I(Male_Head_Education.x - Male_Head_Education.y)) +
      Male_Head_Education_scale.y +
      
      scale(I(Female_Head_Education.x - Female_Head_Education.y)) +
      Female_Head_Education_scale.y +
      
      scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
      Female_Head_Age_scale.y +
      
      scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
      Female_Head_Age_scale.y +
      
      scale(I(total_pop_county.x - total_pop_county.y)) +
      total_pop_county_scale.y +
      
      scale(I(land_area_2010.x - land_area_2010.y)) +
      land_area_2010_scale.y +

      scale(I(Household_Size.x - Household_Size.y)) +
      Household_Size_scale.y +
      
      Race.y +
      
      Male_Head_Employment.y +
      Female_Head_Employment.y +
      Marital_Status.y +

      Male_Head_Employment.x +
      Female_Head_Employment.x +
      Marital_Status.x +
      
      year.x +
      year.y +
      
      (1|fips_code.x) +
      (1|fips_code.y) +
      (1|household_code) +
      (1|quarter),
    data =
      movers
  )


summary(lm1)

# 
# df %>% 
#   filter(income.x == income.y) %>% 
#   count(med_inc_niel_avg.x  < med_inc_niel_avg.y)
# 
# 
lm1 <-
  lmer(
    yes_scale.x ~
      scale(I(med_inc_niel_avg.x - med_inc_niel_avg.y)) +
      med_inc_niel_avg_scale.y +

      yes_scale.y +
      

      scale(I(median_monthly_housing_cost_county_scale.x - median_monthly_housing_cost_county_scale.y)) +
      median_monthly_housing_cost_county_scale.y +
      
      scale(I(income_scale.x - income_scale.y)) +
      income_scale.y +
      
      scale(I(Male_Head_Education.x - Male_Head_Education.y)) +
      Male_Head_Education_scale.y +
      
      scale(I(Female_Head_Education.x - Female_Head_Education.y)) +
      Female_Head_Education_scale.y +
      
      scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
      Female_Head_Age_scale.y +
      
      scale(I(Female_Head_Age.x - Female_Head_Age.y)) +
      Female_Head_Age_scale.y +
      
      scale(I(total_pop_county.x - total_pop_county.y)) +
      total_pop_county_scale.y +
      
      scale(I(land_area_2010.x - land_area_2010.y)) +
      land_area_2010_scale.y +
      
      scale(I(Household_Size.x - Household_Size.y)) +
      Household_Size_scale.y +
      
      Race.y +
      
      Male_Head_Employment.y +
      Female_Head_Employment.y +
      Marital_Status.y +
      
      Male_Head_Employment.x +
      Female_Head_Employment.x +
      Marital_Status.x +
      
      year.x +
      year.y +
      
      (1|fips_code.x) +
      (1|fips_code.y) +
      (1|household_code) +
      (1|quarter),
    data =
      df 
  )


summary(lm1)
