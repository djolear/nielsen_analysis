
###################
## Load Packages ##
###################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  lubridate,
  readr
)


##############
## Set Path ##
##############

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


#########
## ... ##
#########


source(paste0(machine_path, "research/projects/niel/nielsen_analysis/calculate_nutrition_per_cal_fn.R"))

year = "2017"

npc_2017 <- 
  nutrition_per_cal_fn(year)

npc_2017 <-
  bind_zip_code_census_data_function(npc_2017, year)  


year = "2016"

npc_2016 <- 
  nutrition_per_cal_fn(year)

npc_2016 <-
  bind_zip_code_census_data_function(npc_2016, year)  


year = "2015"

npc_2015 <- 
  nutrition_per_cal_fn(year)

npc_2015 <-
  bind_zip_code_census_data_function(npc_2015, year)  


year = "2014"

npc_2014 <- 
  nutrition_per_cal_fn(year)

npc_2014 <-
  bind_zip_code_census_data_function(npc_2014, year)  


year = "2013"

npc_2013 <- 
  nutrition_per_cal_fn(year)

npc_2013 <-
  bind_zip_code_census_data_function(npc_2013, year)  



npc <-
  npc_2013 %>%
  dplyr::select(
    household_code,
    zip_13 = zip,
    produce_cal_13 = produce_cal,
    income_13 = inc_mid,
    inc_diff_13 = inc_diff,
    median_income_13 = median_income,
    mmhc_13 = median_monthly_housing_cost,
    pov_status_below_per_13 = pov_status_below_per,
    Household_Size_13 = Household_Size,
    Male_Head_Education_13 = Male_Head_Education, 
    Female_Head_Education_13 = Female_Head_Education, 
    Race_13 = Race, 
    Marital_Status_13 = Marital_Status, 
    Male_Head_Employment_13 = Male_Head_Employment, 
    Female_Head_Employment_13 = Female_Head_Employment, 
    Male_Head_Age_13 = Male_Head_Age, 
    Female_Head_Age_13 = Female_Head_Age 
  ) %>% 
  full_join(
    npc_2014 %>%
      dplyr::select(
        household_code,
        zip_14 = zip,
        produce_cal_14 = produce_cal,
        income_14 = inc_mid,
        inc_diff_14 = inc_diff,
        median_income_14 = median_income,
        mmhc_14 = median_monthly_housing_cost,
        pov_status_below_per_14 = pov_status_below_per,
        Household_Size_14 = Household_Size,
        Male_Head_Education_14 = Male_Head_Education, 
        Female_Head_Education_14 = Female_Head_Education, 
        Race_14 = Race, 
        Marital_Status_14 = Marital_Status, 
        Male_Head_Employment_14 = Male_Head_Employment, 
        Female_Head_Employment_14 = Female_Head_Employment, 
        Male_Head_Age_14 = Male_Head_Age, 
        Female_Head_Age_14 = Female_Head_Age 
      ),
    by = "household_code"
  ) %>% 
  full_join(
    npc_2015 %>%
      dplyr::select(
        household_code,
        zip_15 = zip,
        produce_cal_15 = produce_cal,
        hei_mod_15 = hei_mod,
        income_15 = inc_mid,
        inc_diff_15 = inc_diff,
        median_income_15 = median_income,
        mmhc_15 = median_monthly_housing_cost,
        pov_status_below_per_15 = pov_status_below_per,
        Household_Size_15 = Household_Size,
        Male_Head_Education_15 = Male_Head_Education, 
        Female_Head_Education_15 = Female_Head_Education, 
        Race_15 = Race, 
        Marital_Status_15 = Marital_Status, 
        Male_Head_Employment_15 = Male_Head_Employment, 
        Female_Head_Employment_15 = Female_Head_Employment, 
        Male_Head_Age_15 = Male_Head_Age, 
        Female_Head_Age_15 = Female_Head_Age 
      ),
    by = "household_code"
  ) %>% 
  full_join(
    npc_2016 %>%
      dplyr::select(
        household_code,
        zip_16 = zip,
        produce_cal_16 = produce_cal,
        hei_mod_16 = hei_mod,
        income_16 = inc_mid,
        inc_diff_16 = inc_diff,
        median_income_16 = median_income,
        mmhc_16 = median_monthly_housing_cost,
        pov_status_below_per_16 = pov_status_below_per,
        Household_Size_16 = Household_Size,
        Male_Head_Education_16 = Male_Head_Education, 
        Female_Head_Education_16 = Female_Head_Education, 
        Race_16 = Race, 
        Marital_Status_16 = Marital_Status, 
        Male_Head_Employment_16 = Male_Head_Employment, 
        Female_Head_Employment_16 = Female_Head_Employment, 
        Male_Head_Age_16 = Male_Head_Age, 
        Female_Head_Age_16 = Female_Head_Age 
      ),
    by = "household_code"
  ) %>%
  full_join(
    npc_2017 %>%
      dplyr::select(
        household_code,
        zip_17 = zip,
        produce_cal_17 = produce_cal,
        hei_mod_17 = hei_mod,
        income_17 = inc_mid,
        inc_diff_17 = inc_diff,
        median_income_17 = median_income,
        mmhc_17 = median_monthly_housing_cost,
        pov_status_below_per_17 = pov_status_below_per,
        Household_Size_17 = Household_Size,
        Male_Head_Education_17 = Male_Head_Education, 
        Female_Head_Education_17 = Female_Head_Education, 
        Race_17 = Race, 
        Marital_Status_17 = Marital_Status, 
        Male_Head_Employment_17 = Male_Head_Employment, 
        Female_Head_Employment_17 = Female_Head_Employment, 
        Male_Head_Age_17 = Male_Head_Age, 
        Female_Head_Age_17 = Female_Head_Age 
      ),
    by = "household_code"
  ) %>% 
  mutate(
    produce_diff_15_to_16 = produce_cal_16 - produce_cal_15,
    hei_mod_diff_15_to_16 = hei_mod_16 - hei_mod_15,
    inc_diff_chg_15_to_16 = inc_diff_16 - inc_diff_15,
    
    produce_diff_16_to_17 = produce_cal_17 - produce_cal_16,
    hei_mod_diff_16_to_17 = hei_mod_17 - hei_mod_16,
    inc_diff_chg_16_to_17 = inc_diff_17 - inc_diff_16
  )
# 
# npc <- 
#   npc %>% 
#   mutate(
#     inc_chg = inc_mid.y - inc_mid.x,
#     inc_diff_chg = inc_diff.y - inc_diff.x,
#     med_inc_chg = median_income.y - median_income.x,
#     produce_chg = produce_cal.y - produce_cal.x,
#     as_chg = add_sugar_cal.y - add_sugar_cal.x,
#   )
# 
# 
lm_med_inc <-
  lmer(
    produce_diff_15_to_17 ~
      produce_cal_15 +
      median_income_15 + 
      inc_diff_chg_15_to_17 * income_15 + 
      mmhc_15 +
      pov_status_below_per_15 +
      Household_Size_15 +
      Male_Head_Education_15 +
      Female_Head_Education_15 +
      Race_15 +
      Marital_Status_15 +
      Male_Head_Employment_15 +
      Female_Head_Employment_15 +
      Male_Head_Age_15 +
      Female_Head_Age_15 +
      (1|zip_15),
    data = 
      npc 
  )

summary(lm_med_inc)

lm.beta.lmer(lm_med_inc)

lm_med_inc <-
  lmer(
    produce_cal_14 ~
      produce_cal_13 +
      median_income_13 + 
      I(median_income_14 - median_income_13) + income_13 + 
      mmhc_13 +
      pov_status_below_per_13 +
      Household_Size_13 +
      Male_Head_Education_13 +
      Female_Head_Education_13 +
      Race_13 +
      Marital_Status_13 +
      Male_Head_Employment_13 +
      Female_Head_Employment_13 +
      Male_Head_Age_13 +
      Female_Head_Age_13 +
      (1|zip_13),
    data = npc %>% filter(income_13 == income_14)
  )

summary(lm_med_inc)

lm.beta.lmer(lm_med_inc)



# lm_med_inc <-
#   lmer(
#     add_sugar_cal.y ~
#       add_sugar_cal.x +
#       inc_mid.x +
#       median_monthly_housing_cost.y + 
#       pov_status_below_per.y +
#       Household_Size.y +
#       Male_Head_Education.y +
#       Female_Head_Education.y +
#       Race.y +
#       Marital_Status.y +
#       Male_Head_Employment.y +
#       Female_Head_Employment.y +
#       Male_Head_Age.y +
#       Female_Head_Age.y +
#       (1|zip.y), 
#     data = npc %>% filter(med_inc_chg == 0)
#   )
# 
# summary(lm_med_inc)
# 
# lm.beta.lmer(lm_med_inc)
# 
# sjPlot::plot_model(lm_med_inc, type = "pred", terms = "med_inc_chg")
