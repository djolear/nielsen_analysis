sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

year = 2013

panelists <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))

df_wide_2013 <-
  read_csv(paste0(machine_path, "research/projects/niel/nielsen_data_output/group_spend_data/group_spend_by_household_wide_2013.csv"))

df_wide_2013 <-
  df_wide_2013 %>% 
  left_join(
    panelists %>% 
      dplyr::select(
        household_code = Household_Cd,
        Fips_State_Cd,
        Household_Composition
      )
  ) %>% 
  mutate(
    state_fips = ifelse(nchar(state_fips) == 1, paste0("0", state_fips), state_fips),
    cty_fips = ifelse(nchar(cty_fips) == 1, paste0("00", cty_fips), ifelse(nchar(cty_fips) == 2, paste0("0", cty_fips), cty_fips)),
    fips_code = paste0(state_fips, cty_fips)
  )

df_wide_2013 <- bind_county_census_data_function(df_wide_2013, 2013)


med_inc_niel <-
  panelists %>% 
  filter(Male_Head_Age > 0) %>% 
  group_by(Male_Head_Age, Male_Head_Education) %>% 
  summarise(
    med_inc_niel = median(Household_Income, na.rm = TRUE)
  ) %>% 
  ungroup() 

df_wide_2013 <-
  df_wide_2013 %>% 
  left_join(
    med_inc_niel
  )

npc14 <-
  npc14 %>% 
  left_join(
    med_inc_niel
  )

summary(lm(fresh_produce ~ income + med_inc_niel * Male_Head_Education, df_wide_2013))
summary(lm(sug_cal ~ Household_Income + med_inc_niel * Male_Head_Education, npc14))

library(tidycensus)

##########
## 2013 ##
##########

med_earn_2013 <- 
  get_acs(
    geography = "county",
    variables = 
      c(
        male_lths = "B20004_008",
        male_hs = "B20004_009",
        male_sc = "B20004_010",
        male_c = "B20004_011",
        male_g = "B20004_012",
        female_lths = "B20004_014",
        female_hs = "B20004_015",
        female_sc = "B20004_016",
        female_c = "B20004_017",
        female_g = "B20004_018"        
      ),
    year = 2013
  )

med_earn_2013 <-
  med_earn_2013 %>% 
  dplyr::select(
    fips_code = GEOID, 
    variable, 
    estimate
  ) %>% 
  separate(variable, into = c("sex", "education"), sep = "_") %>% 
  mutate(
    med_earn_census_cty = estimate,
    sex = 
      case_when(
        sex == "male" ~ 1,
        sex == "female" ~ 2
      ),
    education_new = 
      case_when(
        education == "lths" ~ 1,
        education == "hs" ~ 3,
        education == "sc" ~ 4,
        education == "c" ~ 5,
        education == "g" ~ 6,
      )
  ) %>% 
  dplyr::select(-c(education, estimate))

df_wide_2013 <-
  df_wide_2013 %>% 
  filter(
    Male_Head_Education > 0
  ) %>% 
  mutate(
    education_new = ifelse(Male_Head_Education == 2, 1, Male_Head_Education),
    state_fips = as.character(ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd))
  ) %>% 
  left_join(
    med_earn_2013 %>% filter(sex != 2)
  )

lm1 <- lm(fresh_produce ~ income + med_earn_census_cty * Male_Head_Education + median_monthly_housing_cost, df_wide_2013)
lm1 <- lm.beta::lm.beta(lm1)
summary(lm1)


lm1 <- lm(bread_and_baked_goods ~ income + med_earn_census * Male_Head_Education + median_home_value_14 + Race + Male_Head_Age + Male_Head_Employment, df_wide_2013_earn)
lm1 <- lm.beta::lm.beta(lm1)
summary(lm1)

sjPlot::plot_model(lm1, type = "pred", terms = c("med_earn_census", "Male_Head_Education"))


npc14_earn <-
  npc14_earn %>% 
  mutate(
    cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
    state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
    fips_code = paste0(state_fips, cty_fips)
  ) 

npc14_earn <-
  npc14_earn %>% 
  left_join(
    census_county_2013
  ) 

lm1 <- lm(add_sugar_cal ~ Household_Income + median_income_14 * Male_Head_Education + median_home_value_14+ Race + Male_Head_Age + Male_Head_Employment, npc14_earn)
lm1 <- lm.beta::lm.beta(lm1)
summary(lm1)
