library(tidycensus)

##########
## 2017 ##
##########

med_earn_2017 <- 
  get_acs(
    geography = "state",
    variables = 
      c(
        male_lths = "B20004_008",
        male_hs = "B20004_009",
        male_sc = "B20004_010",
        male_c = "B20004_011",
        male_g = "B20004_012",
        female_lths = "B20004_017",
        female_hs = "B20004_017",
        female_sc = "B20004_016",
        female_c = "B20004_017",
        female_g = "B20004_018"        
      ),
    year = 2017
  )

med_earn_2017 <-
  med_earn_2017 %>% 
  dplyr::select(
    state_fips = GEOID, 
    variable, 
    estimate
  ) %>% 
  separate(variable, into = c("sex", "education"), sep = "_") %>% 
  mutate(
    med_earn_census = estimate,
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

df_wide_2017 <-
  df_wide_2017 %>% 
  filter(
    Male_Head_Education > 0
  ) %>% 
  mutate(
    education_new = ifelse(Male_Head_Education == 2, 1, Male_Head_Education),
    state_fips = as.character(ifelse(nchar(state_fips) == 1, paste0("0", state_fips), state_fips))
  ) %>% 
  left_join(
    med_earn_2017 %>% filter(sex != 2)
  )


## Other Census Data ##

census_data_path <- "research/projects/secondary_data/census/"

census_county_2017 <-
  read_csv(paste0(machine_path, census_data_path, "census_ses_county_wide_2017.csv")) %>% 
  mutate(year = 2017) %>% 
  dplyr::select(
    fips_code,
    year,
    median_income_17 = median_income,
    median_monthly_housing_cost_17 = median_monthly_housing_cost,
    median_home_value_17 = median_home_value,
    pov_status_below_per_17 = pov_status_below_per,
    gini_17 = gini,
    unweighted_pop_17 = unweighted_pop,
    employ_status_civ_lf_unemployed_per_17 = employ_status_civ_lf_unemployed_per,
    ea_bach_or_higher_per_17 = ea_bach_or_higher_per
  )

df_wide_2017_earn <-
  df_wide_2017 %>% 
  mutate(
    cty_fips = ifelse(nchar(cty_fips) == 1, paste0("00", cty_fips), ifelse(nchar(cty_fips) == 2, paste0("0", cty_fips), cty_fips)),
    fips_code = paste0(state_fips, cty_fips)
  ) 

df_wide_2017_earn <-
  df_wide_2017_earn %>% 
  left_join(
    census_county_2017
  ) 
