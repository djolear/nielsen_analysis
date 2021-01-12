## Load Pacakges ##

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven,
  lme4,
  lm.beta,
  sjPlot,
  stargazer,
  psych,
  tidycensus
)


## set path ##

sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"),
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )

## functions ##


source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_join_median_earnings_census_functions.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_join_median_income_nielsen_functions.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_join_census_data_county_functions.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_standardize_vars_bev_survey_function.R"))


## Load Data ##

survey <- 
  read_csv(paste0(machine_path, "research/projects/niel/bev_survey.csv")) %>% 
  mutate(year = 2017)

# Load all panelists data

year = "2017"

panelists <-
  readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))

panelists <-
  panelists %>% 
  mutate(
    inc_mid = 
      case_when(
        Household_Income == 3 ~ 2500,
        Household_Income == 4 ~ 6500,
        Household_Income == 6 ~ 9000,
        Household_Income == 8 ~ 11000,
        Household_Income == 10 ~ 13500,
        Household_Income == 11 ~ 17500,
        Household_Income == 13 ~ 22500,
        Household_Income == 15 ~ 27500,
        Household_Income == 16 ~ 32500,
        Household_Income == 17 ~ 37500,
        Household_Income == 18 ~ 42500,
        Household_Income == 19 ~ 47500,
        Household_Income == 21 ~ 55000,
        Household_Income == 23 ~ 65500,
        Household_Income == 26 ~ 85000,
        Household_Income == 27 ~ 100000
      )
  )


# Join panelists data to survey data

survey <-
  survey %>% mutate(household_code = ID) %>% 
  left_join(
    panelists %>% mutate(household_code = Household_Cd, income = Household_Income)
  )



## Join county-level census data and create fips codes ##

# census_ses_county_wide <- 
#   read_csv(paste0(machine_path, "research/projects/secondary_data/census/census_ses_county_wide_", year, ".csv"))

survey <-
  survey %>%
  mutate(
    cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
    state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
    fips_code = paste0(state_fips, cty_fips)
  ) 


## Add median income variables

survey <- median_income_nielsen_all_function(survey)
survey <- median_earnings_all_census_function(survey)
survey <- bind_county_census_data_function(survey, 2017)
survey <- standardize_vars(survey)

