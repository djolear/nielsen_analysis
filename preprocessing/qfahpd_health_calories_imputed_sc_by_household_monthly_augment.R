sinfo <- data.frame(Sys.info())
machine <- sinfo$Sys.info..[4]

machine_path <- 
  ifelse(
    machine %in% c("sussman-rp-mbpro.local", "sussman-rp-mbpro.lan"), 
    "/Users/djolear/Google Drive/", 
    "G:/My Drive/"
  )


###################
## Load Packages ##
###################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven
)


###########################
## Load Helper Functions ##
###########################

source(paste0(machine_path, "research/projects/niel/nielsen_relative_income/preprocessing/join_county_census_data.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_relative_income/preprocessing/standardize_vars.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_relative_income/preprocessing/bind_chr_data.R"))


###################
## Main Function ##
###################

nielsen_read_add_secondary_write <- function(df_path, current_year, reference_group_incomes) {
  
  df <-
    read_csv(df_path)
  
  df <-
    df %>% 
    mutate(
      year = current_year
    ) %>% 
    mutate(
      state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
      fips_code = paste0(state_fips, cty_fips),
      Race = as.factor(Race)
    )
  
  df <- bind_county_census_data_function(df, current_year)
  df <- bind_chr(df, current_year)
  df <- standardize_vars_qfahpd_health(df)
  
  df <- 
    df %>% 
    left_join(
      reference_group_incomes %>% 
        dplyr::select(
          household_code,
          year,
          income_demo_ranger_sar_scale
        )
    )
  
  write_csv(df, paste0(data_path, "qh_calories_imputed_sc_by_household_monthly_wide_secondary_", current_year, ".csv"))
  
  return(df)
}


######################
## Run the Function ##
######################

# Set data path
data_path <- "D:/data/nielsen/calories_extracts/qfahpd_health_calories_imputed_sc_by_household_monthly/"


# Get list of file names
file_list <- 
  data.frame(
    file_list = list.files(path = data_path)
  )

file_list <-
  file_list %>% 
  filter(
    str_detect(file_list, ".csv")
  ) %>% 
  slice(1:16)


# Load reference group incomes
reference_group_incomes <-
  read_csv("D:/data/nielsen/ml/rf_income_predictions_default_070721.csv")

# Loop over years and munge data
for(i in 1:length(file_list$file_list)) {
  assign(
    paste0("qh_calories_imputed_sc_by_household_monthly_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    nielsen_read_add_secondary_write(
      paste0(data_path, file_list$file_list[i]), 
      as.numeric(str_extract(file_list$file_list[i], "[[:digit:]]+")),
      reference_group_incomes
    )
  )
  print(paste0(str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}


