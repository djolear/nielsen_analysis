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

add_secondary_and_save_two_person <- function(data_path, file_name, current_year, reference_group_incomes, file_name_stem) {
  
  df <-
    read_csv(paste0(data_path, "nielsen_only/", file_name))
  
  df <-
    df %>% 
    filter(
      Male_Head_Education != 0 & Female_Head_Education != 0
    )
  
  df <-
    df %>% 
    mutate(
      year = current_year
    ) %>% 
    mutate(
      state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
      fips_code = paste0(state_fips, cty_fips),
      zip = as.factor(Panelist_ZipCd),
      Race = as.factor(Race)
    ) %>% 
    dplyr::select(
      -c(Panelist_ZipCd:Fips_County_Cd)
    )
  
  df <- bind_county_census_data_function(df, current_year)
  df <- bind_chr(df, current_year)
  df <- standardize_vars_tfp(df)
  
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
  
  write_csv(df, paste0(data_path, "with_secondary_data/", file_name_stem, current_year, ".csv"))
  
  return(df)
}


