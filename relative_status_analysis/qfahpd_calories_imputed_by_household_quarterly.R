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


source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_join_median_income_nielsen_functions.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_join_census_data_county_functions.R"))
source(paste0(machine_path, "research/projects/niel/nielsen_analysis/relative_status_analysis/nielsen_standardize_vars_function.R"))

nielsen_read_add_secondary_write <- function(df_path, current_year) {
  
  panelists <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", current_year, "/Annual_Files/panelists_", current_year, ".tsv"))
  
  df <-
    read_csv(df_path)
  
  df <-
    df %>% 
    mutate(
      year = current_year
    ) %>% 
    left_join(
      panelists %>% 
        dplyr::select(
          household_code = Household_Cd,
          Fips_County_Cd,
          Fips_State_Cd,
          Household_Composition,
          Household_Size,
          Household_Composition,
          Male_Head_Age,
          Female_Head_Age,
          Male_Head_Employment,
          Female_Head_Employment,
          Male_Head_Education,
          Female_Head_Education,
          Marital_Status,
          Race
        )
    ) %>% 
    mutate(
      state_fips = ifelse(nchar(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      cty_fips = ifelse(nchar(Fips_County_Cd) == 1, paste0("00", Fips_County_Cd), ifelse(nchar(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd)),
      fips_code = paste0(state_fips, cty_fips)
    )
  
  df <- median_income_nielsen_all_function(df)
  df <- bind_county_census_data_function(df, current_year)
  
  df <- standardize_vars_qfahpd_health(df)
  
  write_csv(df, paste0(data_path, "qh_calories_imputed_by_household_quarterly_wide_secondary_", current_year, ".csv"))
  
  return(df)
}

data_path <- "D:/data/nielsen/qfahpd_health_calories_imputed_by_household_quarterly/"

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

for(i in 1:length(file_list$file_list)) {
  assign(
    paste0("qh_calories_imputed_by_household_quarterly_", str_extract(file_list$file_list[i], "[[:digit:]]+")), 
    nielsen_read_add_secondary_write(
      paste0(data_path, file_list$file_list[i]), 
      as.numeric(str_extract(file_list$file_list[i], "[[:digit:]]+"))
    )
  )
  print(paste0(str_extract(file_list$file_list[i], "[[:digit:]]+"), " complete."))
}
