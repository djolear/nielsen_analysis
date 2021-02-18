
###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("haven", lib.loc = "/home/djolear/R")
library("lubridate", lib.loc = "/home/djolear/R")

##############
## Set Path ##
##############

bind_nielsen_data_fn <- function(year) {
  purchase <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
   products_master <- 
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/nielsen_extracts/HMS/Master_Files/Latest/products.tsv"))
 
  niel_df <-
    purchase %>% 
    left_join(
      trips %>% 
        dplyr::select(
          trip_code_uc,
          household_code,
          total_spent,
          purchase_date 
        )
    )
  
  niel_df <-
    niel_df %>% 
    left_join(
      products_master %>% 
        dplyr::select(
          product_module_descr,
          product_group_descr,
          department_descr,
          size1_amount,
          size1_units,
          upc,
          upc_ver_uc
        )
    )
  
  rm(purchase, trips, products_master)
  
  return(niel_df)
}


group_spend_by_household_fn <- function(niel_df, year) {
  
  group_spending_data <-
    niel_df %>% 
    # filter(
    #   department_descr %!in% c("ALCOHOLIC BEVERAGES", "DAIRY", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT")
    # ) %>% 
    dplyr::select(
      household_code,
      total_spent,
      group = product_group_descr
    ) %>% 
    mutate(
      group = str_to_lower(group),
      group = str_replace_all(group, " ", "_"),
      group = str_replace_all(group, "-", "_"),
      group = str_remove_all(group, ","),
      group = str_replace_all(group, "___", "_"),
      group = str_replace_all(group, "__", "_")
    )
  
  group_spend_by_household <-
    group_spending_data %>% 
    group_by(household_code, group) %>% 
    summarise(
      group_total_spend = sum(total_spent)
    )
  
  total_spend_by_houshold <-
    group_spending_data %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_spent)
    )
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  group_spend_by_household <- 
    group_spend_by_household %>% 
    left_join(
      total_spend_by_houshold,
      by = "household_code"
    ) %>% 
    mutate(
      group_prop_spend = group_total_spend / total_spend
    ) 
  
  group_spend_by_household <- 
    group_spend_by_household %>% 
    dplyr::select(
      household_code, 
      group, 
      group_prop_spend
    )
  
  write_csv(group_spend_by_household, paste0(machine_path, "research/projects/niel/nielsen_data_output/group_spend_by_household_long_", year, ".csv"))
  print(paste0("Long format data saved for ", year, "."))
            
  group_spend_by_household_wide <-
    group_spend_by_household %>% 
    spread(group, group_prop_spend)
            
  rm(group_spend_by_household)
  
  group_spend_by_household_wide <-
    group_spend_by_household_wide %>% 
    left_join(
      panelists %>% 
        dplyr::select(
          household_code = Household_Cd,
          income = Household_Income,
          household_size = Household_Size,
          Male_Head_Age:Female_Head_Occupation,
          Marital_Status,
          Race,
          zip = Panelist_ZipCd,
          state_fips = Fips_State_Cd,
          cty_fips = Fips_County_Cd,
          Wic_Indicator_Current            
        ),
      by = "household_code"
    )
  
  write_csv(group_spend_by_household_wide, paste0("/home/djolear/nielsen/nielsen_data_output/group_spend_by_household_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
  return(0)
}

group_spend_by_household_food_only_fn <- function(niel_df, year) {
  
  group_spending_data <-
    niel_df %>%
    filter(
      department_descr %!in% c("ALCOHOLIC BEVERAGES", "DAIRY", "DRY GROCERY", "FRESH PRODUCE", "FROZEN FOODS", "PACKAGED MEAT")
    ) %>%
    dplyr::select(
      household_code,
      total_spent,
      group = product_group_descr
    ) %>% 
    mutate(
      group = str_to_lower(group),
      group = str_replace_all(group, " ", "_"),
      group = str_replace_all(group, "-", "_"),
      group = str_remove_all(group, ","),
      group = str_replace_all(group, "___", "_"),
      group = str_replace_all(group, "__", "_")
    )
  
  group_spend_by_household <-
    group_spending_data %>% 
    group_by(household_code, group) %>% 
    summarise(
      group_total_spend = sum(total_spent)
    )
  
  total_spend_by_houshold <-
    group_spending_data %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_spent)
    )
  
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  group_spend_by_household <- 
    group_spend_by_household %>% 
    left_join(
      total_spend_by_houshold,
      by = "household_code"
    ) %>% 
    mutate(
      group_prop_spend = group_total_spend / total_spend
    ) 
  
  group_spend_by_household <- 
    group_spend_by_household %>% 
    dplyr::select(
      household_code, 
      group, 
      group_prop_spend
    )
  
  # write_csv(group_spend_by_household, paste0(machine_path, "research/projects/niel/nielsen_data_output/group_spend_by_household_long_", year, ".csv"))
  # print(paste0("Long format data saved for ", year, "."))
  
  group_spend_by_household_wide <-
    group_spend_by_household %>% 
    spread(group, group_prop_spend)
  
  rm(group_spend_by_household)
  
  group_spend_by_household_wide <-
    group_spend_by_household_wide %>% 
    left_join(
      panelists %>% 
        dplyr::select(
          household_code = Household_Cd,
          income = Household_Income,
          household_size = Household_Size,
          Male_Head_Age:Female_Head_Occupation,
          Marital_Status,
          Race,
          zip = Panelist_ZipCd,
          state_fips = Fips_State_Cd,
          cty_fips = Fips_County_Cd,
          Wic_Indicator_Current            
        ),
      by = "household_code"
    )
  
  write_csv(group_spend_by_household_wide, paste0("/home/djolear/nielsen/nielsen_data_output/group_spend_by_household_food_only_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
  return(0)
}



main_group_spend_by_household_fn <- function(year){
  niel_df <- bind_nielsen_data_fn(year)
  group_spend_by_household_food_only_fn(niel_df, year)
}

years <- seq(2004, 2017, 1)

# for(i in 1:length(years)) {
#   main_group_spend_by_household_fn(years[i])
# }
plan(multisession, workers = 8)

future_map(.x = years, .f = main_group_spend_by_household_fn)

