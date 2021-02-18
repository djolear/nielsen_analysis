
###################
## Load Packages ##
###################

library("tidyverse", lib.loc = "/home/djolear/R")
library("haven", lib.loc = "/home/djolear/R")
library("lubridate", lib.loc = "/home/djolear/R")
library("doParallel", lib.loc = "/home/djolear/R")
library("foreach", lib.loc = "/home/djolear/R")

###############
## Functions ##
###############

'%!in%' <- function(x,y)!('%in%'(x,y))


######################
## Helper Functions ##
######################

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/bind_nielsen_to_label_insight_fn.R"))

calories_per_group_fn <- function(df){
  panelists <-
    readr::read_tsv(paste0("/kilts/nielsen_extracts/HMS/", year, "/Annual_Files/panelists_", year, ".tsv"))
  
  niel_df_calories <-
    df  %>% 
    filter(!is.na(calories))
  
  total_calories_by_household <-
    niel_df_calories %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_calories = sum(calories, na.rm = TRUE)
    )
  
  group_calories_by_household <-
    niel_df_calories %>% 
    group_by(household_code, group, month) %>% 
    summarise(
      group_total_calories = sum(calories, na.rm = TRUE)
    )
  
  group_calories_by_household <- 
    group_calories_by_household %>% 
    left_join(
      total_calories_by_household,
      by = c("household_code", "month")
    ) %>% 
    mutate(
      group_prop_calories = group_total_calories / total_calories
    ) 
  
  group_calories_by_household <- 
    group_calories_by_household %>% 
    dplyr::select(
      household_code, 
      group, 
      month,
      group_prop_calories
    )

  group_calories_by_household <-
    group_calories_by_household %>% 
    spread(group, group_prop_calories)
  
  rm(group_calories_by_household)
  
  group_calories_by_household_wide <-
    group_calories_by_household_wide %>% 
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
  
  write_csv(group_calories_by_household_wide, paste0("/home/djolear/nielsen/nielsen_data_output/group_calories_by_household_monthly_wide_", year, ".csv"))
  print(paste0("Wide format data saved for ", year, "."))
  
}


main_group_calories_by_household_fn <- function(year){
  niel_df <- bind_labels_to_purchases_fn(year)
  calories_per_group_fn(niel_df, niel_df$year[1])
}

years <- seq(2004, 2017, 1)

# for(i in 1:length(years)) {
#   main_group_calories_by_household_fn(years[i])
# }

cores = 4
cl <- makeCluster(4) 
registerDoParallel(cl)

foreach(i = 1:length(years), .packages = c("tidyverse", "lubridate")) %dopar% {
  main_group_calories_by_household_fn(years[i])
  gc()
}
#stop cluster
stopCluster(cl)