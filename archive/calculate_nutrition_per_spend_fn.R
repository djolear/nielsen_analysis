
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


#########################
## Load Auxiliary Data ##
#########################



######################
## Helper Functions ##
######################

source(paste0(machine_path, "research/projects/niel/nielsen_analysis/bind_nielsen_to_label_insight_fn.R"))

calories_per_spend_fn <- function(df){
  niel_df_calories <-
    df  %>% 
    filter(!is.na(calories))
  
  total_calories <-
    niel_df_calories %>% 
    group_by(household_code) %>% 
    summarise(
      total_calories = sum(calories, na.rm = TRUE)
    )
  
  spend <-
    niel_df_calories %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_price_paid , na.rm = TRUE)
    )
  
  total_calories <-
    total_calories %>% 
    left_join(
      spend
    )
  
  total_calories <-
    total_calories %>% 
    mutate(
      cal_per = total_calories / total_spend
    )  
  
  return(total_calories)
  
}

sugar_per_spend_fn <- function(df){
  niel_df_sugar <-
    df %>% 
    filter(!is.na(sugar))
  
  total_sugar <-
    niel_df_sugar %>% 
    group_by(household_code) %>% 
    summarise(
      total_sugar = sum(sugar, na.rm = TRUE)
    )
  
  spend <-
    niel_df_sugar %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_price_paid , na.rm = TRUE)
    )
  
  total_sugar <-
    total_sugar %>% 
    left_join(
      spend
    )
  
  total_sugar <-
    total_sugar %>% 
    mutate(
      sug_per = total_sugar / total_spend
    )  
  
  return(total_sugar)
  
}

saturated_fat_per_spend_fn <- function(df){
  niel_df_saturated_fat <-
    df  %>% 
    filter(!is.na(saturated_fat))
  
  total_saturated_fat <-
    niel_df_saturated_fat %>% 
    group_by(household_code) %>% 
    summarise(
      total_saturated_fat = sum(saturated_fat, na.rm = TRUE)
    )
  
  spend <-
    niel_df_saturated_fat %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_price_paid , na.rm = TRUE)
    )
  
  total_saturated_fat <-
    total_saturated_fat %>% 
    left_join(
      spend
    )
  
  total_saturated_fat <-
    total_saturated_fat %>% 
    mutate(
      sf_per = total_saturated_fat / total_spend
    )  
  
  return(total_saturated_fat)
}


###################
## Main Function ##
###################

nutrition_per_spend_fn <- function(year){
  
  df <- bind_labels_to_purchases_fn(year)
  
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
  
  
  total_calories <- calories_per_spend_fn(df)
  total_sugar <- sugar_per_spend_fn(df)
  total_saturated_fat <- saturated_fat_per_spend_fn(df)
  
  nutr_per_spend <-
    panelists %>% 
    mutate(household_code = Household_Cd) %>% 
    dplyr::select(
      household_code,
      Household_Income,
      Household_Size,
      Male_Head_Education,
      Female_Head_Education,
      Panelist_ZipCd,
      Fips_State_Cd,
      Fips_County_Cd,
      zip = Panelist_ZipCd,
      inc_mid,
      Race,
      Marital_Status,
      Male_Head_Employment,
      Female_Head_Employment,
      Male_Head_Age,
      Female_Head_Age
    ) %>% 
    left_join(
      total_calories %>% dplyr::select(-total_spend)
    ) %>% 
    left_join(
      total_sugar %>% dplyr::select(-total_spend)
    ) %>% 
    left_join(
      total_saturated_fat %>% dplyr::select(-total_spend)
    )
  
  nutr_per_spend <-
    nutr_per_spend %>% 
    mutate(
      across(.cols = c(Race:Female_Head_Employment), .fns = as.factor)
    ) %>% 
    mutate(
      across(.cols = c(Male_Head_Education:Female_Head_Education), .fns = as.numeric)
    )
  
  #rm(df)
  
  return(nutr_per_spend)
  
}

