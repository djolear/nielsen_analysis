
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

# source(paste0(machine_path, "research/projects/niel/nielsen_analysis/bind_nielsen_to_label_insight_fn.R"))

sugar_per_cal_fn <- function(df){
  niel_df_sugar <-
    df %>% 
    filter(!is.na(sugar) & !is.na(calories))%>% 
    mutate(month = lubridate::month(purchase_date))
  
  total_sugar <-
    niel_df_sugar %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_sugar = sum(sugar, na.rm = TRUE)
    )
  
  calories <-
    niel_df_sugar %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_calories = sum(calories , na.rm = TRUE)
    )
  
  total_sugar <-
    total_sugar %>% 
    left_join(
      calories
    )
  
  total_sugar <-
    total_sugar %>% 
    mutate(
      sug_cal = total_sugar / total_calories
    )  
  
  return(total_sugar)
  
}

saturated_fat_per_cal_fn <- function(df){
  niel_df_saturated_fat <-
    df  %>% 
    filter(!is.na(sf) & !is.na(calories))%>% 
    mutate(month = lubridate::month(purchase_date))
  
  total_saturated_fat <-
    niel_df_saturated_fat %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_saturated_fat = sum(sf, na.rm = TRUE)
    )
  
  calories <-
    niel_df_saturated_fat %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_calories = sum(calories , na.rm = TRUE)
    )
  
  total_saturated_fat <-
    total_saturated_fat %>% 
    left_join(
      calories
    )
  
  total_saturated_fat <-
    total_saturated_fat %>% 
    mutate(
      sf_cal = total_saturated_fat / total_calories
    )  
  
  return(total_saturated_fat)
}


sodium_per_cal_fn <- function(df){
  niel_df_sodium <-
    df  %>% 
    filter(!is.na(sodium) & !is.na(calories))%>% 
    mutate(month = lubridate::month(purchase_date))
  
  total_sodium <-
    niel_df_sodium %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_sodium = sum(sodium, na.rm = TRUE)
    )
  
  calories <-
    niel_df_sodium %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_calories = sum(calories , na.rm = TRUE)
    )
  
  total_sodium <-
    total_sodium %>% 
    left_join(
      calories
    )
  
  total_sodium <-
    total_sodium %>% 
    mutate(
      sod_cal = total_sodium / total_calories
    )  
  
  return(total_sodium)
}


add_sugars_per_cal_fn <- function(df){
  niel_df_add_sugars <-
    df  %>% 
    filter(!is.na(as) & !is.na(calories)) %>% 
    mutate(month = lubridate::month(purchase_date))
  
  total_add_sugars <-
    niel_df_add_sugars %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_add_sugars = sum(as, na.rm = TRUE)
    )
  
  calories <-
    niel_df_add_sugars %>% 
    group_by(household_code, month) %>% 
    summarise(
      total_calories = sum(calories , na.rm = TRUE)
    )
  
  total_add_sugars <-
    total_add_sugars %>% 
    left_join(
      calories
    )
  
  total_add_sugars <-
    total_add_sugars %>% 
    mutate(
      add_sugar_cal = total_add_sugars / total_calories
    )  
  
  return(total_add_sugars)
}


produce_per_cal_fn <- function(df){
  niel_df_produce <-
    df %>%
    filter(!is.na(calories)) %>% 
    mutate(month = lubridate::month(purchase_date))

  niel_df_produce <-
    niel_df_produce %>%
    mutate(
      produce =
        case_when(
          # str_detect(product_group_descr, "VEGETABLE") == TRUE ~ "produce",
          # str_detect(product_group_descr, "FRUIT") == TRUE ~ "produce",
          str_detect(product_group_descr, "FRESH PRODUCE") == TRUE ~ "produce",
        )
    )

  total_produce <-
    niel_df_produce %>%
    filter(produce == "produce" & !is.na(calories)) %>%
    mutate(grams = 28.3495 * size1_amount) %>%
    group_by(household_code, month) %>%
    summarise(
      total_produce = sum(calories, na.rm = TRUE)
    )



  calories <-
    niel_df_produce %>%
    filter(!is.na(calories)) %>%
    group_by(household_code, month) %>%
    summarise(
      total_calories = sum(calories , na.rm = TRUE)
    )

  total_produce <-
    total_produce %>%
    left_join(
      calories
    )

  total_produce <-
    total_produce %>%
    mutate(
      produce_cal = total_produce / total_calories
    )

  return(total_produce)
}


###################
## Main Function ##
###################

nutrition_per_cal_fn <- function(year){
  
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
  
  
  # total_calories <- calories_per_cal_fn(df)
  # total_sugar <- sugar_per_cal_fn(niel_df)
  total_saturated_fat <- saturated_fat_per_cal_fn(niel_df)
  total_sodium <- sodium_per_cal_fn(niel_df)
  total_add_sugars <- add_sugars_per_cal_fn(niel_df)
  total_produce <- produce_per_cal_fn(niel_df)
  
  nutr_per_cal <-
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
    # left_join(
    #   total_calories %>% dplyr::select(-total_spend)
    # ) %>% 
    # left_join(
    #   total_sugar %>% dplyr::select(-total_calories)
    # ) %>%
    left_join(
      total_saturated_fat %>% dplyr::select(-total_calories)
    ) %>%
    left_join(
      total_sodium %>% dplyr::select(-total_calories)
    ) %>%
    left_join(
      total_add_sugars %>% dplyr::select(-total_calories)
    ) %>% 
    left_join(
      total_produce %>% dplyr::select(-total_calories)
    )
  
  nutr_per_cal <-
    nutr_per_cal %>% 
    mutate(
      hei_mod = 5 * produce_cal - 10 * add_sugar_cal - 10 * sf_cal - 10 * sod_cal
    )
  
  # nutr_per_cal <-
  #   nutr_per_cal %>% 
  #   mutate(
  #     across(.cols = c(Race:Female_Head_Employment), .fns = as.factor)
  #   ) %>% 
  #   mutate(
  #     across(.cols = c(Male_Head_Education:Female_Head_Education), .fns = as.numeric)
  #   )
  
  #rm(df)
  
  return(nutr_per_cal)
  
}

