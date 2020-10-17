# This file calculates spend per danielcat for nielsen data

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



###################
## Main Function ##
###################

calculate_spend_per_dc_fn <- function(year, products_master) {
  
  purchase <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/purchases_", year, ".tsv"))
  
  trips <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/trips_", year, ".tsv"))
  
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
  
  
  products <-
    readr::read_tsv(paste0("G:/Shared drives/SPL-Nielsen/Consumer_Panel_Data_2004_2017/Consumer_Panel_Data_2004_2017/nielsen_extracts/HMS/", year, "/Annual_Files/products_extra_", year, ".tsv"))
  
  
  
  trips <-
    trips %>% 
    mutate(
      month = lubridate::month(purchase_date),
      quarter = lubridate::quarter(purchase_date)
    ) 
  
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
          product_group_descr,
          product_module_descr,
          department_descr,
          dc,
          upc,
          upc_ver_uc,
        ),
      by = c("upc", "upc_ver_uc")
    )
  
  
  # Could look only at food stores
  
  # trips <- 
  #   trips %>% 
  #   left_join(
  #     retailers %>% 
  #       filter(
  #         channel_type %in% 
  #           c(
  #             "Grocery", 
  #             "Bakery", 
  #             "Beverage Store", 
  #             "Bodega", "Butcher", 
  #             "Candy Store", 
  #             "Cheese Stores", 
  #             "Coffee Store/Gourmet Coffee",
  #             "Convenience Store",
  #             "Dairy Store",
  #             "Delicatessen",
  #             "Dollar Store",
  #             "Drug Store",
  #             "Fish Market",
  #             "Fruit Stand",
  #             "as Mini Mart",
  #             "Health Food Store",
  #             "Pizzeria",
  #             "Quick Serve Restaurants",
  #             "Restaurant",
  #             "Vending Machine"
  #           )
  #       )
  #   )
  
  spend <-
    trips %>% 
    # filter(!is.na(channel_type)) %>% 
    group_by(household_code) %>% 
    summarise(
      total_spend = sum(total_spent, na.rm = TRUE)
    )
  
  spend_per_dc <-
    niel_df %>%
    group_by(
      household_code,
      dc
    ) %>%
    summarise(
      spend_per_dc = sum(total_price_paid)
    )
  
  
  spend_per_dc <-
    spend_per_dc %>% 
    left_join(
      spend,
      by = "household_code"
    )
  
  spend_per_dc <-
    spend_per_dc %>% 
    mutate(
      spend_per_dc_per = spend_per_dc / total_spend
    )
  
  spend_per_dc <-
    spend_per_dc %>% 
    left_join(
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
        )        
    )

  
  spend_per_dc <-
    spend_per_dc %>% 
    mutate(
      across(.cols = c(Race:Female_Head_Employment), .fns = as.factor)
    ) %>% 
    mutate(
      across(.cols = c(Male_Head_Education:Female_Head_Education), .fns = as.numeric)
    )
  
  spend_per_dc <-
    spend_per_dc %>% 
    ungroup %>% 
    mutate(
      state_fips = ifelse(str_length(Fips_State_Cd) == 1, paste0("0", Fips_State_Cd), Fips_State_Cd),
      county_fips = ifelse(str_length(Fips_County_Cd) == 2, paste0("0", Fips_County_Cd), Fips_County_Cd),
      fips_code = paste0(state_fips, county_fips)
    )
  
  #rm(niel_df, trips, panelists, purchase, trips)
  
  
  return(spend_per_dc)
  
}